/*
 * Copyright 2025 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.renderer.entity.layers;

import com.mojang.authlib.GameProfile;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.SkullModelBase;
import net.minecraft.client.renderer.ItemInHandRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.SkullBlockRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ArmorItem;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.AbstractSkullBlock;
import net.minecraft.world.level.block.SkullBlock;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SkullHeadRenderLayer<T extends LivingEntity, M extends EntityModel<T>>
    extends RenderLayer<T, M> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Set<Item> FAILED_HEAD_ITEMS = ConcurrentHashMap.newKeySet();

  private final ItemInHandRenderer itemInHandRenderer;
  private Map<SkullBlock.Type, SkullModelBase> skullModelCache;
  private boolean renderingDisabled;

  public SkullHeadRenderLayer(
      RenderLayerParent<T, M> renderer, ItemInHandRenderer itemInHandRenderer) {
    super(renderer);
    this.itemInHandRenderer = itemInHandRenderer;
  }

  private static GameProfile extractGameProfile(ItemStack itemStack) {
    CompoundTag tag = itemStack.getTag();
    if (tag != null && tag.contains("SkullOwner", 10)) {
      return NbtUtils.readGameProfile(tag.getCompound("SkullOwner"));
    }
    return null;
  }

  @Override
  public void render(
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight,
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float partialTick,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {

    if (this.renderingDisabled || !(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null || !modelData.getModelPartVisibility(EquipmentSlot.HEAD)) {
      return;
    }

    ItemStack headItem = entity.getItemBySlot(EquipmentSlot.HEAD);
    if (headItem.isEmpty()) {
      return;
    }

    if (!(this.getParentModel() instanceof HumanoidModel<?> humanoidParentModel)) {
      return;
    }

    Item item = headItem.getItem();
    if (item instanceof ArmorItem armorItem
        && armorItem.getEquipmentSlot() == EquipmentSlot.HEAD) {
      return;
    }

    if (!(item instanceof BlockItem blockItem)
        || !(blockItem.getBlock() instanceof AbstractSkullBlock skullBlock)) {
      renderHeadItem(
          entity, headItem, humanoidParentModel, poseStack, buffer, packedLight);
      return;
    }

    SkullBlock.Type skullType = skullBlock.getType();
    SkullModelBase skullModel = getOrCreateSkullModel(skullType);
    if (skullModel == null) {
      return;
    }

    if (FAILED_HEAD_ITEMS.contains(blockItem)) {
      return;
    }

    RenderType renderType =
        SkullBlockRenderer.getRenderType(skullType, extractGameProfile(headItem));

    poseStack.pushPose();
    try {
      humanoidParentModel.head.translateAndRotate(poseStack);
      poseStack.scale(1.0625F, -1.0625F, -1.0625F);
      poseStack.translate(-0.5, 0.0, -0.5);
      SkullBlockRenderer.renderSkull(
          null, 180.0F, 0.0F, poseStack, buffer, packedLight, skullModel, renderType);
    } catch (LinkageError error) {
      this.renderingDisabled = true;
      log.error(
          "Disabling Easy NPC skull rendering for entity {} ({}) with model type {}, model {}, skull type {} after linkage error.",
          entity.getType(),
          entity.getUUID(),
          modelData.getModelType(),
          this.getParentModel().getClass().getName(),
          skullType,
          error);
    } catch (RuntimeException exception) {
      FAILED_HEAD_ITEMS.add(blockItem);
      log.error(
          "Skipping Easy NPC skull rendering for item {} on entity {} ({}) with model type {}, model {}, skull type {} after render error.",
          blockItem,
          entity.getType(),
          entity.getUUID(),
          modelData.getModelType(),
          this.getParentModel().getClass().getName(),
          skullType,
          exception);
    } finally {
      poseStack.popPose();
    }
  }

  private void renderHeadItem(
      T entity,
      ItemStack headItem,
      HumanoidModel<?> humanoidParentModel,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    Item item = headItem.getItem();
    if (FAILED_HEAD_ITEMS.contains(item)) {
      return;
    }

    poseStack.pushPose();
    try {
      humanoidParentModel.head.translateAndRotate(poseStack);
      CustomHeadLayer.translateToHead(poseStack, false);
      this.itemInHandRenderer.renderItem(
          entity, headItem, ItemDisplayContext.HEAD, false, poseStack, buffer, packedLight);
    } catch (LinkageError error) {
      this.renderingDisabled = true;
      log.error(
          "Disabling Easy NPC head item rendering for entity {} ({}) with item {} after linkage error.",
          entity.getType(),
          entity.getUUID(),
          item,
          error);
    } catch (RuntimeException exception) {
      FAILED_HEAD_ITEMS.add(item);
      log.error(
          "Skipping Easy NPC head item {} on entity {} ({}) after render error.",
          item,
          entity.getType(),
          entity.getUUID(),
          exception);
    } finally {
      poseStack.popPose();
    }
  }

  private SkullModelBase getOrCreateSkullModel(SkullBlock.Type type) {
    if (skullModelCache == null) {
      skullModelCache =
          SkullBlockRenderer.createSkullRenderers(Minecraft.getInstance().getEntityModels());
    }
    return skullModelCache.get(type);
  }
}
