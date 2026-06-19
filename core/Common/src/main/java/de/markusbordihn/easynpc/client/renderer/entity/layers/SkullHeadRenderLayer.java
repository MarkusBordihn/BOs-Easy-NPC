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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.object.skull.SkullModelBase;
import net.minecraft.client.renderer.SubmitNodeCollector;
import net.minecraft.client.renderer.blockentity.SkullBlockRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.rendertype.RenderType;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.AbstractSkullBlock;
import net.minecraft.world.level.block.SkullBlock;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SkullHeadRenderLayer<
        S extends LivingEntityRenderState, M extends EntityModel<? super S>>
    extends RenderLayer<S, M> {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Set<Item> FAILED_HEAD_ITEMS = ConcurrentHashMap.newKeySet();

  private Map<SkullBlock.Type, SkullModelBase> skullModelCache;
  private boolean renderingDisabled;

  public SkullHeadRenderLayer(RenderLayerParent<S, M> renderer) {
    super(renderer);
  }

  @Override
  public void submit(
      PoseStack poseStack,
      SubmitNodeCollector submitNodeCollector,
      int packedLight,
      S renderState,
      float limbSwing,
      float limbSwingAmount) {

    if (this.renderingDisabled) {
      return;
    }

    if (!(renderState instanceof EasyNPCRenderStateExtension renderStateExtension)) {
      return;
    }

    UUID entityUUID = renderStateExtension.getEasyNpcUUID();
    if (entityUUID == null) {
      return;
    }

    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(entityUUID);
    if (easyNPC == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null || !modelData.getModelPartVisibility(EquipmentSlot.HEAD)) {
      return;
    }

    LivingEntity entity = (LivingEntity) easyNPC.getEntity();
    if (entity == null) {
      return;
    }

    ItemStack headItem = entity.getItemBySlot(EquipmentSlot.HEAD);
    if (headItem.isEmpty()
        || !(headItem.getItem() instanceof BlockItem blockItem)
        || !(blockItem.getBlock() instanceof AbstractSkullBlock skullBlock)) {
      return;
    }

    if (FAILED_HEAD_ITEMS.contains(blockItem)) {
      return;
    }

    if (!(this.getParentModel() instanceof HumanoidModel<?> humanoidParentModel)) {
      return;
    }

    SkullBlock.Type skullType = skullBlock.getType();
    SkullModelBase skullModel = getOrCreateSkullModel(skullType);
    if (skullModel == null) {
      return;
    }

    RenderType renderType = SkullBlockRenderer.getSkullRenderType(skullType, null);

    poseStack.pushPose();
    try {
      humanoidParentModel.head.translateAndRotate(poseStack);
      poseStack.scale(1.0625F, -1.0625F, -1.0625F);
      poseStack.translate(-0.5, 0.0, -0.5);
      SkullBlockRenderer.submitSkull(
          180.0F,
          poseStack,
          submitNodeCollector,
          packedLight,
          skullModel,
          renderType,
          OverlayTexture.NO_OVERLAY,
          null);
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

  private SkullModelBase getOrCreateSkullModel(SkullBlock.Type type) {
    if (this.skullModelCache == null) {
      this.skullModelCache = new HashMap<>();
    }
    return this.skullModelCache.computeIfAbsent(
        type, t -> SkullBlockRenderer.createModel(Minecraft.getInstance().getEntityModels(), t));
  }
}
