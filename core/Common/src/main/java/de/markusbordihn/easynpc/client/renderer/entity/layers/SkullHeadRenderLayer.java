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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.Map;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.SkullModelBase;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.blockentity.SkullBlockRenderer;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.core.component.DataComponents;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.ResolvableProfile;
import net.minecraft.world.level.block.AbstractSkullBlock;
import net.minecraft.world.level.block.SkullBlock;

public class SkullHeadRenderLayer<T extends LivingEntity, M extends EntityModel<T>>
    extends RenderLayer<T, M> {

  private Map<SkullBlock.Type, SkullModelBase> skullModelCache;

  public SkullHeadRenderLayer(RenderLayerParent<T, M> renderer) {
    super(renderer);
  }

  private static ResolvableProfile extractProfile(ItemStack itemStack) {
    return itemStack.get(DataComponents.PROFILE);
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

    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null || !modelData.getModelPartVisibility(EquipmentSlot.HEAD)) {
      return;
    }

    ItemStack headItem = entity.getItemBySlot(EquipmentSlot.HEAD);
    if (headItem.isEmpty()
        || !(headItem.getItem() instanceof BlockItem blockItem)
        || !(blockItem.getBlock() instanceof AbstractSkullBlock skullBlock)) {
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

    RenderType renderType = SkullBlockRenderer.getRenderType(skullType, extractProfile(headItem));

    poseStack.pushPose();
    humanoidParentModel.head.translateAndRotate(poseStack);
    poseStack.scale(1.0625F, -1.0625F, -1.0625F);
    poseStack.translate(-0.5, 0.0, -0.5);
    SkullBlockRenderer.renderSkull(
        null, 180.0F, 0.0F, poseStack, buffer, packedLight, skullModel, renderType);
    poseStack.popPose();
  }

  private SkullModelBase getOrCreateSkullModel(SkullBlock.Type type) {
    if (skullModelCache == null) {
      skullModelCache =
          SkullBlockRenderer.createSkullRenderers(Minecraft.getInstance().getEntityModels());
    }
    return skullModelCache.get(type);
  }
}
