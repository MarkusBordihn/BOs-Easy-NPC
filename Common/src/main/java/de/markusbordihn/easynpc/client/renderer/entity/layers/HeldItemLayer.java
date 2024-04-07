/*
 * Copyright 2023 Markus Bordihn
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
import com.mojang.math.Vector3f;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.ItemInHandRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.block.model.ItemTransforms.TransformType;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;

public class HeldItemLayer<E extends LivingEntity, M extends EntityModel<E> & HeadedModel>
    extends RenderLayer<E, M> {

  private final ItemInHandRenderer itemInHandRenderer;
  private double offsetX = 0.06;
  private double offsetY = 0.15;
  private double offsetZ = -0.52;

  public HeldItemLayer(
      RenderLayerParent<E, M> renderer,
      ItemInHandRenderer itemInHandRenderer,
      double offsetX, double offsetY, double offsetZ) {
    this(renderer, itemInHandRenderer);
    this.offsetX = offsetX;
    this.offsetY = offsetY;
    this.offsetZ = offsetZ;
  }

  public HeldItemLayer(RenderLayerParent<E, M> renderer, ItemInHandRenderer itemInHandRenderer) {
    super(renderer);
    this.itemInHandRenderer = itemInHandRenderer;
  }

  public void render(
      PoseStack poseStack,
      MultiBufferSource $$1,
      int $$2,
      E entity,
      float $$4,
      float $$5,
      float $$6,
      float $$7,
      float $$8,
      float $$9) {
    ModelPart head = this.getParentModel().getHead();
    poseStack.pushPose();
    poseStack.translate(head.x / 16.0F, head.y / 16.0F, head.z / 16.0F);
    poseStack.mulPose(Vector3f.ZP.rotation(0));
    poseStack.mulPose(Vector3f.YP.rotationDegrees($$8));
    poseStack.mulPose(Vector3f.XP.rotationDegrees($$9));
    poseStack.translate(offsetX, offsetY, offsetZ);
    poseStack.mulPose(Vector3f.XP.rotationDegrees(90.0F));
    ItemStack itemStack = entity.getItemBySlot(EquipmentSlot.MAINHAND);
    this.itemInHandRenderer
        .renderItem(entity, itemStack, TransformType.GROUND, false, poseStack, $$1, $$2);
    poseStack.popPose();
  }
}
