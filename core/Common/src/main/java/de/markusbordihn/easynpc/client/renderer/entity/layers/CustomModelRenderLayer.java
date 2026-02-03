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
import com.mojang.blaze3d.vertex.VertexConsumer;
import de.markusbordihn.easynpc.api.model.CustomModelConfig;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.entity.RenderLayerParent;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;

public abstract class CustomModelRenderLayer<T extends LivingEntity, M extends EntityModel<T>>
    extends RenderLayer<T, M> {

  protected final M customModel;
  protected final CustomModelConfig config;
  protected final RenderLayerParent<T, M> renderer;

  protected CustomModelRenderLayer(
      RenderLayerParent<T, M> renderer, M customModel, CustomModelConfig config) {
    super(renderer);
    this.renderer = renderer;
    this.customModel = customModel;
    this.config = config;
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

    M parentModel = this.getParentModel();
    parentModel.copyPropertiesTo(this.customModel);

    this.customModel.prepareMobModel(entity, limbSwing, limbSwingAmount, partialTick);
    this.customModel.setupAnim(
        entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);

    ResourceLocation texture = getTextureLocation(entity);
    VertexConsumer vertexConsumer = buffer.getBuffer(RenderType.entityCutoutNoCull(texture));
    this.customModel.renderToBuffer(
        poseStack, vertexConsumer, packedLight, OverlayTexture.NO_OVERLAY);
  }

  @Override
  protected ResourceLocation getTextureLocation(T entity) {
    if (config.shouldUseEntityTexture()
        && entity instanceof EasyNPC<?> easyNPC
        && this.renderer instanceof EasyNPCEntityRenderer easyNPCRenderer) {
      return easyNPCRenderer.getEntityTexture(easyNPC);
    } else if (config.shouldUseVariantTexture()
        && entity instanceof EasyNPC<?> easyNPC
        && this.renderer instanceof EasyNPCEntityRenderer easyNPCRenderer) {
      return easyNPCRenderer.getVariantTexture(easyNPC);
    }
    return config.getCustomTexture();
  }
}
