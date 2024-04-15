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

package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.renderer.EasyNPCRenderer;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider.Context;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.layers.ArrowLayer;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.PathfinderMob;

public class CrashTestDummyRenderer<E extends PathfinderMob, M extends PlayerModel<E>>
    extends LivingEntityRenderer<E, M> implements EasyNPCRenderer<E, M> {

  public CrashTestDummyRenderer(Context context) {
    super(context, (M) new PlayerModel<E>(context.bakeLayer(ModelLayers.PLAYER), false), 0.5F);
    this.addLayer(new CustomHeadLayer<>(this, context.getModelSet()));
    this.addLayer(new ItemInHandLayer<>(this));
    this.addLayer(new ArrowLayer<>(context, this));
  }

  @Override
  public ResourceLocation getTextureLocation(E t) {
    return new ResourceLocation("textures/entity/steve.png");
  }

  @Override
  public void render(
      E entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    EasyNPCRenderer.renderEntity(entity, entityYaw, partialTicks, poseStack, buffer, packedLight);
    super.render(entity, entityYaw, partialTicks, poseStack, buffer, packedLight);
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variant) {
    // Use default texture.
    return null;
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    // Use default texture.
    return null;
  }
}
