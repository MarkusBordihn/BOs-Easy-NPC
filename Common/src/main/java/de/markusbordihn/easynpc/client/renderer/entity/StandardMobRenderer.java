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

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.EasyNPCRenderer;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import java.util.Map;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.MobRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class StandardMobRenderer<T extends EasyNPCBaseEntity, V, M extends EntityModel<T>>
    extends MobRenderer<T, M> implements EasyNPCRenderer {

  protected final Map<V, ResourceLocation> textures;
  protected final Map<V, ResourceLocation> texturesOverlay;
  protected final ResourceLocation defaultTexture;

  public StandardMobRenderer(
      EntityRendererProvider.Context context,
      M model,
      float shadowRadius,
      ResourceLocation defaultTexture) {
    this(context, model, shadowRadius, defaultTexture, null, null);
  }

  public StandardMobRenderer(
      EntityRendererProvider.Context context,
      M model,
      float shadowRadius,
      ResourceLocation defaultTexture,
      Map<V, ResourceLocation> textures) {
    this(context, model, shadowRadius, defaultTexture, textures, null);
  }

  public StandardMobRenderer(
      EntityRendererProvider.Context context,
      M model,
      float shadowRadius,
      ResourceLocation defaultTexture,
      Map<V, ResourceLocation> textures,
      Map<V, ResourceLocation> texturesOverlay) {
    super(context, model, shadowRadius);
    this.defaultTexture = defaultTexture != null ? defaultTexture : Constants.BLANK_ENTITY_TEXTURE;
    this.textures = textures;
    this.texturesOverlay = texturesOverlay;
  }

  @Override
  public ResourceLocation getTextureLocation(T entity) {
    return this.getEntityTexture(entity);
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return defaultTexture;
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variant) {
    return textures == null
        ? Constants.BLANK_ENTITY_TEXTURE
        : textures.getOrDefault(variant, defaultTexture);
  }

  @Override
  public ResourceLocation getTextureOverlayByVariant(Enum<?> variant) {
    return texturesOverlay == null
        ? Constants.BLANK_ENTITY_TEXTURE
        : texturesOverlay.getOrDefault(variant, Constants.BLANK_ENTITY_TEXTURE);
  }

  @Override
  protected void scale(T entity, PoseStack poseStack, float unused) {
    this.scaleEntity(entity, poseStack);
  }

  @Override
  protected void renderNameTag(
      T entity,
      Component component,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int color) {
    this.renderEntityNameTag(entity, poseStack);
    super.renderNameTag(entity, component, poseStack, multiBufferSource, color);
  }

  @Override
  protected int getBlockLightLevel(T entity, BlockPos blockPos) {
    return getEntityLightLevel(entity, blockPos);
  }

  public void renderCustomPose(
      T entity,
      M model,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {}

  public void renderDefaultPose(
      T entity,
      M model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {}

  @Override
  public void render(
      T entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    M model = this.getModel();

    // Rotate entity, if needed
    this.rotateEntity(entity, poseStack);

    // Render custom or default pose.
    ModelPose modelPose = entity.getModelPose();
    if (modelPose == ModelPose.DEFAULT) {
      this.renderDefaultPose(
          entity, model, entity.getPose(), entityYaw, partialTicks, poseStack, buffer, packedLight);
    } else if (modelPose == ModelPose.CUSTOM) {
      this.renderCustomPose(entity, model, entityYaw, partialTicks, poseStack, buffer, packedLight);
    }

    // Render entity with original render method.
    super.render(entity, entityYaw, partialTicks, poseStack, buffer, packedLight);
  }
}
