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

package de.markusbordihn.easynpc.client.renderer.entity.base;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.EasyNPCModelRenderer;
import de.markusbordihn.easynpc.client.renderer.EasyNPCRenderer;
import de.markusbordihn.easynpc.entity.EasyNPCBaseModelEntity;
import java.util.Map;
import net.minecraft.client.model.EntityModel;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.MobRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public class BaseMobModelRenderer<E extends EasyNPCBaseModelEntity<E>, V, M extends EntityModel<E>>
    extends MobRenderer<E, M> implements EasyNPCModelRenderer<E, M> {

  protected final Map<V, ResourceLocation> textures;
  protected final Map<V, ResourceLocation> texturesOverlay;
  protected final ResourceLocation defaultTexture;

  public BaseMobModelRenderer(
      EntityRendererProvider.Context context,
      M model,
      float shadowRadius,
      ResourceLocation defaultTexture) {
    this(context, model, shadowRadius, defaultTexture, null, null);
  }

  public BaseMobModelRenderer(
      EntityRendererProvider.Context context,
      M model,
      float shadowRadius,
      ResourceLocation defaultTexture,
      Map<V, ResourceLocation> textures) {
    this(context, model, shadowRadius, defaultTexture, textures, null);
  }

  public BaseMobModelRenderer(
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
  public ResourceLocation getTextureLocation(E entity) {
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
  protected void scale(E entity, PoseStack poseStack, float unused) {
    EasyNPCModelRenderer.scaleEntity(entity, poseStack);
  }

  @Override
  protected void renderNameTag(
      E entity,
      Component component,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int color) {
    EasyNPCModelRenderer.renderEntityNameTag(entity, poseStack);
    super.renderNameTag(entity, component, poseStack, multiBufferSource, color);
  }

  @Override
  protected int getBlockLightLevel(E entity, BlockPos blockPos) {
    return EasyNPCModelRenderer.getEntityLightLevel(entity, blockPos);
  }

  @Override
  public void render(
      E entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int packedLight) {
    // Render model specific pose.
    this.renderModel(
        entity, this.getModel(), entityYaw, partialTicks, poseStack, buffer, packedLight);

    // Render entity with optional custom renderer or default renderer.
    if (!EasyNPCRenderer.renderEntity(
        entity, entityYaw, partialTicks, poseStack, buffer, packedLight)) {
      super.render(entity, entityYaw, partialTicks, poseStack, buffer, packedLight);
    }
  }
}
