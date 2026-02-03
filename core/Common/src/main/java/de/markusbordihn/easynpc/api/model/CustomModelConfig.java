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

package de.markusbordihn.easynpc.api.model;

import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.resources.ResourceLocation;

public record CustomModelConfig(
    ModelLayerLocation modelLayer, ResourceLocation customTexture, RenderMode renderMode) {

  public static final CustomModelConfig NONE = new CustomModelConfig(null, null, RenderMode.NONE);

  public static CustomModelConfig overlay(ModelLayerLocation modelLayer, ResourceLocation texture) {
    return new CustomModelConfig(modelLayer, texture, RenderMode.OVERLAY);
  }

  public static CustomModelConfig overlayUseEntityTexture(ModelLayerLocation modelLayer) {
    return new CustomModelConfig(modelLayer, null, RenderMode.OVERLAY_USE_ENTITY_TEXTURE);
  }

  public static CustomModelConfig overlayUseVariantTexture(ModelLayerLocation modelLayer) {
    return new CustomModelConfig(modelLayer, null, RenderMode.OVERLAY_USE_VARIANT_TEXTURE);
  }

  public static CustomModelConfig replacement(
      ModelLayerLocation modelLayer, ResourceLocation texture) {
    return new CustomModelConfig(modelLayer, texture, RenderMode.REPLACEMENT);
  }

  public static CustomModelConfig replacementUseEntityTexture(ModelLayerLocation modelLayer) {
    return new CustomModelConfig(modelLayer, null, RenderMode.REPLACEMENT_USE_ENTITY_TEXTURE);
  }

  public static CustomModelConfig replacementUseVariantTexture(ModelLayerLocation modelLayer) {
    return new CustomModelConfig(modelLayer, null, RenderMode.REPLACEMENT_USE_VARIANT_TEXTURE);
  }

  public boolean hasCustomModel() {
    return modelLayer != null;
  }

  public ModelLayerLocation getModelLayer() {
    return modelLayer;
  }

  public ResourceLocation getCustomTexture() {
    return customTexture;
  }

  public boolean shouldUseEntityTexture() {
    return renderMode == RenderMode.OVERLAY_USE_ENTITY_TEXTURE
        || renderMode == RenderMode.REPLACEMENT_USE_ENTITY_TEXTURE;
  }

  public boolean shouldUseVariantTexture() {
    return renderMode == RenderMode.OVERLAY_USE_VARIANT_TEXTURE
        || renderMode == RenderMode.REPLACEMENT_USE_VARIANT_TEXTURE;
  }

  public boolean shouldHideOriginal() {
    return renderMode == RenderMode.REPLACEMENT
        || renderMode == RenderMode.REPLACEMENT_USE_ENTITY_TEXTURE
        || renderMode == RenderMode.REPLACEMENT_USE_VARIANT_TEXTURE;
  }

  enum RenderMode {
    NONE,
    OVERLAY,
    OVERLAY_USE_ENTITY_TEXTURE,
    OVERLAY_USE_VARIANT_TEXTURE,
    REPLACEMENT,
    REPLACEMENT_USE_ENTITY_TEXTURE,
    REPLACEMENT_USE_VARIANT_TEXTURE
  }
}
