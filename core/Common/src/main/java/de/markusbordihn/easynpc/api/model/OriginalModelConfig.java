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

import net.minecraft.resources.ResourceLocation;

public record OriginalModelConfig(ResourceLocation customTexture, RenderMode renderMode) {

  public static final OriginalModelConfig DEFAULT =
      new OriginalModelConfig(null, RenderMode.DEFAULT);

  public static OriginalModelConfig withHidden() {
    return new OriginalModelConfig(null, RenderMode.HIDDEN);
  }

  public static OriginalModelConfig withTexture(ResourceLocation texture) {
    return new OriginalModelConfig(texture, RenderMode.CUSTOM_TEXTURE);
  }

  public static OriginalModelConfig withVariantTexture() {
    return new OriginalModelConfig(null, RenderMode.USE_VARIANT_TEXTURE);
  }

  public static OriginalModelConfig withEntityTexture() {
    return new OriginalModelConfig(null, RenderMode.USE_ENTITY_TEXTURE);
  }

  public boolean isHidden() {
    return renderMode == RenderMode.HIDDEN;
  }

  public boolean hasCustomTexture() {
    return customTexture != null && renderMode == RenderMode.CUSTOM_TEXTURE;
  }

  public boolean shouldUseVariantTexture() {
    return renderMode == RenderMode.USE_VARIANT_TEXTURE;
  }

  public boolean shouldUseEntityTexture() {
    return renderMode == RenderMode.USE_ENTITY_TEXTURE || renderMode == RenderMode.DEFAULT;
  }

  public ResourceLocation getCustomTexture() {
    return customTexture;
  }

  enum RenderMode {
    DEFAULT,
    HIDDEN,
    CUSTOM_TEXTURE,
    USE_VARIANT_TEXTURE,
    USE_ENTITY_TEXTURE
  }
}
