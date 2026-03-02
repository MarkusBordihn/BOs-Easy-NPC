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

package de.markusbordihn.easynpc.client.renderer.entity.raw;

import de.markusbordihn.easynpc.api.model.CustomModelConfig;
import de.markusbordihn.easynpc.api.model.OriginalModelConfig;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.EasyNPCItemAttachmentLayer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.GhastCustomModelLayer;
import de.markusbordihn.easynpc.data.skin.variant.GhastSkinVariant;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.GhastRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.monster.Ghast;

public class GhastRawRenderer extends GhastRenderer implements EasyNPCEntityRenderer {

  protected static final ResourceLocation DEFAULT_TEXTURE =
      GhastSkinVariant.GHAST.getTextureLocation();

  private final OriginalModelConfig originalConfig;
  private final CustomModelConfig customConfig;

  public GhastRawRenderer(EntityRendererProvider.Context context) {
    this(context, OriginalModelConfig.DEFAULT, CustomModelConfig.NONE);
  }

  public GhastRawRenderer(
      EntityRendererProvider.Context context, OriginalModelConfig originalConfig) {
    this(context, originalConfig, CustomModelConfig.NONE);
  }

  public GhastRawRenderer(EntityRendererProvider.Context context, CustomModelConfig customConfig) {
    this(context, OriginalModelConfig.DEFAULT, customConfig);
  }

  public GhastRawRenderer(
      EntityRendererProvider.Context context,
      OriginalModelConfig originalConfig,
      CustomModelConfig customConfig) {
    super(context);
    this.originalConfig = originalConfig != null ? originalConfig : OriginalModelConfig.DEFAULT;
    this.customConfig = customConfig != null ? customConfig : CustomModelConfig.NONE;

    if (this.customConfig.hasCustomModel()) {
      this.addLayer(new GhastCustomModelLayer(this, context.getModelSet(), this.customConfig));
    }
    this.addLayer(new EasyNPCItemAttachmentLayer<>(this, context.getItemInHandRenderer()));
  }

  @Override
  public OriginalModelConfig getOriginalModelConfig() {
    return originalConfig;
  }

  @Override
  public CustomModelConfig getCustomModelConfig() {
    return customConfig;
  }

  @Override
  public ResourceLocation getTextureLocation(Ghast entity) {
    return getTextureLocationWithConfig(entity);
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
