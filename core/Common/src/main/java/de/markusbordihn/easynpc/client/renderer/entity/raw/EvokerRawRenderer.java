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

import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.EasyNPCItemAttachmentLayer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.SkullHeadRenderLayer;
import de.markusbordihn.easynpc.data.skin.variant.IllagerSkinVariant;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.EvokerRenderer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.client.renderer.entity.state.EvokerRenderState;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.monster.illager.SpellcasterIllager;

public class EvokerRawRenderer<T extends SpellcasterIllager> extends EvokerRenderer<T>
    implements EasyNPCEntityRenderer {

  protected static final Identifier DEFAULT_TEXTURE =
      IllagerSkinVariant.EVOKER.getTextureLocation();

  public EvokerRawRenderer(EntityRendererProvider.Context context) {
    this(context, false);
  }

  public EvokerRawRenderer(
      EntityRendererProvider.Context context, boolean useVanillaItemInHandLayer) {
    super(context);
    this.addLayer(new SkullHeadRenderLayer<>(this));
    if (!useVanillaItemInHandLayer) {
      this.layers.removeIf(ItemInHandLayer.class::isInstance);
      this.addLayer(new EasyNPCItemAttachmentLayer<>(this));
    }
  }

  @Override
  public Identifier getTextureLocation(EvokerRenderState renderState) {
    return getTextureFromRenderStateWithConfig(renderState);
  }

  @Override
  public Identifier getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }
}
