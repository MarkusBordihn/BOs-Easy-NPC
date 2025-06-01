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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.CatRaw.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.renderer.entity.CatRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.animal.Cat;

public class CatRawRenderer extends CatRenderer implements EasyNPCEntityRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.ALL_BLACK,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/all_black.png"));
            map.put(
                VariantType.BLACK,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/black.png"));
            map.put(
                VariantType.BRITISH_SHORTHAIR,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/british_shorthair.png"));
            map.put(
                VariantType.CALICO,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/calico.png"));
            map.put(
                VariantType.JELLIE,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/jellie.png"));
            map.put(
                VariantType.OCELOT,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/ocelot.png"));
            map.put(
                VariantType.PERSIAN,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/persian.png"));
            map.put(
                VariantType.RAGDOLL,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/ragdoll.png"));
            map.put(
                VariantType.RED,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/red.png"));
            map.put(
                VariantType.SIAMESE,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/siamese.png"));
            map.put(
                VariantType.TABBY,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/tabby.png"));
            map.put(
                VariantType.WHITE,
                ResourceLocation.withDefaultNamespace("textures/entity/cat/white.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.BLACK);

  public CatRawRenderer(EntityRendererProvider.Context context) {
    super(context);
  }

  @Override
  public ResourceLocation getTextureLocation(Cat entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      return getEntityTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variantType) {
    return TEXTURE_BY_VARIANT_TYPE != null
        ? TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE)
        : Constants.BLANK_ENTITY_TEXTURE;
  }
}
