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
import de.markusbordihn.easynpc.data.skin.SkinVariantType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.renderer.entity.CatRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.animal.Cat;

public class CatRawRenderer extends CatRenderer implements EasyNPCEntityRenderer {

  protected static final Map<SkinVariantType.CAT, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(SkinVariantType.CAT.class),
          map -> {
            map.put(
                SkinVariantType.CAT.ALL_BLACK,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/all_black.png"));
            map.put(
                SkinVariantType.CAT.BLACK,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/black.png"));
            map.put(
                SkinVariantType.CAT.BRITISH_SHORTHAIR,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/cat/british_shorthair.png"));
            map.put(
                SkinVariantType.CAT.CALICO,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/calico.png"));
            map.put(
                SkinVariantType.CAT.JELLIE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/jellie.png"));
            map.put(
                SkinVariantType.CAT.OCELOT,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/ocelot.png"));
            map.put(
                SkinVariantType.CAT.PERSIAN,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/persian.png"));
            map.put(
                SkinVariantType.CAT.RAGDOLL,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/ragdoll.png"));
            map.put(
                SkinVariantType.CAT.RED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/red.png"));
            map.put(
                SkinVariantType.CAT.SIAMESE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/siamese.png"));
            map.put(
                SkinVariantType.CAT.TABBY,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/tabby.png"));
            map.put(
                SkinVariantType.CAT.WHITE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/cat/white.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(SkinVariantType.CAT.BLACK);

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
    return TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE);
  }
}
