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
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.PiglinRaw.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.PiglinRenderer;
import net.minecraft.client.renderer.entity.state.PiglinRenderState;
import net.minecraft.resources.ResourceLocation;

public class PiglinRawRenderer extends PiglinRenderer implements EasyNPCEntityRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.PIGLIN,
                ResourceLocation.withDefaultNamespace("textures/entity/piglin/piglin.png"));
            map.put(
                VariantType.PIGLIN_BRUTE,
                ResourceLocation.withDefaultNamespace("textures/entity/piglin/piglin_brute.png"));
            map.put(
                VariantType.ZOMBIFIED_PIGLIN,
                ResourceLocation.withDefaultNamespace(
                    "textures/entity/piglin/zombified_piglin.png"));
          });

  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.PIGLIN);

  public PiglinRawRenderer(EntityRendererProvider.Context context) {
    this(
        context,
        ModelLayers.PIGLIN,
        ModelLayers.PIGLIN_BABY,
        ModelLayers.PIGLIN_INNER_ARMOR,
        ModelLayers.PIGLIN_OUTER_ARMOR,
        ModelLayers.PIGLIN_BABY_INNER_ARMOR,
        ModelLayers.PIGLIN_BABY_OUTER_ARMOR);
  }

  public PiglinRawRenderer(
      EntityRendererProvider.Context context,
      ModelLayerLocation adultModelLocation,
      ModelLayerLocation babyModelLocation,
      ModelLayerLocation innerArmorModelLocation,
      ModelLayerLocation outerArmorModelLocation,
      ModelLayerLocation innerArmorModelBabyLocation,
      ModelLayerLocation outerArmorModelBabyLocation) {
    super(
        context,
        adultModelLocation,
        babyModelLocation,
        innerArmorModelLocation,
        outerArmorModelLocation,
        innerArmorModelBabyLocation,
        outerArmorModelBabyLocation);
  }

  @Override
  public ResourceLocation getTextureLocation(PiglinRenderState renderState) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC != null) {
      return getEntityTexture(easyNPC);
    }
    return renderState.isBrute
        ? TEXTURE_BY_VARIANT_TYPE.get(VariantType.PIGLIN_BRUTE)
        : TEXTURE_BY_VARIANT_TYPE.get(VariantType.PIGLIN);
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
