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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.HorseRaw.VariantType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.HorseModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.AbstractHorseRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.HorseArmorLayer;
import net.minecraft.client.renderer.entity.layers.HorseMarkingLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.animal.horse.Horse;

public class HorseRawRenderer extends AbstractHorseRenderer<Horse, HorseModel<Horse>>
    implements EasyNPCEntityRenderer {

  protected static final Map<VariantType, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(VariantType.class),
          map -> {
            map.put(
                VariantType.WHITE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_white.png"));
            map.put(
                VariantType.WHITE_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_white.png"));
            map.put(
                VariantType.CREAMY,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_creamy.png"));
            map.put(
                VariantType.CREAMY_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_creamy.png"));
            map.put(
                VariantType.CHESTNUT,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_chestnut.png"));
            map.put(
                VariantType.CHESTNUT_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_chestnut.png"));
            map.put(
                VariantType.BROWN,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_brown.png"));
            map.put(
                VariantType.BROWN_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_brown.png"));
            map.put(
                VariantType.BLACK,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_black.png"));
            map.put(
                VariantType.BLACK_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_black.png"));
            map.put(
                VariantType.GRAY,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_gray.png"));
            map.put(
                VariantType.GRAY_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_gray.png"));
            map.put(
                VariantType.DARKBROWN,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_darkbrown.png"));
            map.put(
                VariantType.DARKBROWN_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_darkbrown.png"));
            map.put(
                VariantType.ZOMBIE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_zombie.png"));
            map.put(
                VariantType.ZOMBIE_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_zombie.png"));
            map.put(
                VariantType.SKELETON,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_skeleton.png"));
            map.put(
                VariantType.SKELETON_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_skeleton.png"));
            map.put(
                VariantType.DONKEY,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/donkey.png"));
            map.put(
                VariantType.DONKEY_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/donkey.png"));
            map.put(
                VariantType.MULE,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/mule.png"));
            map.put(
                VariantType.MULE_SADDLED,
                new ResourceLocation(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/mule.png"));
          });

  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(VariantType.WHITE);

  public HorseRawRenderer(EntityRendererProvider.Context context) {
    super(context, new HorseModel<>(context.bakeLayer(ModelLayers.HORSE)), 1.1F);
    this.addLayer(new HorseMarkingLayer(this));
    this.addLayer(new HorseArmorLayer(this, context.getModelSet()));
  }

  @Override
  public ResourceLocation getTextureLocation(Horse entity) {
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
