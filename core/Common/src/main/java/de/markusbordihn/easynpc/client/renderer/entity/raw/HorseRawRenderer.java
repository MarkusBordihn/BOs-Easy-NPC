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
import net.minecraft.client.model.HorseModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.AbstractHorseRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.HorseArmorLayer;
import net.minecraft.client.renderer.entity.layers.HorseMarkingLayer;
import net.minecraft.client.renderer.entity.state.HorseRenderState;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.animal.horse.Horse;

public class HorseRawRenderer extends AbstractHorseRenderer<Horse, HorseRenderState, HorseModel>
    implements EasyNPCEntityRenderer {

  protected static final Map<SkinVariantType.HORSE, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(SkinVariantType.HORSE.class),
          map -> {
            map.put(
                SkinVariantType.HORSE.WHITE,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_white.png"));
            map.put(
                SkinVariantType.HORSE.WHITE_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_white.png"));
            map.put(
                SkinVariantType.HORSE.CREAMY,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_creamy.png"));
            map.put(
                SkinVariantType.HORSE.CREAMY_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_creamy.png"));
            map.put(
                SkinVariantType.HORSE.CHESTNUT,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_chestnut.png"));
            map.put(
                SkinVariantType.HORSE.CHESTNUT_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_chestnut.png"));
            map.put(
                SkinVariantType.HORSE.BROWN,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_brown.png"));
            map.put(
                SkinVariantType.HORSE.BROWN_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_brown.png"));
            map.put(
                SkinVariantType.HORSE.BLACK,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_black.png"));
            map.put(
                SkinVariantType.HORSE.BLACK_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_black.png"));
            map.put(
                SkinVariantType.HORSE.GRAY,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_gray.png"));
            map.put(
                SkinVariantType.HORSE.GRAY_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_gray.png"));
            map.put(
                SkinVariantType.HORSE.DARKBROWN,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_darkbrown.png"));
            map.put(
                SkinVariantType.HORSE.DARKBROWN_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_darkbrown.png"));
            map.put(
                SkinVariantType.HORSE.ZOMBIE,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_zombie.png"));
            map.put(
                SkinVariantType.HORSE.ZOMBIE_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/horse_zombie.png"));
            map.put(
                SkinVariantType.HORSE.SKELETON,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_skeleton.png"));
            map.put(
                SkinVariantType.HORSE.SKELETON_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE,
                    "textures/entity/horse/horse_skeleton.png"));
            map.put(
                SkinVariantType.HORSE.DONKEY,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/donkey.png"));
            map.put(
                SkinVariantType.HORSE.DONKEY_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/donkey.png"));
            map.put(
                SkinVariantType.HORSE.MULE,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/mule.png"));
            map.put(
                SkinVariantType.HORSE.MULE_SADDLED,
                ResourceLocation.fromNamespaceAndPath(
                    ResourceLocation.DEFAULT_NAMESPACE, "textures/entity/horse/mule.png"));
          });

  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(SkinVariantType.HORSE.WHITE);

  public HorseRawRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new HorseModel(context.bakeLayer(ModelLayers.HORSE)),
        new HorseModel(context.bakeLayer(ModelLayers.HORSE_BABY)));
    this.addLayer(new HorseMarkingLayer(this));
    this.addLayer(new HorseArmorLayer(this, context.getModelSet(), context.getEquipmentRenderer()));
  }

  public ResourceLocation getTextureLocation(HorseRenderState renderState) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC != null) {
      return getEntityTexture(easyNPC);
    }
    return DEFAULT_TEXTURE;
  }

  public HorseRenderState createRenderState() {
    return new HorseRenderState();
  }

  @Override
  public void extractRenderState(Horse horse, HorseRenderState state, float partialTicks) {
    super.extractRenderState(horse, state, partialTicks);
    state.variant = horse.getVariant();
    state.markings = horse.getMarkings();
    state.bodyArmorItem = horse.getBodyArmorItem().copy();
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
