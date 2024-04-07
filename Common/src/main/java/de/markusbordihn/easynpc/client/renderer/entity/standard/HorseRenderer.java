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

package de.markusbordihn.easynpc.client.renderer.entity.standard;

import de.markusbordihn.easynpc.client.model.standard.StandardHorseModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobRenderer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Horse;
import de.markusbordihn.easynpc.entity.easynpc.npc.Horse.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;

public class HorseRenderer
    extends BaseMobRenderer<Horse, Horse.Variant, StandardHorseModel<Horse>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.WHITE, new ResourceLocation("textures/entity/horse/horse_white.png"));
            map.put(
                Variant.WHITE_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_white.png"));
            map.put(Variant.CREAMY, new ResourceLocation("textures/entity/horse/horse_creamy.png"));
            map.put(
                Variant.CREAMY_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_creamy.png"));
            map.put(
                Variant.CHESTNUT, new ResourceLocation("textures/entity/horse/horse_chestnut.png"));
            map.put(
                Variant.CHESTNUT_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_chestnut.png"));
            map.put(Variant.BROWN, new ResourceLocation("textures/entity/horse/horse_brown.png"));
            map.put(
                Variant.BROWN_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_brown.png"));
            map.put(Variant.BLACK, new ResourceLocation("textures/entity/horse/horse_black.png"));
            map.put(
                Variant.BLACK_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_black.png"));
            map.put(Variant.GRAY, new ResourceLocation("textures/entity/horse/horse_gray.png"));
            map.put(
                Variant.GRAY_SADDLED, new ResourceLocation("textures/entity/horse/horse_gray.png"));
            map.put(
                Variant.DARKBROWN,
                new ResourceLocation("textures/entity/horse/horse_darkbrown.png"));
            map.put(
                Variant.DARKBROWN_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_darkbrown.png"));
            map.put(Variant.ZOMBIE, new ResourceLocation("textures/entity/horse/horse_zombie.png"));
            map.put(
                Variant.ZOMBIE_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_zombie.png"));
            map.put(
                Variant.SKELETON, new ResourceLocation("textures/entity/horse/horse_skeleton.png"));
            map.put(
                Variant.SKELETON_SADDLED,
                new ResourceLocation("textures/entity/horse/horse_skeleton.png"));
            map.put(Variant.DONKEY, new ResourceLocation("textures/entity/horse/donkey.png"));
            map.put(
                Variant.DONKEY_SADDLED, new ResourceLocation("textures/entity/horse/donkey.png"));
            map.put(Variant.MULE, new ResourceLocation("textures/entity/horse/mule.png"));
            map.put(Variant.MULE_SADDLED, new ResourceLocation("textures/entity/horse/mule.png"));
          });

  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.WHITE);

  public HorseRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new StandardHorseModel<>(context.bakeLayer(ModelLayers.HORSE)),
        1.1F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    // this.addLayer(new HeldItemLayer<>(this));
    // this.addLayer(new HorseMarkingLayer(this));
    // this.addLayer(new HorseArmorLayer(this, context.getModelSet()));
  }
}
