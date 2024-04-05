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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.model.standard.StandardIronGolemModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.VariantOverLayer;
import de.markusbordihn.easynpc.entity.easynpc.npc.IronGolem;
import de.markusbordihn.easynpc.entity.easynpc.npc.IronGolem.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class IronGolemRenderer
    extends BaseMobRenderer<IronGolem, Variant, StandardIronGolemModel<IronGolem>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            ResourceLocation resourceLocation =
                new ResourceLocation("textures/entity/iron_golem/iron_golem.png");
            map.put(Variant.IRON_GOLEM, resourceLocation);
            map.put(Variant.IRON_GOLEM_CRACKINESS_HIGH, resourceLocation);
            map.put(Variant.IRON_GOLEM_CRACKINESS_MEDIUM, resourceLocation);
            map.put(Variant.IRON_GOLEM_CRACKINESS_LOW, resourceLocation);
          });

  protected static final Map<Variant, ResourceLocation> TEXTURE_OVERLAY_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.IRON_GOLEM_CRACKINESS_HIGH,
                new ResourceLocation("textures/entity/iron_golem/iron_golem_crackiness_high.png"));
            map.put(
                Variant.IRON_GOLEM_CRACKINESS_MEDIUM,
                new ResourceLocation(
                    "textures/entity/iron_golem/iron_golem_crackiness_medium.png"));
            map.put(
                Variant.IRON_GOLEM_CRACKINESS_LOW,
                new ResourceLocation("textures/entity/iron_golem/iron_golem_crackiness_low.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT.get(Variant.IRON_GOLEM);

  public IronGolemRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new StandardIronGolemModel<>(context.bakeLayer(ModelLayers.IRON_GOLEM)),
        0.7F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT,
        TEXTURE_OVERLAY_BY_VARIANT);
    this.addLayer(new VariantOverLayer<>(this, context.getModelSet()));
    this.addLayer(new ItemInHandLayer<>(this));
  }

  @Override
  public void renderDefaultPose(
      IronGolem entity,
      StandardIronGolemModel<IronGolem> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int light) {}
}
