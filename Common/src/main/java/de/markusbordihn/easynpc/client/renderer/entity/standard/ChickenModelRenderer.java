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
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.client.model.standard.StandardChickenModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobModelRenderer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Chicken;
import de.markusbordihn.easynpc.entity.easynpc.npc.Chicken.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Pose;

public class ChickenModelRenderer
    extends BaseMobModelRenderer<Chicken, Variant, StandardChickenModel<Chicken>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> map.put(Variant.WHITE, new ResourceLocation("textures/entity/chicken.png")));
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.WHITE);

  public ChickenModelRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new StandardChickenModel<>(context.bakeLayer(ModelLayers.CHICKEN)),
        0.3F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
  }

  @Override
  public void renderDefaultPose(
      Chicken entity,
      StandardChickenModel<Chicken> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int light) {
    switch (entity.getPose()) {
      case DYING:
        poseStack.translate(-0.5D, 0.0D, 0.0D);
        poseStack.mulPose(Axis.YP.rotationDegrees(180f));
        poseStack.mulPose(Axis.ZP.rotationDegrees(90f));
        poseStack.mulPose(Axis.YP.rotationDegrees(180.0F));
        break;
      case SLEEPING:
        poseStack.translate(-0.5D, 0.5D, 0.0D);
        poseStack.mulPose(Axis.ZP.rotationDegrees(90f));
        break;
      default:
        break;
    }
  }

  @Override
  protected float getBob(Chicken entity, float delay) {
    float f = Mth.lerp(delay, entity.getOFlap(), entity.getFlap());
    float f1 = Mth.lerp(delay, entity.getOFlapSpeed(), entity.getFlapSpeed());
    return (Mth.sin(f) + 1.0F) * f1;
  }
}
