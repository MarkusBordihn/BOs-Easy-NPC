/**
 * Copyright 2023 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.client.renderer.entity.standard;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.standard.StandardAllayModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobRenderer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Allay;
import de.markusbordihn.easynpc.entity.easynpc.npc.Allay.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class AllayRenderer extends BaseMobRenderer<Allay, Variant, StandardAllayModel<Allay>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.LAVA,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/allay/allay_lava.png"));
            map.put(
                Variant.GRASSLAND,
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/allay/allay_grassland.png"));
            map.put(
                Variant.WATER,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/allay/allay_water.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.WATER);

  public AllayRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(
        context,
        new StandardAllayModel<>(context.bakeLayer(modelLayerLocation)),
        0.4F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(new ItemInHandLayer<>(this));
  }

  @Override
  public void renderDefaultPose(
      Allay entity,
      StandardAllayModel<Allay> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-0.25D, 0.0D, 0.0D);
        poseStack.mulPose(Vector3f.YP.rotationDegrees(180f));
        poseStack.mulPose(Vector3f.ZP.rotationDegrees(this.getFlipDegrees(entity)));
        poseStack.mulPose(Vector3f.YP.rotationDegrees(270.0F));
        model.getHead().xRot = -0.7853982F;
        model.getHead().yRot = -0.7853982F;
        model.getHead().zRot = -0.7853982F;
        break;
      case SLEEPING:
        poseStack.translate(0.25D, 0.0D, 0.0D);
        break;
      case SPIN_ATTACK:
        poseStack.mulPose(Vector3f.YP.rotationDegrees(-35f));
        break;
      default:
        model.getHead().xRot = 0F;
        model.getHead().yRot = 0F;
        model.getHead().zRot = 0F;
        break;
    }
  }
}
