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
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.client.model.standard.StandardIllagerModel;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseMobModelRenderer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Illager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Illager.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class IllagerModelRenderer
    extends BaseMobModelRenderer<Illager, Variant, StandardIllagerModel<Illager>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.EVOKER, new ResourceLocation("textures/entity/illager/evoker.png"));
            map.put(
                Variant.EVOKER_CROSSED_ARMS,
                new ResourceLocation("textures/entity/illager/evoker.png"));
            map.put(
                Variant.ILLUSIONER, new ResourceLocation("textures/entity/illager/illusioner.png"));
            map.put(
                Variant.ILLUSIONER_CROSSED_ARMS,
                new ResourceLocation("textures/entity/illager/illusioner.png"));
            map.put(Variant.PILLAGER, new ResourceLocation("textures/entity/illager/pillager.png"));
            map.put(
                Variant.VINDICATOR, new ResourceLocation("textures/entity/illager/vindicator.png"));
            map.put(
                Variant.VINDICATOR_CROSSED_ARMS,
                new ResourceLocation("textures/entity/illager/vindicator.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT.get(Variant.PILLAGER);

  public <L extends RenderLayer<Illager, StandardIllagerModel<Illager>>> IllagerModelRenderer(
      EntityRendererProvider.Context context) {
    super(
        context,
        new StandardIllagerModel<>(context.bakeLayer(ModelLayers.PILLAGER)),
        0.5F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(new CustomHeadLayer<>(this, context.getModelSet()));
    this.addLayer(new ItemInHandLayer<>(this));
  }

  @Override
  public void renderDefaultPose(
      Illager entity,
      StandardIllagerModel<Illager> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-1.0D, 0.0D, 0.0D);
        poseStack.mulPose(Vector3f.YP.rotationDegrees(180f));
        poseStack.mulPose(Vector3f.ZP.rotationDegrees(this.getFlipDegrees(entity)));
        poseStack.mulPose(Vector3f.YP.rotationDegrees(270.0F));
        model.getHead().xRot = -0.7853982F;
        model.getHead().yRot = -0.7853982F;
        model.getHead().zRot = -0.7853982F;
        break;
      case SLEEPING:
        poseStack.translate(1.0D, 0.0D, 0.0D);
        break;
      default:
        model.getHead().xRot = 0F;
        model.getHead().yRot = 0F;
        model.getHead().zRot = 0F;
        break;
    }
  }
}
