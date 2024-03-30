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
import de.markusbordihn.easynpc.client.model.standard.StandardCatModel;
import de.markusbordihn.easynpc.client.renderer.entity.StandardMobRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.CatCollarLayer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Cat;
import de.markusbordihn.easynpc.entity.easynpc.npc.Cat.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class CatRenderer extends StandardMobRenderer<Cat, Cat.Variant, StandardCatModel<Cat>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.ALL_BLACK, new ResourceLocation("textures/entity/cat/all_black.png"));
            map.put(Variant.BLACK, new ResourceLocation("textures/entity/cat/black.png"));
            map.put(
                Variant.BRITISH_SHORTHAIR,
                new ResourceLocation("textures/entity/cat/british_shorthair.png"));
            map.put(Variant.CALICO, new ResourceLocation("textures/entity/cat/calico.png"));
            map.put(Variant.JELLIE, new ResourceLocation("textures/entity/cat/jellie.png"));
            map.put(Variant.OCELOT, new ResourceLocation("textures/entity/cat/ocelot.png"));
            map.put(Variant.PERSIAN, new ResourceLocation("textures/entity/cat/persian.png"));
            map.put(Variant.RAGDOLL, new ResourceLocation("textures/entity/cat/ragdoll.png"));
            map.put(Variant.RED, new ResourceLocation("textures/entity/cat/red.png"));
            map.put(Variant.SIAMESE, new ResourceLocation("textures/entity/cat/siamese.png"));
            map.put(Variant.TABBY, new ResourceLocation("textures/entity/cat/tabby.png"));
            map.put(Variant.WHITE, new ResourceLocation("textures/entity/cat/white.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.BLACK);

  public CatRenderer(EntityRendererProvider.Context context) {
    super(
        context,
        new StandardCatModel<>(context.bakeLayer(ModelLayers.CAT)),
        0.4F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(new CatCollarLayer<>(this, context.getModelSet()));
  }

  @Override
  public void renderDefaultPose(
      Cat entity,
      StandardCatModel<Cat> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-0.5D, 0.0D, 0.0D);
        poseStack.mulPose(Axis.YP.rotationDegrees(180f));
        poseStack.mulPose(Axis.ZP.rotationDegrees(90f));
        poseStack.mulPose(Axis.YP.rotationDegrees(180.0F));
        this.getModel().getHead().xRot = -0.7853982F;
        this.getModel().getHead().yRot = -0.7853982F;
        this.getModel().getHead().zRot = -0.7853982F;
        break;
      case SLEEPING:
        poseStack.translate(-0.5D, 0.5D, 0.0D);
        poseStack.mulPose(Axis.ZP.rotationDegrees(90f));
        break;
      default:
        this.getModel().getHead().xRot = 0F;
        this.getModel().getHead().yRot = 0F;
        this.getModel().getHead().zRot = 0F;
        break;
    }
  }
}
