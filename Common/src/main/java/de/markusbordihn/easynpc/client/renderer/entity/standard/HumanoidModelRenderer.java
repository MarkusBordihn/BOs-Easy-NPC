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
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.base.BasePlayerModel;
import de.markusbordihn.easynpc.client.renderer.EasyNPCModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.base.BaseLivingEntityModelRenderer;
import de.markusbordihn.easynpc.entity.easynpc.npc.Humanoid;
import de.markusbordihn.easynpc.entity.easynpc.npc.Humanoid.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ElytraLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.client.renderer.entity.layers.RenderLayer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class HumanoidModelRenderer
    extends BaseLivingEntityModelRenderer<Humanoid, Variant, BasePlayerModel<Humanoid>> {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(Variant.STEVE, new ResourceLocation("textures/entity/steve.png"));
            map.put(
                Variant.JAYJASONBO,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/jayjasonbo.png"));
            map.put(
                Variant.PROFESSOR_01,
                new ResourceLocation(
                    Constants.MOD_ID, "textures/entity/humanoid/professor_01.png"));
            map.put(
                Variant.SECURITY_01,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/security_01.png"));
            map.put(
                Variant.KNIGHT_01,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/knight_01.png"));
            map.put(
                Variant.KNIGHT_02,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/humanoid/knight_02.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.STEVE);

  public <L extends RenderLayer<Humanoid, BasePlayerModel<Humanoid>>> HumanoidModelRenderer(
      EntityRendererProvider.Context context, Class<L> humanoidArmorLayerClass) {
    super(
        context,
        new BasePlayerModel<>(context.bakeLayer(ModelLayers.PLAYER), false),
        0.5F,
        DEFAULT_TEXTURE,
        TEXTURE_BY_VARIANT);
    this.addLayer(
        EasyNPCModelRenderer.getHumanoidArmorLayer(
            this,
            context,
            ModelLayers.PLAYER_INNER_ARMOR,
            ModelLayers.PLAYER_OUTER_ARMOR,
            humanoidArmorLayerClass));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
    this.addLayer(new ElytraLayer<>(this, context.getModelSet()));
  }

  @Override
  public ResourceLocation getTextureLocation(Humanoid entity) {
    return this.getEntityPlayerTexture(entity);
  }

  @Override
  public void renderDefaultPose(
      Humanoid entity,
      BasePlayerModel<Humanoid> model,
      Pose pose,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      MultiBufferSource buffer,
      int light) {
    switch (pose) {
      case DYING:
        poseStack.translate(-1.0D, 0.0D, 0.0D);
        poseStack.mulPose(Vector3f.YP.rotationDegrees(180f));
        poseStack.mulPose(Vector3f.ZP.rotationDegrees(this.getFlipDegrees(entity)));
        poseStack.mulPose(Vector3f.YP.rotationDegrees(270.0f));
        model.getHead().xRot = -0.7853982F;
        model.getHead().yRot = -0.7853982F;
        model.getHead().zRot = -0.7853982F;
        break;
      case LONG_JUMPING:
        model.leftArmPose = HumanoidModel.ArmPose.CROSSBOW_HOLD;
        model.rightArmPose = HumanoidModel.ArmPose.SPYGLASS;
        break;
      case SLEEPING:
        poseStack.translate(1.0D, 0.0D, 0.0D);
        break;
      case SPIN_ATTACK:
        model.leftArmPose = HumanoidModel.ArmPose.BLOCK;
        model.rightArmPose = HumanoidModel.ArmPose.THROW_SPEAR;
        poseStack.mulPose(Vector3f.YP.rotationDegrees(-35f));
        break;
      default:
        model.leftArmPose = HumanoidModel.ArmPose.EMPTY;
        model.rightArmPose = HumanoidModel.ArmPose.EMPTY;
        model.getHead().xRot = 0F;
        model.getHead().yRot = 0F;
        model.getHead().zRot = 0F;
        break;
    }
  }
}
