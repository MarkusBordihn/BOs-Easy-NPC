/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.FairyModel;
import de.markusbordihn.easynpc.client.renderer.EasyNPCRenderer;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.npc.Fairy;
import de.markusbordihn.easynpc.entity.easynpc.npc.Fairy.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public class FairyRenderer extends HumanoidMobRenderer<Fairy, FairyModel<Fairy>>
    implements EasyNPCRenderer {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.BLUE,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_blue.png"));
            map.put(
                Variant.GREEN,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_green.png"));
            map.put(
                Variant.RED,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/fairy/fairy_red.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.GREEN);

  public FairyRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new FairyModel<>(context.bakeLayer(modelLayerLocation)), 0.3F);
  }

  @Override
  public ResourceLocation getTextureByVariant(Enum<?> variant) {
    return TEXTURE_BY_VARIANT.getOrDefault(variant, DEFAULT_TEXTURE);
  }

  @Override
  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  @Override
  public ResourceLocation getTextureLocation(Fairy entity) {
    return this.getEntityTexture(entity);
  }

  @Override
  protected void scale(Fairy entity, PoseStack poseStack, float unused) {
    this.scaleEntity(entity, poseStack);
  }

  @Override
  public void render(
      Fairy entity,
      float entityYaw,
      float partialTicks,
      PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer,
      int light) {
    FairyModel<Fairy> playerModel = this.getModel();

    // Model Rotation
    this.rotateEntityAlternative(entity, poseStack);

    // Render additional poses
    if (entity.getModelPose() == ModelPose.DEFAULT) {

      // Crouching
      playerModel.crouching = entity.isCrouching();

      switch (entity.getPose()) {
        case DYING:
          poseStack.translate(-0.5D, 0.0D, 0.0D);
          poseStack.mulPose(Axis.YP.rotationDegrees(180f));
          poseStack.mulPose(Axis.ZP.rotationDegrees(this.getFlipDegrees(entity)));
          poseStack.mulPose(Axis.YP.rotationDegrees(270.0F));
          playerModel.getHead().xRot = -0.7853982F;
          playerModel.getHead().yRot = -0.7853982F;
          playerModel.getHead().zRot = -0.7853982F;
          break;
        case LONG_JUMPING:
          playerModel.leftArmPose = HumanoidModel.ArmPose.CROSSBOW_HOLD;
          playerModel.rightArmPose = HumanoidModel.ArmPose.SPYGLASS;
          break;
        case SLEEPING:
          poseStack.translate(0.5D, 0.0D, 0.0D);
          break;
        case SPIN_ATTACK:
          playerModel.leftArmPose = HumanoidModel.ArmPose.BLOCK;
          playerModel.rightArmPose = HumanoidModel.ArmPose.THROW_SPEAR;
          poseStack.mulPose(Axis.YP.rotationDegrees(-35f));
          break;
        default:
          playerModel.leftArmPose = HumanoidModel.ArmPose.EMPTY;
          playerModel.rightArmPose = HumanoidModel.ArmPose.EMPTY;
          playerModel.getHead().xRot = 0F;
          playerModel.getHead().yRot = 0F;
          playerModel.getHead().zRot = 0F;
          break;
      }
    } else {
      playerModel.crouching = false;
    }

    super.render(entity, entityYaw, partialTicks, poseStack, buffer, light);
  }

  @Override
  protected void renderNameTag(
      Fairy entity,
      Component component,
      PoseStack poseStack,
      MultiBufferSource multiBufferSource,
      int color) {
    this.renderEntityNameTag(entity, poseStack);
    super.renderNameTag(entity, component, poseStack, multiBufferSource, color);
  }

  @Override
  protected int getBlockLightLevel(Fairy entity, BlockPos blockPos) {
    return getEntityLightLevel(entity, blockPos);
  }
}
