/**
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

package de.markusbordihn.easynpc.client.renderer.entity;

import java.util.EnumMap;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;

import net.minecraft.Util;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelLayers;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.MobRenderer;
import net.minecraft.client.renderer.entity.layers.CustomHeadLayer;
import net.minecraft.client.renderer.entity.layers.ElytraLayer;
import net.minecraft.client.renderer.entity.layers.HumanoidArmorLayer;
import net.minecraft.client.renderer.entity.layers.ItemInHandLayer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Rotations;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.CustomZombieModel;
import de.markusbordihn.easynpc.client.renderer.entity.layers.OuterLayer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.npc.Zombie.Variant;
import de.markusbordihn.easynpc.model.ModelPose;

@OnlyIn(Dist.CLIENT)
public class ZombieRenderer extends MobRenderer<EasyNPCEntity, CustomZombieModel<EasyNPCEntity>> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Variant Textures
  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        // Build-in Variants
        map.put(Variant.DROWNED, new ResourceLocation("textures/entity/zombie/drowned.png"));
        map.put(Variant.HUSK, new ResourceLocation("textures/entity/zombie/husk.png"));
        map.put(Variant.ZOMBIE, new ResourceLocation("textures/entity/zombie/zombie.png"));

        // Custom Variants
      });
  protected static final Map<Variant, ResourceLocation> OUTER_TEXTURE_BY_VARIANT =
      Util.make(new EnumMap<>(Variant.class), map -> {
        // Build-in Variants
        map.put(Variant.DROWNED,
            new ResourceLocation("textures/entity/zombie/drowned_outer_layer.png"));
      });
  protected static final ResourceLocation DEFAULT_TEXTURE = TEXTURE_BY_VARIANT.get(Variant.ZOMBIE);

  public ZombieRenderer(EntityRendererProvider.Context context) {
    super(context, new CustomZombieModel<>(context.bakeLayer(ModelLayers.ZOMBIE)), 0.5F);
    this.addLayer(new HumanoidArmorLayer<>(this,
        new HumanoidModel<>(context.bakeLayer(ModelLayers.ZOMBIE_INNER_ARMOR)),
        new HumanoidModel<>(context.bakeLayer(ModelLayers.ZOMBIE_OUTER_ARMOR))));
    this.addLayer(
        new CustomHeadLayer<>(this, context.getModelSet(), context.getItemInHandRenderer()));
    this.addLayer(new ItemInHandLayer<>(this, context.getItemInHandRenderer()));
    this.addLayer(new OuterLayer<>(this, context.getModelSet(), OUTER_TEXTURE_BY_VARIANT));
    this.addLayer(new ElytraLayer<>(this, context.getModelSet()));
  }

  @Override
  public ResourceLocation getTextureLocation(EasyNPCEntity entity) {
    switch (entity.getSkinType()) {
      case CUSTOM:
        return CustomTextureManager.getOrCreateTextureWithDefault(entity, DEFAULT_TEXTURE);
      case PLAYER_SKIN:
      case SECURE_REMOTE_URL:
      case INSECURE_REMOTE_URL:
        return PlayerTextureManager.getOrCreateTextureWithDefault(entity, DEFAULT_TEXTURE);
      default:
        return TEXTURE_BY_VARIANT.getOrDefault(entity.getVariant(), DEFAULT_TEXTURE);
    }
  }

  @Override
  protected void scale(EasyNPCEntity entity, PoseStack poseStack, float unused) {
    if (entity.isBaby()) {
      poseStack.scale(entity.getScaleX() * 0.5f, entity.getScaleY() * 0.5f,
          entity.getScaleZ() * 0.5f);
    } else {
      poseStack.scale(entity.getScaleX(), entity.getScaleY(), entity.getScaleZ());
    }
  }

  @Override
  public void render(EasyNPCEntity entity, float entityYaw, float partialTicks, PoseStack poseStack,
      net.minecraft.client.renderer.MultiBufferSource buffer, int light) {
    CustomZombieModel<EasyNPCEntity> playerModel = this.getModel();

    // Model Rotation
    Rotations rootRotation = entity.getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, 1, 0);
      poseStack.mulPose(Vector3f.XP.rotation(rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(rootRotation.getZ()));
      poseStack.translate(0, -1, 0);
    }

    // Render additional poses
    if (entity.getModelPose() == ModelPose.DEFAULT) {

      // Crouching
      playerModel.crouching = entity.isCrouching();

      switch (entity.getPose()) {
        case DYING:
          poseStack.translate(-1.0D, 0.0D, 0.0D);
          poseStack.mulPose(Vector3f.YP.rotationDegrees(180f));
          poseStack.mulPose(Vector3f.ZP.rotationDegrees(this.getFlipDegrees(entity)));
          poseStack.mulPose(Vector3f.YP.rotationDegrees(270.0f));
          playerModel.getHead().xRot = -0.7853982F;
          playerModel.getHead().yRot = -0.7853982F;
          playerModel.getHead().zRot = -0.7853982F;
          break;
        case LONG_JUMPING:
          playerModel.leftArmPose = HumanoidModel.ArmPose.CROSSBOW_HOLD;
          playerModel.rightArmPose = HumanoidModel.ArmPose.SPYGLASS;
          break;
        case SLEEPING:
          poseStack.translate(1.0D, 0.0D, 0.0D);
          break;
        case SPIN_ATTACK:
          playerModel.leftArmPose = HumanoidModel.ArmPose.BLOCK;
          playerModel.rightArmPose = HumanoidModel.ArmPose.THROW_SPEAR;
          poseStack.mulPose(Vector3f.YP.rotationDegrees(-35f));
          break;
        default:
          playerModel.leftArmPose = HumanoidModel.ArmPose.EMPTY;
          playerModel.rightArmPose = HumanoidModel.ArmPose.EMPTY;
          playerModel.getHead().xRot = 0F;
          playerModel.getHead().yRot = 0F;
          playerModel.getHead().zRot = 0F;
          break;
      }
    }

    super.render(entity, entityYaw, partialTicks, poseStack, buffer, light);
  }

  @Override
  protected void renderNameTag(EasyNPCEntity entity, Component component, PoseStack poseStack,
      MultiBufferSource multiBufferSource, int color) {

    // Model Rotation
    Rotations rootRotation = entity.getModelRootRotation();
    if (rootRotation != null) {
      poseStack.translate(0, 1, 0);
      poseStack.mulPose(Vector3f.XP.rotation(-rootRotation.getX()));
      poseStack.mulPose(Vector3f.YP.rotation(-rootRotation.getY()));
      poseStack.mulPose(Vector3f.ZP.rotation(-rootRotation.getZ()));
      poseStack.translate(0, -1, 0);
    }

    super.renderNameTag(entity, component, poseStack, multiBufferSource, color);
  }

  @Override
  protected int getBlockLightLevel(EasyNPCEntity entity, BlockPos blockPos) {
    return 7;
  }

}
