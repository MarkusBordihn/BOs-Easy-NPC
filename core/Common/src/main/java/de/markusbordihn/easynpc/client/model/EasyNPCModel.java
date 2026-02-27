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

package de.markusbordihn.easynpc.client.model;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.model.armpose.ModelArmPoseUtils;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.VisibilityHandler;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.LightLayer;

public class EasyNPCModel {

  private static final float MAX_HEAD_YAW = 60.0F;
  private static final float MAX_HEAD_PITCH = 45.0F;
  private static final float DEG_TO_RAD = (float) Math.PI / 180.0F;

  public static boolean setupAnimationStart(
      final EasyNPCRenderStateExtension extension, EasyNPCModelManager modelManager) {
    if (extension == null || modelManager == null) {
      return false;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return false;
    }

    // Always reset model parts first to prevent state bleeding between entities
    modelManager.resetModelParts();

    // Get Model Data
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null || modelData.getModelPose() == ModelPose.VANILLA) {
      return false;
    }

    return setupAnimation(easyNPC, modelData, modelManager);
  }

  public static boolean setupAnimation(
      final EasyNPC<?> easyNPC,
      final ModelDataCapable<?> modelData,
      final EasyNPCModelManager modelManager) {
    if (easyNPC == null || modelData == null || modelManager == null) {
      return false;
    }

    // Handle canceled animations and setup model parts accordingly
    if (modelManager.shouldCancelAnimation(modelData)) {
      modelManager.setupModelParts(
          modelData, modelData.getModelAnimationBehavior() != ModelAnimationBehavior.SMART);
      applyLimitedHeadTracking(easyNPC, modelData, modelManager);
      return true;
    }

    return false;
  }

  private static void applyLimitedHeadTracking(
      final EasyNPC<?> easyNPC,
      final ModelDataCapable<?> modelData,
      final EasyNPCModelManager modelManager) {
    CustomRotation rootRotation = modelData.getModelPartRotation(ModelPartType.ROOT);
    if (rootRotation == null || !rootRotation.locked()) {
      return;
    }
    if (modelData.getModelPartRotation(ModelPartType.HEAD).hasChanged()) {
      return;
    }
    ModelPart head = modelManager.getModelPart(ModelPartType.HEAD);
    if (head == null) {
      return;
    }
    LivingEntity living = easyNPC.getLivingEntity();
    head.yRot =
        Mth.clamp(Mth.wrapDegrees(living.yHeadRot - living.yBodyRot), -MAX_HEAD_YAW, MAX_HEAD_YAW)
            * DEG_TO_RAD;
    head.xRot = Mth.clamp(living.getXRot(), -MAX_HEAD_PITCH, MAX_HEAD_PITCH) * DEG_TO_RAD;
  }

  public static EasyNPC<?> getEasyNPC(final EasyNPCRenderStateExtension extension) {
    if (extension == null) {
      return null;
    }

    UUID uuid = extension.getEasyNpcUUID();
    if (uuid == null) {
      return null;
    }

    return LivingEntityManager.getEasyNPCEntityByUUID(uuid);
  }

  public static int getEntityLightLevel(
      final EasyNPC<?> easyNPC,
      final DisplayAttributeDataCapable<?> displayAttributeData,
      final BlockPos blockPos) {
    if (easyNPC == null || displayAttributeData == null || blockPos == null) {
      return 0;
    }
    int entityLightLevel =
        displayAttributeData.getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL);
    if (entityLightLevel > 0) {
      return entityLightLevel;
    }

    return easyNPC.getLivingEntity().level().getBrightness(LightLayer.BLOCK, blockPos);
  }

  public static boolean renderEntityNameTag(
      final EasyNPCRenderStateExtension extension, final PoseStack poseStack) {
    if (extension == null) {
      return true;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return true;
    }

    // Check if name tag should be rendered at all.
    LocalPlayer player = Minecraft.getInstance().player;
    Entity entity = easyNPC.getEntity();
    if (player != null
        && !VisibilityHandler.handleIsCustomNameVisibleToPlayer(
            easyNPC, player, entity.isCustomNameVisible(), player.distanceToSqr(entity))) {
      return false;
    }

    return true;
  }

  public static void setupAnimationEnd(
      final EasyNPCRenderStateExtension extension, final EasyNPCModelManager modelManager) {
    if (extension == null || modelManager == null) {
      return;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null
        && (modelData.getModelPose() == ModelPose.CUSTOM
            || modelData.getModelPose() == ModelPose.DEFAULT)
        && !modelManager.shouldCancelAnimation(modelData)) {
      modelManager.applySelectiveChanges(modelData);
    }

    setupArmPoses(extension, modelManager);

    // Apply visibility synchronization after all standard animations
    if (modelData != null) {
      if (modelData.getModelAnimationBehavior() == ModelAnimationBehavior.SMART) {
        modelManager.applyVisibilityChanges(modelData);
      } else {
        modelManager.syncModelParts(modelData);
      }
    }
  }

  public static void setupArmPoses(
      final EasyNPCRenderStateExtension extension, final EasyNPCModelManager modelManager) {

    if (extension == null) {
      return;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return;
    }

    ModelArmPose rightArmPose = ModelArmPoseUtils.getArmPoseForRightArm(easyNPC);
    if (rightArmPose != null && rightArmPose != ModelArmPose.DEFAULT) {
      applyArmPoseToModelPart(
          rightArmPose,
          modelManager.getModelPart(ModelPartType.RIGHT_ARM),
          true,
          modelManager.getModelPart(ModelPartType.HEAD),
          easyNPC);
    }

    ModelArmPose leftArmPose = ModelArmPoseUtils.getArmPoseForLeftArm(easyNPC);
    if (leftArmPose != null && leftArmPose != ModelArmPose.DEFAULT) {
      applyArmPoseToModelPart(
          leftArmPose,
          modelManager.getModelPart(ModelPartType.LEFT_ARM),
          false,
          modelManager.getModelPart(ModelPartType.HEAD),
          easyNPC);
    }
  }

  public static void setupLegAnimations(
      final EasyNPC<?> easyNPC,
      final EasyNPCModelManager modelManager,
      final float limbSwing,
      final float limbSwingAmount,
      final boolean isSitting) {
    if (easyNPC == null || modelManager == null || isSitting) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null && modelData.getModelPose() != ModelPose.VANILLA) {
      return;
    }

    ModelPart rightHindLeg = modelManager.getModelPart(ModelPartType.RIGHT_HIND_LEG);
    if (rightHindLeg != null) {
      rightHindLeg.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }

    ModelPart leftHindLeg = modelManager.getModelPart(ModelPartType.LEFT_HIND_LEG);
    if (leftHindLeg != null) {
      leftHindLeg.xRot = Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 1.4F * limbSwingAmount;
    }

    ModelPart rightFrontLeg = modelManager.getModelPart(ModelPartType.RIGHT_FRONT_LEG);
    if (rightFrontLeg != null) {
      rightFrontLeg.xRot = Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 1.4F * limbSwingAmount;
    }

    ModelPart leftFrontLeg = modelManager.getModelPart(ModelPartType.LEFT_FRONT_LEG);
    if (leftFrontLeg != null) {
      leftFrontLeg.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
  }

  public static void applyArmPoseToModelPart(
      final ModelArmPose armPose,
      final ModelPart armModelPart,
      final boolean isRightArm,
      final ModelPart head,
      final EasyNPC<?> easyNPC) {
    if (armPose == null || armModelPart == null) {
      return;
    }

    switch (armPose) {
      case BOW_AND_ARROW -> {
        if (isRightArm) {
          armModelPart.yRot = -0.1F + (head != null ? head.yRot : 0);
        } else {
          armModelPart.yRot = 0.1F + (head != null ? head.yRot : 0) + 0.4F;
        }
        armModelPart.xRot = (float) (-Math.PI / 2) + (head != null ? head.xRot : 0);
      }
      case CROSSBOW_HOLD -> {
        if (isRightArm) {
          armModelPart.yRot = -0.3F + (head != null ? head.yRot : 0);
          armModelPart.xRot = (float) (-Math.PI / 2) + (head != null ? head.xRot : 0) + 0.1F;
        } else {
          armModelPart.yRot = 0.6F + (head != null ? head.yRot : 0);
          armModelPart.xRot = -1.5F + (head != null ? head.xRot : 0);
        }
      }
      case CROSSBOW_CHARGE -> {
        if (isRightArm) {
          armModelPart.yRot = -0.8F;
          armModelPart.xRot = -0.97079635F;
        } else {
          armModelPart.yRot = 0.85F;
          armModelPart.xRot = (float) (-Math.PI / 2);
        }
      }
      case SPYGLASS -> {
        armModelPart.xRot = Mth.clamp(armModelPart.xRot, -1.2F, 1.2F) - 1.9198622F;
        armModelPart.yRot = isRightArm ? 0.5235988F : -0.5235988F;
      }
      case ATTACKING_WITH_MELEE_WEAPON -> {
        float swingProgress = 0.0F;
        if (easyNPC != null && easyNPC.getLivingEntity() != null) {
          var livingEntity = easyNPC.getLivingEntity();
          if (livingEntity.swinging) {
            swingProgress = Mth.clamp(livingEntity.swingTime / 6.0F, 0.0F, 1.0F);
            swingProgress = Mth.sin(swingProgress * (float) Math.PI);
          }
        }

        armModelPart.xRot = swingProgress * -1.8F - 0.3F;
        armModelPart.yRot = isRightArm ? -0.3F : 0.3F;
        armModelPart.zRot = swingProgress * (isRightArm ? -0.5F : 0.5F);
      }
      case SPELLCASTING -> {
        armModelPart.xRot = armModelPart.xRot * 0.5F - (float) Math.PI;
        armModelPart.yRot = 0.0F;
      }
      case GUN_HOLD -> {
        if (isRightArm) {
          armModelPart.yRot = -0.2F + (head != null ? head.yRot : 0);
          armModelPart.xRot = (float) (-Math.PI / 2) + (head != null ? head.xRot : 0);
        } else {
          armModelPart.yRot = 0.4F + (head != null ? head.yRot : 0);
          armModelPart.xRot = -1.3F + (head != null ? head.xRot : 0);
        }
      }
      case ATTACKING -> {
        float attackRotation = 0.8F;
        armModelPart.xRot = attackRotation * -0.8F;
        armModelPart.yRot = isRightArm ? -0.2F : 0.2F;
      }
      case CELEBRATING -> {
        armModelPart.xRot = -0.5F;
        armModelPart.yRot = isRightArm ? -0.3F : 0.3F;
        armModelPart.zRot = isRightArm ? 0.3F : -0.3F;
      }
      case DANCING -> {
        float danceRotation = Mth.sin(System.currentTimeMillis() * 0.001F) * 0.3F;
        armModelPart.xRot = danceRotation;
        armModelPart.yRot = isRightArm ? -0.2F : 0.2F;
        armModelPart.zRot = isRightArm ? danceRotation * 0.5F : -danceRotation * 0.5F;
      }
      case CROSSED -> {
        if (isRightArm) {
          armModelPart.xRot = -0.8F;
          armModelPart.yRot = 0.6F;
        } else {
          armModelPart.xRot = -0.8F;
          armModelPart.yRot = -0.6F;
        }
      }
      case NEUTRAL -> {
        armModelPart.xRot = 0.0F;
        armModelPart.yRot = 0.0F;
        armModelPart.zRot = 0.0F;
      }
      default -> {
        // Keep default positioning for DEFAULT and CUSTOM poses
      }
    }
  }
}
