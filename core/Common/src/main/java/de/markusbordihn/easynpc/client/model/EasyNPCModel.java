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
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.client.model.armpose.ModelArmPoseUtils;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.level.LightLayer;

public class EasyNPCModel {

  public static boolean setupAnimationStart(
      final EasyNPC<?> easyNPC, final EasyNPCModelManager modelManager) {
    if (easyNPC == null || modelManager == null) {
      return false;
    }

    // Always reset model parts first to prevent state bleeding between entities
    modelManager.resetModelParts();

    // Get Model Data
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null || modelData.getModelPose() == ModelPose.DEFAULT) {
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
      return true;
    }

    return false;
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

  public static void renderEntityNameTag(
      final EasyNPC<?> easyNPC, final ModelDataCapable<?> modelData, final PoseStack poseStack) {
    if (easyNPC == null || modelData == null) {
      return;
    }
    CustomRotation rootRotation = modelData.getModelPartRotation(ModelPartType.ROOT);
    if (rootRotation != null) {
      poseStack.translate(0, 1, 0);
      poseStack.mulPose(Axis.XP.rotationDegrees(-rootRotation.x()));
      poseStack.mulPose(Axis.YP.rotationDegrees(-rootRotation.y()));
      poseStack.mulPose(Axis.ZP.rotationDegrees(-rootRotation.z()));
      poseStack.translate(0, -1, 0);
    }
  }

  public static void setupAnimationEnd(
      final EasyNPC<?> easyNPC, final EasyNPCModelManager modelManager) {
    if (easyNPC == null || modelManager == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null
        && modelData.getModelPose() == ModelPose.CUSTOM
        && !modelManager.shouldCancelAnimation(modelData)) {
      modelManager.applySelectiveChanges(modelData);
    }

    setupArmPoses(easyNPC, modelManager);

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
      final EasyNPC<?> easyNPC, final EasyNPCModelManager modelManager) {
    if (easyNPC == null || modelManager == null) {
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
