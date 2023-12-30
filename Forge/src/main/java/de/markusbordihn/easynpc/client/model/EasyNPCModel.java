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

import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.util.Mth;

public interface EasyNPCModel {

  // General model positions
  CustomPosition MODEL_ROOT_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_HEAD_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_BODY_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_ARMS_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_LEFT_ARM_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_RIGHT_ARM_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_LEFT_LEG_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_RIGHT_LEG_POSITION = new CustomPosition(0, 0, 0);

  // Animal model position
  CustomPosition MODEL_LEFT_FRONT_LEG_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_RIGHT_FRONT_LEG_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_LEFT_HIND_LEG_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_RIGHT_HIND_LEG_POSITION = new CustomPosition(0, 0, 0);

  // Bird model position
  CustomPosition MODEL_LEFT_WING_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition MODEL_RIGHT_WING_POSITION = new CustomPosition(0, 0, 0);

  // General model rotations
  Rotations MODEL_HEAD_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_BODY_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_ARMS_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_LEFT_ARM_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_RIGHT_ARM_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_LEFT_LEG_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_RIGHT_LEG_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_ROOT_ROTATION = new Rotations(0, 0, 0);

  // Animal model rotations
  Rotations MODEL_LEFT_FRONT_LEG_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_RIGHT_FRONT_LEG_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_LEFT_HIND_LEG_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_RIGHT_HIND_LEG_ROTATION = new Rotations(0, 0, 0);

  // Bird model rotations
  Rotations MODEL_LEFT_WING_ROTATION = new Rotations(0, 0, 0);
  Rotations MODEL_RIGHT_WING_ROTATION = new Rotations(0, 0, 0);

  // Allay Model specific positions
  Rotations ALLAY_MODEL_LEFT_ARM_ROTATION = new Rotations(0, -0.27925268F, 0);
  Rotations ALLAY_MODEL_RIGHT_ARM_ROTATION = new Rotations(0, 0.27925268F, 0);
  CustomPosition ALLAY_MODEL_ROOT_POSITION = new CustomPosition(0, 15f, 0);
  CustomPosition ALLAY_MODEL_LEFT_ARM_POSITION = new CustomPosition(2.0F, 0.0F, 0.0F);
  CustomPosition ALLAY_MODEL_RIGHT_ARM_POSITION = new CustomPosition(-2.0F, 0.0F, 0.0F);

  // Humanoid Model specific positions
  CustomPosition HUMANOID_MODEL_HEAD_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition HUMANOID_MODEL_BODY_POSITION = new CustomPosition(0, 0, 0);
  CustomPosition HUMANOID_MODEL_LEFT_ARM_POSITION = new CustomPosition(5f, 2f, 0f);
  CustomPosition HUMANOID_MODEL_RIGHT_ARM_POSITION = new CustomPosition(-5f, 2f, 0f);
  CustomPosition HUMANOID_MODEL_LEFT_LEG_POSITION = new CustomPosition(1.9f, 12f, 0.1f);
  CustomPosition HUMANOID_MODEL_RIGHT_LEG_POSITION = new CustomPosition(-1.9f, 12f, 0.1f);

  // Villager Model specific positions
  CustomPosition VILLAGER_MODEL_ARMS_POSITION = new CustomPosition(0.0f, 2.0f, 0.0f);
  CustomPosition VILLAGER_MODEL_LEFT_LEG_POSITION = new CustomPosition(1.9f, 12f, 0.1f);
  CustomPosition VILLAGER_MODEL_RIGHT_LEG_POSITION = new CustomPosition(-1.9f, 12f, 0.1f);

  // Villager Model specific rotations
  Rotations VILLAGER_MODEL_ARMS_ROTATION = new Rotations(-0.8f, 0.0f, 0.0f);

  private static void setModelPartRotation(ModelPart modelPart, Rotations rotations) {
    modelPart.xRot = rotations.getX();
    modelPart.yRot = rotations.getY();
    modelPart.zRot = rotations.getZ();
  }

  private static void setModelPartPosition(ModelPart modelPart, CustomPosition position) {
    modelPart.x = position.x();
    modelPart.y = position.y();
    modelPart.z = position.z();
  }

  static boolean animateHumanoidModel(
      EasyNPCModel easyNPCModel,
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {

    float attackTime = 0;
    boolean hasArmAnimations = false;
    boolean hasDefaultHead = easyNPCModel.hasDefaultHumanoidModelHead(headPart);
    boolean hasDefaultBody = easyNPCModel.hasDefaultHumanoidModelBody(bodyPart);
    boolean hasDefaultRightArm = easyNPCModel.hasDefaultHumanoidModelRightArm(rightArmPart);
    boolean hasDefaultLeftArm = easyNPCModel.hasDefaultHumanoidModelLeftArm(leftArmPart);
    boolean hasDefaultRightLeg = easyNPCModel.hasDefaultHumanoidModelRightLeg(rightLegPart);
    boolean hasDefaultLeftLeg = easyNPCModel.hasDefaultHumanoidModelLeftLeg(leftLegPart);
    if (easyNPCModel instanceof PlayerModel<?> playerModel) {
      attackTime = playerModel.attackTime;
    }

    // Allow arm animation if arms are not adjusted.
    if (hasDefaultRightArm && hasDefaultLeftArm) {
      hasArmAnimations =
          animateHumanoidArmModel(
              entity,
              headPart,
              bodyPart,
              rightArmPart,
              leftArmPart,
              attackTime,
              ageInTicks,
              limbSwing,
              limbSwingAmount);
    } else {
      if (hasDefaultRightArm) {
        rightArmPart.xRot =
            Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 2.0F * limbSwingAmount * 0.5F;
      } else if (hasDefaultLeftArm) {
        leftArmPart.xRot = Mth.cos(limbSwing * 0.6662F) * 2.0F * limbSwingAmount * 0.5F;
      }
    }

    // Allow leg animation if legs are not adjusted.
    if (hasDefaultRightLeg) {
      rightLegPart.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
    if (hasDefaultLeftLeg) {
      leftLegPart.xRot = Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 1.4F * limbSwingAmount;
    }

    // Was any animation applied?
    return hasArmAnimations
        || !hasDefaultHead
        || !hasDefaultBody
        || !hasDefaultRightArm
        || !hasDefaultLeftArm
        || !hasDefaultRightLeg
        || !hasDefaultLeftLeg;
  }

  static boolean animateHumanoidArmModel(
      EasyNPCEntity entity,
      ModelPart headModelPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float attackTime,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    ModelArmPose modelArmPose = entity.getModelArmPose(entity);
    switch (modelArmPose) {
      case ATTACKING_WITH_MELEE_WEAPON:
        AnimationUtils.swingWeaponDown(rightArmPart, leftArmPart, entity, attackTime, ageInTicks);
        break;
      case ATTACKING:
        AnimationUtils.swingWeaponDown(rightArmPart, leftArmPart, entity, attackTime, ageInTicks);
        break;
      case BOW_AND_ARROW:
        rightArmPart.yRot = -0.1F + headModelPart.yRot;
        rightArmPart.xRot = (-(float) Math.PI / 2F) + headModelPart.xRot;
        leftArmPart.xRot = -0.9424779F + headModelPart.xRot;
        leftArmPart.yRot = headModelPart.yRot - 0.4F;
        leftArmPart.zRot = ((float) Math.PI / 2F);
        break;
      case CELEBRATING:
        rightArmPart.z = 0.0F;
        rightArmPart.x = -5.0F;
        rightArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.05F;
        rightArmPart.zRot = 2.670354F;
        rightArmPart.yRot = 0.0F;
        leftArmPart.z = 0.0F;
        leftArmPart.x = 5.0F;
        leftArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.05F;
        leftArmPart.zRot = -2.3561945F;
        leftArmPart.yRot = 0.0F;
        break;
      case CROSSBOW_CHARGE:
        AnimationUtils.animateCrossbowCharge(rightArmPart, leftArmPart, entity, true);
        break;
      case CROSSBOW_HOLD:
        AnimationUtils.animateCrossbowHold(rightArmPart, leftArmPart, headModelPart, true);
        break;
      case DANCING:
        float swingAmount = ageInTicks / 60.0F;
        headModelPart.x = Mth.sin(swingAmount * 10.0F);
        headModelPart.y = Mth.sin(swingAmount * 40.0F) + 0.4F;
        rightArmPart.zRot =
            ((float) Math.PI / 180F) * (70.0F + Mth.cos(swingAmount * 40.0F) * 10.0F);
        leftArmPart.zRot = rightArmPart.zRot * -1.0F;
        rightArmPart.y = Mth.sin(swingAmount * 40.0F) * 0.5F + 1.5F;
        leftArmPart.y = Mth.sin(swingAmount * 40.0F) * 0.5F + 1.5F;
        bodyPart.y = Mth.sin(swingAmount * 40.0F) * 0.35F;
        break;
      case SPELLCASTING:
        rightArmPart.z = 0.0F;
        rightArmPart.x = -5.0F;
        leftArmPart.z = 0.0F;
        leftArmPart.x = 5.0F;
        rightArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.25F;
        leftArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.25F;
        rightArmPart.zRot = 2.3561945F;
        leftArmPart.zRot = -2.3561945F;
        rightArmPart.yRot = 0.0F;
        leftArmPart.yRot = 0.0F;
        break;
      case NEUTRAL:
      default:
        return false;
    }
    return true;
  }

  static void setupArmModel(
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart leftArmPart,
      ModelPart rightArmPart,
      float netHeadYaw,
      float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(
        headPart,
        entity.getModelHeadPosition(),
        entity.getModelHeadRotation(),
        entity.isModelHeadVisible(),
        netHeadYaw,
        headPitch);
    CustomModelHelper.setPositionRotationVisibility(
        bodyPart,
        entity.getModelBodyPosition(),
        entity.getModelBodyRotation(),
        entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftArmPart,
        entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(),
        entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightArmPart,
        entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(),
        entity.isModelRightArmVisible());
  }

  static void setupAnimalModel(
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart leftFrontLeg,
      ModelPart rightFrontLeg,
      ModelPart leftHindLeg,
      ModelPart rightHindLeg,
      float netHeadYaw,
      float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(
        headPart,
        entity.getModelHeadPosition(),
        entity.getModelHeadRotation(),
        entity.isModelHeadVisible(),
        netHeadYaw,
        headPitch);
    CustomModelHelper.setPositionRotationVisibility(
        bodyPart,
        entity.getModelBodyPosition(),
        entity.getModelBodyRotation(),
        entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftFrontLeg,
        entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(),
        entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightFrontLeg,
        entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(),
        entity.isModelRightArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftHindLeg,
        entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(),
        entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightHindLeg,
        entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(),
        entity.isModelRightLegVisible());
  }

  static void setupBirdModel(
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart leftWing,
      ModelPart rightWing,
      ModelPart leftLeg,
      ModelPart rightLeg,
      float netHeadYaw,
      float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(
        headPart,
        entity.getModelHeadPosition(),
        entity.getModelHeadRotation(),
        entity.isModelHeadVisible(),
        netHeadYaw,
        headPitch);
    CustomModelHelper.setPositionRotationVisibility(
        bodyPart,
        entity.getModelBodyPosition(),
        entity.getModelBodyRotation(),
        entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftWing,
        entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(),
        entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightWing,
        entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(),
        entity.isModelRightArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftLeg,
        entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(),
        entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightLeg,
        entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(),
        entity.isModelRightLegVisible());
  }

  static void setupHumanoidModel(
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float netHeadYaw,
      float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(
        headPart,
        entity.getModelHeadPosition(),
        entity.getModelHeadRotation(),
        entity.isModelHeadVisible(),
        netHeadYaw,
        headPitch);
    CustomModelHelper.setPositionRotationVisibility(
        bodyPart,
        entity.getModelBodyPosition(),
        entity.getModelBodyRotation(),
        entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftArmPart,
        entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(),
        entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightArmPart,
        entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(),
        entity.isModelRightArmVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftLegPart,
        entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(),
        entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightLegPart,
        entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(),
        entity.isModelRightLegVisible());
  }

  static void resetArmModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart leftArm,
      ModelPart rightArm) {

    // Reset all rotations.
    setModelPartRotation(headPart, easyNPCModel.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, easyNPCModel.getDefaultModelBodyRotation());
    setModelPartRotation(leftArm, easyNPCModel.getDefaultModelLeftArmRotation());
    setModelPartRotation(rightArm, easyNPCModel.getDefaultModelRightArmRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, easyNPCModel.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, easyNPCModel.getDefaultModelHeadPosition());
    setModelPartPosition(leftArm, easyNPCModel.getDefaultModelLeftArmPosition());
    setModelPartPosition(rightArm, easyNPCModel.getDefaultModelRightArmPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftArm.visible = true;
    rightArm.visible = true;
  }

  static void setupVillagerModel(
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart armsPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float netHeadYaw,
      float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(
        headPart,
        entity.getModelHeadPosition(),
        entity.getModelHeadRotation(),
        entity.isModelHeadVisible(),
        netHeadYaw,
        headPitch);
    CustomModelHelper.setPositionRotationVisibility(
        bodyPart,
        entity.getModelBodyPosition(),
        entity.getModelBodyRotation(),
        entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(
        armsPart,
        entity.getModelArmsPosition(),
        entity.getModelArmsRotation(),
        entity.isModelArmsVisible());
    CustomModelHelper.setPositionRotationVisibility(
        leftLegPart,
        entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(),
        entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(
        rightLegPart,
        entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(),
        entity.isModelRightLegVisible());
  }

  static void resetAnimalModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart leftFrontLeg,
      ModelPart rightFrontLeg,
      ModelPart leftHindLeg,
      ModelPart rightHindLeg) {

    // Reset all rotations.
    setModelPartRotation(headPart, easyNPCModel.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, easyNPCModel.getDefaultModelBodyRotation());
    setModelPartRotation(leftFrontLeg, easyNPCModel.getDefaultModelLeftFrontLegRotation());
    setModelPartRotation(rightFrontLeg, easyNPCModel.getDefaultModelRightFrontLegRotation());
    setModelPartRotation(leftHindLeg, easyNPCModel.getDefaultModelLeftHindLegRotation());
    setModelPartRotation(rightHindLeg, easyNPCModel.getDefaultModelRightHindLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, easyNPCModel.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, easyNPCModel.getDefaultModelHeadPosition());
    setModelPartPosition(leftFrontLeg, easyNPCModel.getDefaultModelLeftFrontLegPosition());
    setModelPartPosition(rightFrontLeg, easyNPCModel.getDefaultModelRightFrontLegPosition());
    setModelPartPosition(leftHindLeg, easyNPCModel.getDefaultModelLeftHindLegPosition());
    setModelPartPosition(rightHindLeg, easyNPCModel.getDefaultModelRightHindLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftFrontLeg.visible = true;
    rightFrontLeg.visible = true;
    leftHindLeg.visible = true;
    rightHindLeg.visible = true;
  }

  static void resetBirdModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart leftWingPart,
      ModelPart rightWingPart,
      ModelPart leftLegPart,
      ModelPart rightLegPart) {

    // Reset all rotations.
    setModelPartRotation(headPart, easyNPCModel.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, easyNPCModel.getDefaultModelBodyRotation());
    setModelPartRotation(leftWingPart, easyNPCModel.getDefaultModelLeftWingRotation());
    setModelPartRotation(rightWingPart, easyNPCModel.getDefaultModelRightArmRotation());
    setModelPartRotation(leftLegPart, easyNPCModel.getDefaultModelLeftLegRotation());
    setModelPartRotation(rightLegPart, easyNPCModel.getDefaultModelRightLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, easyNPCModel.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, easyNPCModel.getDefaultModelHeadPosition());
    setModelPartPosition(leftWingPart, easyNPCModel.getDefaultModelLeftWingPosition());
    setModelPartPosition(rightWingPart, easyNPCModel.getDefaultModelRightWingPosition());
    setModelPartPosition(leftLegPart, easyNPCModel.getDefaultModelLeftLegPosition());
    setModelPartPosition(rightLegPart, easyNPCModel.getDefaultModelRightLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftWingPart.visible = true;
    rightWingPart.visible = true;
    leftLegPart.visible = true;
    rightLegPart.visible = true;
  }

  static void resetHumanoidModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart) {

    // Reset all rotations.
    setModelPartRotation(headPart, easyNPCModel.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, easyNPCModel.getDefaultModelBodyRotation());
    setModelPartRotation(leftArmPart, easyNPCModel.getDefaultModelLeftArmRotation());
    setModelPartRotation(rightArmPart, easyNPCModel.getDefaultModelRightArmRotation());
    setModelPartRotation(leftLegPart, easyNPCModel.getDefaultModelLeftLegRotation());
    setModelPartRotation(rightLegPart, easyNPCModel.getDefaultModelRightLegRotation());

    // Reset all positions.
    setModelPartPosition(headPart, easyNPCModel.getDefaultModelHeadPosition());
    setModelPartPosition(bodyPart, easyNPCModel.getDefaultModelBodyPosition());
    setModelPartPosition(leftArmPart, easyNPCModel.getDefaultModelLeftArmPosition());
    setModelPartPosition(rightArmPart, easyNPCModel.getDefaultModelRightArmPosition());
    setModelPartPosition(leftLegPart, easyNPCModel.getDefaultModelLeftLegPosition());
    setModelPartPosition(rightLegPart, easyNPCModel.getDefaultModelRightLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftArmPart.visible = true;
    rightArmPart.visible = true;
    leftLegPart.visible = true;
    rightLegPart.visible = true;
  }

  static void resetHierarchicalModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart) {
    resetHumanoidModel(
        easyNPCModel, headPart, bodyPart, rightArmPart, leftArmPart, rightLegPart, leftLegPart);
  }

  static void resetVillagerModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart armsPart,
      ModelPart leftLegPart,
      ModelPart rightLegPart) {
    // Reset all rotations.
    setModelPartRotation(headPart, easyNPCModel.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, easyNPCModel.getDefaultModelBodyRotation());
    setModelPartRotation(armsPart, easyNPCModel.getDefaultModelArmsRotation());
    setModelPartRotation(leftLegPart, easyNPCModel.getDefaultModelLeftLegRotation());
    setModelPartRotation(rightLegPart, easyNPCModel.getDefaultModelRightLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, easyNPCModel.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, easyNPCModel.getDefaultModelHeadPosition());
    setModelPartPosition(armsPart, easyNPCModel.getDefaultModelArmsPosition());
    setModelPartPosition(leftLegPart, easyNPCModel.getDefaultModelLeftLegPosition());
    setModelPartPosition(rightLegPart, easyNPCModel.getDefaultModelRightLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    armsPart.visible = true;
    leftLegPart.visible = true;
    rightLegPart.visible = true;
  }

  private static boolean equalPositionAndRotation(
      ModelPart modelPart, CustomPosition position, Rotations rotations) {
    return equalPosition(modelPart, position) && equalRotation(modelPart, rotations);
  }

  private static boolean equalPosition(ModelPart modelPart, CustomPosition position) {
    return modelPart != null
        && Math.abs(modelPart.x - position.x()) < 0.01
        && Math.abs(modelPart.y - position.y()) < 0.01
        && Math.abs(modelPart.z - position.z()) < 0.01;
  }

  private static boolean equalRotation(ModelPart modelPart, Rotations rotations) {
    return modelPart != null
        && Math.abs(modelPart.xRot - rotations.getX()) < 0.01
        && Math.abs(modelPart.yRot - rotations.getY()) < 0.01
        && Math.abs(modelPart.zRot - rotations.getZ()) < 0.01;
  }

  default CustomPosition getDefaultModelHeadPosition() {
    return MODEL_HEAD_POSITION;
  }

  default CustomPosition getDefaultModelBodyPosition() {
    return MODEL_BODY_POSITION;
  }

  default CustomPosition getDefaultModelArmsPosition() {
    return VILLAGER_MODEL_ARMS_POSITION;
  }

  default CustomPosition getDefaultModelLeftArmPosition() {
    return HUMANOID_MODEL_LEFT_ARM_POSITION;
  }

  default CustomPosition getDefaultModelRightArmPosition() {
    return HUMANOID_MODEL_RIGHT_ARM_POSITION;
  }

  default CustomPosition getDefaultModelLeftLegPosition() {
    return HUMANOID_MODEL_LEFT_LEG_POSITION;
  }

  default CustomPosition getDefaultModelRightLegPosition() {
    return HUMANOID_MODEL_RIGHT_LEG_POSITION;
  }

  default CustomPosition getDefaultModelLeftFrontLegPosition() {
    return MODEL_LEFT_FRONT_LEG_POSITION;
  }

  default CustomPosition getDefaultModelRightFrontLegPosition() {
    return MODEL_RIGHT_FRONT_LEG_POSITION;
  }

  default CustomPosition getDefaultModelLeftHindLegPosition() {
    return MODEL_LEFT_HIND_LEG_POSITION;
  }

  default CustomPosition getDefaultModelRightHindLegPosition() {
    return MODEL_RIGHT_HIND_LEG_POSITION;
  }

  default CustomPosition getDefaultModelLeftWingPosition() {
    return MODEL_LEFT_WING_POSITION;
  }

  default CustomPosition getDefaultModelRightWingPosition() {
    return MODEL_RIGHT_WING_POSITION;
  }

  default CustomPosition getDefaultModelRootPosition() {
    return MODEL_ROOT_POSITION;
  }

  default boolean getDefaultModelLockRotation() {
    return false;
  }

  default Rotations getDefaultModelHeadRotation() {
    return MODEL_HEAD_ROTATION;
  }

  default Rotations getDefaultModelBodyRotation() {
    return MODEL_BODY_ROTATION;
  }

  default Rotations getDefaultModelArmsRotation() {
    return VILLAGER_MODEL_ARMS_ROTATION;
  }

  default Rotations getDefaultModelLeftArmRotation() {
    return MODEL_LEFT_ARM_ROTATION;
  }

  default Rotations getDefaultModelRightArmRotation() {
    return MODEL_RIGHT_ARM_ROTATION;
  }

  default Rotations getDefaultModelLeftLegRotation() {
    return MODEL_LEFT_LEG_ROTATION;
  }

  default Rotations getDefaultModelRightLegRotation() {
    return MODEL_RIGHT_LEG_ROTATION;
  }

  default Rotations getDefaultModelLeftFrontLegRotation() {
    return MODEL_LEFT_FRONT_LEG_ROTATION;
  }

  default Rotations getDefaultModelRightFrontLegRotation() {
    return MODEL_RIGHT_FRONT_LEG_ROTATION;
  }

  default Rotations getDefaultModelLeftHindLegRotation() {
    return MODEL_LEFT_HIND_LEG_ROTATION;
  }

  default Rotations getDefaultModelRightHindLegRotation() {
    return MODEL_RIGHT_HIND_LEG_ROTATION;
  }

  default Rotations getDefaultModelLeftWingRotation() {
    return MODEL_LEFT_WING_ROTATION;
  }

  default Rotations getDefaultModelRightWingRotation() {
    return MODEL_RIGHT_WING_ROTATION;
  }

  default Rotations getDefaultModelRootRotation() {
    return MODEL_ROOT_ROTATION;
  }

  default void setupModel() {}

  default void setupHierarchicalModel(
      EasyNPCEntity entity,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float netHeadYaw,
      float headPitch) {
    setupHumanoidModel(
        entity,
        headPart,
        bodyPart,
        rightArmPart,
        leftArmPart,
        rightLegPart,
        leftLegPart,
        netHeadYaw,
        headPitch);
  }

  default boolean hasDefaultHumanoidModelHead(ModelPart headPart) {
    return equalPositionAndRotation(headPart, HUMANOID_MODEL_HEAD_POSITION, MODEL_HEAD_ROTATION);
  }

  default boolean hasDefaultHumanoidModelBody(ModelPart bodyPart) {
    return equalPositionAndRotation(bodyPart, HUMANOID_MODEL_BODY_POSITION, MODEL_BODY_ROTATION);
  }

  default boolean hasDefaultHumanoidModelLeftArm(ModelPart leftArmPart) {
    return equalPositionAndRotation(
        leftArmPart, HUMANOID_MODEL_LEFT_ARM_POSITION, MODEL_LEFT_ARM_ROTATION);
  }

  default boolean hasDefaultHumanoidModelRightArm(ModelPart rightArmPart) {
    return equalPositionAndRotation(
        rightArmPart, HUMANOID_MODEL_RIGHT_ARM_POSITION, MODEL_RIGHT_ARM_ROTATION);
  }

  default boolean hasDefaultHumanoidModelLeftLeg(ModelPart leftLegPart) {
    return equalPositionAndRotation(
        leftLegPart, HUMANOID_MODEL_LEFT_LEG_POSITION, MODEL_LEFT_LEG_ROTATION);
  }

  default boolean hasDefaultHumanoidModelRightLeg(ModelPart rightLegPart) {
    return equalPositionAndRotation(
        rightLegPart, HUMANOID_MODEL_RIGHT_LEG_POSITION, MODEL_RIGHT_LEG_ROTATION);
  }
}
