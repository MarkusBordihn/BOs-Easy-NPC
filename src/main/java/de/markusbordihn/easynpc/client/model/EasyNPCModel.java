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

import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
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

  static void animateHumanoidModel(
      EasyNPCModel easyNPCModel,
      ModelPart headPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float limbSwing,
      float limbSwingAmount) {

    // Allow arm animation if arms are not adjusted.
    if (easyNPCModel.hasDefaultHumanoidModelRightArm(rightArmPart)) {
      rightArmPart.xRot =
          Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 2.0F * limbSwingAmount * 0.5F;
    }
    if (easyNPCModel.hasDefaultHumanoidModelLeftArm(leftArmPart)) {
      leftArmPart.xRot = Mth.cos(limbSwing * 0.6662F) * 2.0F * limbSwingAmount * 0.5F;
    }

    // Allow leg animation if legs are not adjusted.
    if (easyNPCModel.hasDefaultHumanoidModelRightLeg(rightLegPart)) {
      rightLegPart.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
    if (easyNPCModel.hasDefaultHumanoidModelLeftLeg(leftLegPart)) {
      leftLegPart.xRot = Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 1.4F * limbSwingAmount;
    }
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
    setModelPartPosition(bodyPart, easyNPCModel.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, easyNPCModel.getDefaultModelHeadPosition());
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

  default boolean hasDefaultHumanoidModelLeftArm(ModelPart leftArmPart) {
    if (leftArmPart == null) {
      return false;
    }
    return leftArmPart.x == HUMANOID_MODEL_LEFT_ARM_POSITION.x()
        && leftArmPart.y == HUMANOID_MODEL_LEFT_ARM_POSITION.y()
        && leftArmPart.z == HUMANOID_MODEL_LEFT_ARM_POSITION.z()
        && leftArmPart.xRot == MODEL_LEFT_ARM_ROTATION.getX()
        && leftArmPart.yRot == MODEL_LEFT_ARM_ROTATION.getY()
        && leftArmPart.zRot == MODEL_LEFT_ARM_ROTATION.getZ();
  }

  default boolean hasDefaultHumanoidModelRightArm(ModelPart rightArmPart) {
    if (rightArmPart == null) {
      return false;
    }
    return rightArmPart.x == HUMANOID_MODEL_RIGHT_ARM_POSITION.x()
        && rightArmPart.y == HUMANOID_MODEL_RIGHT_ARM_POSITION.y()
        && rightArmPart.z == HUMANOID_MODEL_RIGHT_ARM_POSITION.z()
        && rightArmPart.xRot == MODEL_RIGHT_ARM_ROTATION.getX()
        && rightArmPart.yRot == MODEL_RIGHT_ARM_ROTATION.getY()
        && rightArmPart.zRot == MODEL_RIGHT_ARM_ROTATION.getZ();
  }

  default boolean hasDefaultHumanoidModelLeftLeg(ModelPart leftLegPart) {
    if (leftLegPart == null) {
      return false;
    }
    return leftLegPart.x == HUMANOID_MODEL_LEFT_LEG_POSITION.x()
        && leftLegPart.y == HUMANOID_MODEL_LEFT_LEG_POSITION.y()
        && leftLegPart.z == HUMANOID_MODEL_LEFT_LEG_POSITION.z()
        && leftLegPart.xRot == MODEL_LEFT_LEG_ROTATION.getX()
        && leftLegPart.yRot == MODEL_LEFT_LEG_ROTATION.getY()
        && leftLegPart.zRot == MODEL_LEFT_LEG_ROTATION.getZ();
  }

  default boolean hasDefaultHumanoidModelRightLeg(ModelPart rightLegPart) {
    if (rightLegPart == null) {
      return false;
    }
    return rightLegPart.x == HUMANOID_MODEL_RIGHT_LEG_POSITION.x()
        && rightLegPart.y == HUMANOID_MODEL_RIGHT_LEG_POSITION.y()
        && rightLegPart.z == HUMANOID_MODEL_RIGHT_LEG_POSITION.z()
        && rightLegPart.xRot == MODEL_RIGHT_LEG_ROTATION.getX()
        && rightLegPart.yRot == MODEL_RIGHT_LEG_ROTATION.getY()
        && rightLegPart.zRot == MODEL_RIGHT_LEG_ROTATION.getZ();
  }
}
