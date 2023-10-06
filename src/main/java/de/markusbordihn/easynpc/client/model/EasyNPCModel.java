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

package de.markusbordihn.easynpc.client.model;

import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;

import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

public interface EasyNPCModel {

  // General model positions
  public static final CustomPosition MODEL_ROOT_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_HEAD_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_BODY_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_ARMS_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_LEFT_ARM_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_RIGHT_ARM_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_LEFT_LEG_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_RIGHT_LEG_POSITION = new CustomPosition(0, 0, 0);

  // Animal model position
  public static final CustomPosition MODEL_LEFT_FRONT_LEG_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_RIGHT_FRONT_LEG_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_LEFT_HIND_LEG_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_RIGHT_HIND_LEG_POSITION = new CustomPosition(0, 0, 0);

  // Bird model position
  public static final CustomPosition MODEL_LEFT_WING_POSITION = new CustomPosition(0, 0, 0);
  public static final CustomPosition MODEL_RIGHT_WING_POSITION = new CustomPosition(0, 0, 0);

  // General model rotations
  public static final Rotations MODEL_HEAD_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_BODY_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_ARMS_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_LEFT_ARM_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_RIGHT_ARM_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_LEFT_LEG_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_RIGHT_LEG_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_ROOT_ROTATION = new Rotations(0, 0, 0);

  // Animal model rotations
  public static final Rotations MODEL_LEFT_FRONT_LEG_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_RIGHT_FRONT_LEG_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_LEFT_HIND_LEG_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_RIGHT_HIND_LEG_ROTATION = new Rotations(0, 0, 0);

  // Bird model rotations
  public static final Rotations MODEL_LEFT_WING_ROTATION = new Rotations(0, 0, 0);
  public static final Rotations MODEL_RIGHT_WING_ROTATION = new Rotations(0, 0, 0);

  // Allay Model specific positions
  public static final Rotations ALLAY_MODEL_LEFT_ARM_ROTATION = new Rotations(0, -0.27925268F, 0);
  public static final Rotations ALLAY_MODEL_RIGHT_ARM_ROTATION = new Rotations(0, 0.27925268F, 0);
  public static final CustomPosition ALLAY_MODEL_ROOT_POSITION = new CustomPosition(0, 15f, 0);
  public static final CustomPosition ALLAY_MODEL_LEFT_ARM_POSITION =
      new CustomPosition(2.0F, 0.0F, 0.0F);
  public static final CustomPosition ALLAY_MODEL_RIGHT_ARM_POSITION =
      new CustomPosition(-2.0F, 0.0F, 0.0F);

  // Humanoid Model specific positions
  public static final CustomPosition HUMANOID_MODEL_LEFT_ARM_POSITION =
      new CustomPosition(5f, 2f, 0f);
  public static final CustomPosition HUMANOID_MODEL_RIGHT_ARM_POSITION =
      new CustomPosition(-5f, 2f, 0f);
  public static final CustomPosition HUMANOID_MODEL_LEFT_LEG_POSITION =
      new CustomPosition(1.9f, 12f, 0.1f);
  public static final CustomPosition HUMANOID_MODEL_RIGHT_LEG_POSITION =
      new CustomPosition(-1.9f, 12f, 0.1f);

  // Villager Model specific positions
  public static final CustomPosition VILLAGER_MODEL_ARMS_POSITION =
      new CustomPosition(0.0f, 2.0f, 0.0f);
  public static final CustomPosition VILLAGER_MODEL_LEFT_LEG_POSITION =
      new CustomPosition(1.9f, 12f, 0.1f);
  public static final CustomPosition VILLAGER_MODEL_RIGHT_LEG_POSITION =
      new CustomPosition(-1.9f, 12f, 0.1f);

  // Villager Model specific rotations
  public static final Rotations VILLAGER_MODEL_ARMS_ROTATION = new Rotations(-0.8f, 0.0f, 0.0f);

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

  default void setupCustomModel() {}

  default void setupCustomAnimalModel(EasyNPCEntity entity, ModelPart headPart, ModelPart bodyPart,
      ModelPart leftFrontLeg, ModelPart rightFrontLeg, ModelPart leftHindLeg,
      ModelPart rightHindLeg, float netHeadYaw, float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(headPart, entity.getModelHeadPosition(),
        entity.getModelHeadRotation(), entity.isModelHeadVisible(), netHeadYaw, headPitch);
    CustomModelHelper.setPositionRotationVisibility(bodyPart, entity.getModelBodyPosition(),
        entity.getModelBodyRotation(), entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(leftFrontLeg, entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(), entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(rightFrontLeg,
        entity.getModelRightArmPosition(), entity.getModelRightArmRotation(),
        entity.isModelRightArmVisible());
    CustomModelHelper.setPositionRotationVisibility(leftHindLeg, entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(), entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(rightHindLeg, entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(), entity.isModelRightLegVisible());
  }

  default void setupCustomBirdModel(EasyNPCEntity entity, ModelPart headPart, ModelPart bodyPart,
      ModelPart leftWing, ModelPart rightWing, ModelPart leftLeg, ModelPart rightLeg,
      float netHeadYaw, float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(headPart, entity.getModelHeadPosition(),
        entity.getModelHeadRotation(), entity.isModelHeadVisible(), netHeadYaw, headPitch);
    CustomModelHelper.setPositionRotationVisibility(bodyPart, entity.getModelBodyPosition(),
        entity.getModelBodyRotation(), entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(leftWing, entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(), entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(rightWing, entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(), entity.isModelRightArmVisible());
    CustomModelHelper.setPositionRotationVisibility(leftLeg, entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(), entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(rightLeg, entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(), entity.isModelRightLegVisible());
  }

  default void setupCustomArmModel(EasyNPCEntity entity, ModelPart headPart, ModelPart bodyPart,
      ModelPart leftArmPart, ModelPart rightArmPart, float netHeadYaw, float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(headPart, entity.getModelHeadPosition(),
        entity.getModelHeadRotation(), entity.isModelHeadVisible(), netHeadYaw, headPitch);
    CustomModelHelper.setPositionRotationVisibility(bodyPart, entity.getModelBodyPosition(),
        entity.getModelBodyRotation(), entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(leftArmPart, entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(), entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(rightArmPart, entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(), entity.isModelRightArmVisible());
  }

  default void setupCustomHumanoidModel(EasyNPCEntity entity, ModelPart headPart,
      ModelPart bodyPart, ModelPart rightArmPart, ModelPart leftArmPart, ModelPart rightLegPart,
      ModelPart leftLegPart, float netHeadYaw, float headPitch) {
    CustomModelHelper.setHeadPositionRotationVisibility(headPart, entity.getModelHeadPosition(),
        entity.getModelHeadRotation(), entity.isModelHeadVisible(), netHeadYaw, headPitch);
    CustomModelHelper.setPositionRotationVisibility(bodyPart, entity.getModelBodyPosition(),
        entity.getModelBodyRotation(), entity.isModelBodyVisible());
    CustomModelHelper.setPositionRotationVisibility(leftArmPart, entity.getModelLeftArmPosition(),
        entity.getModelLeftArmRotation(), entity.isModelLeftArmVisible());
    CustomModelHelper.setPositionRotationVisibility(rightArmPart, entity.getModelRightArmPosition(),
        entity.getModelRightArmRotation(), entity.isModelRightArmVisible());
    CustomModelHelper.setPositionRotationVisibility(leftLegPart, entity.getModelLeftLegPosition(),
        entity.getModelLeftLegRotation(), entity.isModelLeftLegVisible());
    CustomModelHelper.setPositionRotationVisibility(rightLegPart, entity.getModelRightLegPosition(),
        entity.getModelRightLegRotation(), entity.isModelRightLegVisible());
  }

  default void resetModel() {}

  default void resetAnimalModel(ModelPart headPart, ModelPart bodyPart, ModelPart leftFrontLeg,
      ModelPart rightFrontLeg, ModelPart leftHindLeg, ModelPart rightHindLeg) {

    // Reset all rotations.
    setModelPartRotation(headPart, this.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, this.getDefaultModelBodyRotation());
    setModelPartRotation(leftFrontLeg, this.getDefaultModelLeftFrontLegRotation());
    setModelPartRotation(rightFrontLeg, this.getDefaultModelRightFrontLegRotation());
    setModelPartRotation(leftHindLeg, this.getDefaultModelLeftHindLegRotation());
    setModelPartRotation(rightHindLeg, this.getDefaultModelRightHindLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, this.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, this.getDefaultModelHeadPosition());
    setModelPartPosition(leftFrontLeg, this.getDefaultModelLeftFrontLegPosition());
    setModelPartPosition(rightFrontLeg, this.getDefaultModelRightFrontLegPosition());
    setModelPartPosition(leftHindLeg, this.getDefaultModelLeftHindLegPosition());
    setModelPartPosition(rightHindLeg, this.getDefaultModelRightHindLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftFrontLeg.visible = true;
    rightFrontLeg.visible = true;
    leftHindLeg.visible = true;
    rightHindLeg.visible = true;
  }

  default void resetArmModel(ModelPart headPart, ModelPart bodyPart, ModelPart leftArm,
      ModelPart rightArm) {

    // Reset all rotations.
    setModelPartRotation(headPart, this.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, this.getDefaultModelBodyRotation());
    setModelPartRotation(leftArm, this.getDefaultModelLeftArmRotation());
    setModelPartRotation(rightArm, this.getDefaultModelRightArmRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, this.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, this.getDefaultModelHeadPosition());
    setModelPartPosition(leftArm, this.getDefaultModelLeftArmPosition());
    setModelPartPosition(rightArm, this.getDefaultModelRightArmPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftArm.visible = true;
    rightArm.visible = true;
  }

  default void resetBirdModel(ModelPart headPart, ModelPart bodyPart, ModelPart leftWingPart,
      ModelPart rightWingPart, ModelPart leftLegPart, ModelPart rightLegPart) {

    // Reset all rotations.
    setModelPartRotation(headPart, this.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, this.getDefaultModelBodyRotation());
    setModelPartRotation(leftWingPart, this.getDefaultModelLeftWingRotation());
    setModelPartRotation(rightWingPart, this.getDefaultModelRightArmRotation());
    setModelPartRotation(leftLegPart, this.getDefaultModelLeftLegRotation());
    setModelPartRotation(rightLegPart, this.getDefaultModelRightLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, this.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, this.getDefaultModelHeadPosition());
    setModelPartPosition(leftWingPart, this.getDefaultModelLeftWingPosition());
    setModelPartPosition(rightWingPart, this.getDefaultModelRightWingPosition());
    setModelPartPosition(leftLegPart, this.getDefaultModelLeftLegPosition());
    setModelPartPosition(rightLegPart, this.getDefaultModelRightLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftWingPart.visible = true;
    rightWingPart.visible = true;
    leftLegPart.visible = true;
    rightLegPart.visible = true;
  }

  default void resetHumanoidModel(ModelPart headPart, ModelPart bodyPart, ModelPart rightArmPart,
      ModelPart leftArmPart, ModelPart rightLegPart, ModelPart leftLegPart) {

    // Reset all rotations.
    setModelPartRotation(headPart, this.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, this.getDefaultModelBodyRotation());
    setModelPartRotation(leftArmPart, this.getDefaultModelLeftArmRotation());
    setModelPartRotation(rightArmPart, this.getDefaultModelRightArmRotation());
    setModelPartRotation(leftLegPart, this.getDefaultModelLeftLegRotation());
    setModelPartRotation(rightLegPart, this.getDefaultModelRightLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, this.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, this.getDefaultModelHeadPosition());
    setModelPartPosition(leftArmPart, this.getDefaultModelLeftArmPosition());
    setModelPartPosition(rightArmPart, this.getDefaultModelRightArmPosition());
    setModelPartPosition(leftLegPart, this.getDefaultModelLeftLegPosition());
    setModelPartPosition(rightLegPart, this.getDefaultModelRightLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    leftArmPart.visible = true;
    rightArmPart.visible = true;
    leftLegPart.visible = true;
    rightLegPart.visible = true;
  }

  default void resetVillagerModel(ModelPart headPart, ModelPart bodyPart, ModelPart armsPart,
      ModelPart leftLegPart, ModelPart rightLegPart) {
    // Reset all rotations.
    setModelPartRotation(headPart, this.getDefaultModelHeadRotation());
    setModelPartRotation(bodyPart, this.getDefaultModelBodyRotation());
    setModelPartRotation(armsPart, this.getDefaultModelArmsRotation());
    setModelPartRotation(leftLegPart, this.getDefaultModelLeftLegRotation());
    setModelPartRotation(rightLegPart, this.getDefaultModelRightLegRotation());

    // Reset all positions.
    setModelPartPosition(bodyPart, this.getDefaultModelBodyPosition());
    setModelPartPosition(headPart, this.getDefaultModelHeadPosition());
    setModelPartPosition(armsPart, this.getDefaultModelArmsPosition());
    setModelPartPosition(leftLegPart, this.getDefaultModelLeftLegPosition());
    setModelPartPosition(rightLegPart, this.getDefaultModelRightLegPosition());

    // Reset all visibility.
    headPart.visible = true;
    bodyPart.visible = true;
    armsPart.visible = true;
    leftLegPart.visible = true;
    rightLegPart.visible = true;
  }

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

}
