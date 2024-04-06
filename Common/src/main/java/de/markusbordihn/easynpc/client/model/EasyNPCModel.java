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
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;

public interface EasyNPCModel<E extends Entity> {

  CustomRotation EMPTY_ROTATION = new CustomRotation(0, 0, 0);
  CustomPosition EMPTY_POSITION = new CustomPosition(0, 0, 0);

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

  default void resetModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    CustomPosition defaultPosition = getDefaultModelPartPosition(modelPartType);
    modelPart.x = defaultPosition.x();
    modelPart.y = defaultPosition.y();
    modelPart.z = defaultPosition.z();
    Rotations defaultRotation = getDefaultModelPartRotation(modelPartType);
    modelPart.xRot = defaultRotation.getX();
    modelPart.yRot = defaultRotation.getY();
    modelPart.zRot = defaultRotation.getZ();
  }

  default boolean hasDefaultModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    CustomPosition defaultPosition = getDefaultModelPartPosition(modelPartType);
    Rotations defaultRotation = getDefaultModelPartRotation(modelPartType);
    return equalPositionAndRotation(modelPart, defaultPosition, defaultRotation);
  }

  default ModelPart defineModelPart(
      ModelPartType modelPartType, ModelPart parentModel, String name) {
    ModelPart modelPart = parentModel.getChild(name);
    return defineModelPart(modelPartType, modelPart);
  }

  default ModelPart defineModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    setDefaultModelPartPosition(
        modelPartType, new CustomPosition(modelPart.x, modelPart.y, modelPart.z));
    setDefaultModelPartRotation(
        modelPartType, new Rotations(modelPart.xRot, modelPart.yRot, modelPart.zRot));
    setDefaultModelPart(modelPartType, modelPart);
    return modelPart;
  }

  void resetModelParts();

  void setDefaultModelPartPosition(ModelPartType modelPartType, CustomPosition customPosition);

  CustomPosition getDefaultModelPartPosition(ModelPartType modelPartType);

  void setDefaultModelPartRotation(ModelPartType modelPartType, Rotations rotations);

  Rotations getDefaultModelPartRotation(ModelPartType modelPartType);

  void setDefaultModelPart(ModelPartType modelPartType, ModelPart modelPart);

  ModelPart getDefaultModelPart(ModelPartType modelPartType);

  default void setupCustomModelPose(
      E entity,
      ModelPose modelPose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
  }

  default void animateCustomModelPose(
      E entity,
      ModelPose modelPose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
  }

  default void adjustDefaultModelParts(E entity) {
  }

  default boolean setupDefaultModelPose(
      E entity,
      Pose pose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean animateDefaultModelPose(
      E entity,
      Pose pose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean setupStandingModelPose(
      E entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean setupCrouchingModelPose(
      E entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean setupFallbackModelPose(
      E entity,
      Pose pose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean additionalModelAnimation(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean animateModelArmPose(
      E entity,
      ModelArmPose modelArmPose,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean animateModelArms(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    boolean result = false;
    if (rightArmPart != null) {
      result =
          this.animateModelRightArm(
              entity, attackData, modelData, rightArmPart, ageInTicks, limbSwing, limbSwingAmount);
    }
    if (leftArmPart != null) {
      result =
          this.animateModelLeftArm(
              entity, attackData, modelData, leftArmPart, ageInTicks, limbSwing, limbSwingAmount);
    }
    return result;
  }

  default boolean animateModelRightArm(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    return false;
  }

  default boolean animateModelLeftArm(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart leftArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    return false;
  }

  default void animateModelLegs(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
  }

  default void animateModelHead(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart headPart,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
  }

  default void animateAttackModelPose(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
  }

  default boolean handleDefaultModelPose(
      E entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    boolean hasAdjustedDefaultModelPose = false;
    Pose pose = modelData.getDefaultPose();
    hasAdjustedDefaultModelPose =
        this.setupDefaultModelPose(
            entity, pose, modelData, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);

    // Handle default standard model poses.
    hasAdjustedDefaultModelPose =
        switch (pose) {
          case STANDING -> hasAdjustedDefaultModelPose
              || this.setupStandingModelPose(
              entity,
              modelData,
              limbSwing,
              limbSwingAmount,
              ageInTicks,
              netHeadYaw,
              headPitch);
          case CROUCHING -> hasAdjustedDefaultModelPose
              || this.setupCrouchingModelPose(
              entity,
              modelData,
              limbSwing,
              limbSwingAmount,
              ageInTicks,
              netHeadYaw,
              headPitch);
          default -> hasAdjustedDefaultModelPose
              || this.setupFallbackModelPose(
              entity,
              pose,
              modelData,
              limbSwing,
              limbSwingAmount,
              ageInTicks,
              netHeadYaw,
              headPitch);
        };
    hasAdjustedDefaultModelPose =
        hasAdjustedDefaultModelPose
            || this.animateDefaultModelPose(
            entity,
            pose,
            modelData,
            limbSwing,
            limbSwingAmount,
            ageInTicks,
            netHeadYaw,
            headPitch);
    return hasAdjustedDefaultModelPose;
  }

  default boolean handleSmartAnimations(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {

    // Track default model parts.
    boolean hasDefaultHead = true;
    boolean hasAnimatedModelHead = false;
    boolean hasAnimatedModelArms = false;
    boolean hasAnimatedModelArmPose = false;
    boolean hasAnimatedModelLegs = false;
    boolean hasDefaultRightArm = true;
    boolean hasDefaultLeftArm = true;
    boolean hasDefaultRightLeg;
    boolean hasDefaultLeftLeg;

    // Handle attack related model animation.
    this.animateAttackModelPose(
        entity,
        attackData,
        modelData,
        limbSwing,
        limbSwingAmount,
        ageInTicks,
        netHeadYaw,
        headPitch);

    // Animate model head.
    ModelPart head = this.getDefaultModelPart(ModelPartType.HEAD);
    hasDefaultHead = head != null && this.hasDefaultModelPart(ModelPartType.HEAD, head);
    if (hasDefaultHead) {
      this.animateModelHead(entity, attackData, modelData, head, ageInTicks, netHeadYaw, headPitch);
    }

    // Animate model arms and model arm pose.
    ModelArmPose modelArmPose = modelData.getModelArmPose();
    if (modelArmPose == ModelArmPose.DEFAULT || modelArmPose == ModelArmPose.NEUTRAL) {
      ModelPart rightArm = this.getDefaultModelPart(ModelPartType.RIGHT_ARM);
      hasDefaultRightArm =
          rightArm != null && this.hasDefaultModelPart(ModelPartType.RIGHT_ARM, rightArm);
      ModelPart leftArm = this.getDefaultModelPart(ModelPartType.LEFT_ARM);
      hasDefaultLeftArm =
          leftArm != null && this.hasDefaultModelPart(ModelPartType.LEFT_ARM, leftArm);
      hasAnimatedModelArms =
          this.animateModelArms(
              entity,
              attackData,
              modelData,
              hasDefaultRightArm ? rightArm : null,
              hasDefaultLeftArm ? leftArm : null,
              ageInTicks,
              limbSwing,
              limbSwingAmount);
    } else {
      hasAnimatedModelArmPose =
          this.animateModelArmPose(
              entity,
              modelArmPose,
              attackData,
              modelData,
              limbSwing,
              limbSwingAmount,
              ageInTicks,
              netHeadYaw,
              headPitch);
    }

    // Animate model legs.
    ModelPart rightLeg = this.getDefaultModelPart(ModelPartType.RIGHT_LEG);
    hasDefaultRightLeg =
        rightLeg != null && this.hasDefaultModelPart(ModelPartType.RIGHT_LEG, rightLeg);
    ModelPart leftLeg = this.getDefaultModelPart(ModelPartType.LEFT_LEG);
    hasDefaultLeftLeg =
        leftLeg != null && this.hasDefaultModelPart(ModelPartType.LEFT_LEG, leftLeg);
    this.animateModelLegs(
        entity,
        attackData,
        modelData,
        hasDefaultRightLeg ? rightLeg : null,
        hasDefaultLeftLeg ? leftLeg : null,
        ageInTicks,
        limbSwing,
        limbSwingAmount);

    return !(hasDefaultHead
        && !hasAnimatedModelArms
        && !hasAnimatedModelArmPose
        && hasDefaultRightArm
        && hasDefaultLeftArm
        && hasDefaultRightLeg
        && hasDefaultLeftLeg);
  }

  default boolean setupAnimation(
      E entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return false;
    }

    // Reset model parts to avoid any issues with other modifications.
    this.resetModelParts();

    // Get model data and pose.
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    ModelPose modelPose = modelData.getModelPose();
    AttackData<?> attackData = easyNPC.getEasyNPCAttackData();

    // Track model adjustments and animations.
    boolean isCustomModelPose = modelPose == ModelPose.CUSTOM;
    boolean isDefaultModelPose = modelPose == ModelPose.DEFAULT;
    boolean hasAdjustedDefaultModelPose = false;
    boolean hasSmartAnimations = modelData.useSmartAnimations();
    boolean hasAdditionalModelAnimation = false;

    if (isCustomModelPose) {
      // Handle custom model pose and animation.
      this.setupCustomModelPose(
          entity,
          modelPose,
          modelData,
          limbSwing,
          limbSwingAmount,
          ageInTicks,
          netHeadYaw,
          headPitch);
      this.animateCustomModelPose(
          entity,
          modelPose,
          modelData,
          limbSwing,
          limbSwingAmount,
          ageInTicks,
          netHeadYaw,
          headPitch);
    } else if (isDefaultModelPose) {
      hasAdjustedDefaultModelPose =
          this.handleDefaultModelPose(
              entity, modelData, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    }

    // Use smart animations for better performance, if enabled.
    if (modelData.useSmartAnimations() || isCustomModelPose || hasAdjustedDefaultModelPose) {
      hasSmartAnimations =
          this.handleSmartAnimations(
              entity,
              attackData,
              modelData,
              limbSwing,
              limbSwingAmount,
              ageInTicks,
              netHeadYaw,
              headPitch);
    }

    // Handle additional model animation.
    hasAdditionalModelAnimation =
        this.additionalModelAnimation(
            entity,
            attackData,
            modelData,
            limbSwing,
            limbSwingAmount,
            ageInTicks,
            netHeadYaw,
            headPitch);

    // Handle additional model adjustments parts.
    this.adjustDefaultModelParts(entity);

    return isCustomModelPose
        || (isDefaultModelPose && hasAdjustedDefaultModelPose)
        || hasSmartAnimations
        || hasAdditionalModelAnimation;
  }
}
