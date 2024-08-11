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

import de.markusbordihn.easynpc.client.model.animation.HumanoidLegAnimation;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.Map;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;

public interface EasyNPCModel<E extends Entity> {
  CustomPosition EMPTY_POSITION = new CustomPosition(0, 0, 0);
  CustomRotation EMPTY_ROTATION = new CustomRotation(0, 0, 0);

  private static boolean equalPositionAndRotation(
      ModelPart modelPart, CustomPosition position, CustomRotation rotation) {
    return equalPosition(modelPart, position) && equalRotation(modelPart, rotation);
  }

  private static boolean equalPosition(ModelPart modelPart, CustomPosition position) {
    return modelPart != null
        && Math.abs(modelPart.x - position.x()) < 0.01
        && Math.abs(modelPart.y - position.y()) < 0.01
        && Math.abs(modelPart.z - position.z()) < 0.01;
  }

  private static boolean equalRotation(ModelPart modelPart, CustomRotation rotation) {
    return modelPart != null
        && Math.abs(modelPart.xRot - rotation.x()) < 0.01
        && Math.abs(modelPart.yRot - rotation.y()) < 0.01
        && Math.abs(modelPart.zRot - rotation.z()) < 0.01;
  }

  default void resetModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    CustomPosition defaultPosition = getDefaultModelPartPosition(modelPartType);
    modelPart.x = defaultPosition.x();
    modelPart.y = defaultPosition.y();
    modelPart.z = defaultPosition.z();
    CustomRotation defaultRotation = getDefaultModelPartRotation(modelPartType);
    modelPart.xRot = defaultRotation.x();
    modelPart.yRot = defaultRotation.y();
    modelPart.zRot = defaultRotation.z();
  }

  default boolean isHumanoidModel() {
    return true;
  }

  default boolean hasDefaultModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    CustomPosition defaultPosition = getDefaultModelPartPosition(modelPartType);
    CustomRotation defaultRotation = getDefaultModelPartRotation(modelPartType);
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
        modelPartType, new CustomRotation(modelPart.xRot, modelPart.yRot, modelPart.zRot));
    setDefaultModelPart(modelPartType, modelPart);
    return modelPart;
  }

  void resetModelParts();

  Map<ModelPartType, CustomPosition> getModelPartPositionMap();

  Map<ModelPartType, CustomRotation> getModelPartRotationMap();

  Map<ModelPartType, ModelPart> getModelPartMap();

  default void setDefaultModelPartPosition(
      final ModelPartType modelPartType, final CustomPosition customPosition) {
    this.getModelPartPositionMap().put(modelPartType, customPosition);
  }

  default void setDefaultModelPartRotation(
      final ModelPartType modelPartType, final CustomRotation rotation) {
    this.getModelPartRotationMap().put(modelPartType, rotation);
  }

  default void setDefaultModelPart(final ModelPartType modelPartType, final ModelPart modelPart) {
    this.getModelPartMap().put(modelPartType, modelPart);
  }

  default CustomPosition getDefaultModelPartPosition(final ModelPartType modelPartType) {
    return this.getModelPartPositionMap().getOrDefault(modelPartType, EMPTY_POSITION);
  }

  default CustomRotation getDefaultModelPartRotation(final ModelPartType modelPartType) {
    return this.getModelPartRotationMap().getOrDefault(modelPartType, EMPTY_ROTATION);
  }

  default ModelPart getDefaultModelPart(final ModelPartType modelPartType) {
    return this.getModelPartMap().getOrDefault(modelPartType, null);
  }

  default void setupCustomModelPose(
      E entity,
      ModelPose modelPose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {}

  default void animateCustomModelPose(
      E entity,
      ModelPose modelPose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {}

  default void adjustDefaultModelParts(E entity, EasyNPC<?> easyNPC) {}

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

  default boolean animateModelHead(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart headPart,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return false;
  }

  default boolean animateModelBody(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart bodyPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
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

  default boolean animateModelLegs(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    if (rightLegPart == null || leftLegPart == null) {
      return false;
    }
    return HumanoidLegAnimation.animateHumanoidModelLegs(
        rightLegPart, leftLegPart, limbSwing, limbSwingAmount);
  }

  default boolean animateModelFrontLegs(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    return false;
  }

  default boolean animateModelHindLegs(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    return false;
  }

  default void animateAttackModelPose(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {}

  default boolean handleDefaultModelPose(
      E entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    Pose pose = modelData.getDefaultPose();
    boolean hasAdjustedDefaultModelPose =
        this.setupDefaultModelPose(
            entity, pose, modelData, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);

    // Handle default standard model poses.
    hasAdjustedDefaultModelPose =
        switch (pose) {
          case STANDING ->
              hasAdjustedDefaultModelPose
                  || this.setupStandingModelPose(
                      entity,
                      modelData,
                      limbSwing,
                      limbSwingAmount,
                      ageInTicks,
                      netHeadYaw,
                      headPitch);
          case CROUCHING ->
              hasAdjustedDefaultModelPose
                  || this.setupCrouchingModelPose(
                      entity,
                      modelData,
                      limbSwing,
                      limbSwingAmount,
                      ageInTicks,
                      netHeadYaw,
                      headPitch);
          default ->
              hasAdjustedDefaultModelPose
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
    boolean isHumanoidModel = this.isHumanoidModel();
    boolean hasAnimatedModelBody = false;
    boolean hasAnimatedModelHead = false;
    boolean hasAnimatedModelArms = false;
    boolean hasAnimatedModelArmPose = false;
    boolean hasAnimatedModelLegs = false;
    boolean hasAnimatedModelFrontLegs = false;
    boolean hasAnimatedModelHindLegs = false;

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
    boolean hasDefaultHead = head != null && this.hasDefaultModelPart(ModelPartType.HEAD, head);
    if (hasDefaultHead) {
      hasAnimatedModelHead =
          this.animateModelHead(
              entity, attackData, modelData, head, ageInTicks, netHeadYaw, headPitch);
    }

    // Animate model body.
    ModelPart body = this.getDefaultModelPart(ModelPartType.BODY);
    boolean hasDefaultBody = body != null && this.hasDefaultModelPart(ModelPartType.BODY, body);
    if (hasDefaultBody) {
      hasAnimatedModelBody =
          this.animateModelBody(
              entity, attackData, modelData, body, ageInTicks, limbSwing, limbSwingAmount);
    }

    if (isHumanoidModel) {
      // Animate model arms and model arm pose.
      ModelArmPose modelArmPose = modelData.getModelArmPose();
      if (modelArmPose == ModelArmPose.DEFAULT || modelArmPose == ModelArmPose.NEUTRAL) {
        ModelPart rightArm = this.getDefaultModelPart(ModelPartType.RIGHT_ARM);
        boolean hasDefaultRightArm =
            rightArm != null && this.hasDefaultModelPart(ModelPartType.RIGHT_ARM, rightArm);
        ModelPart leftArm = this.getDefaultModelPart(ModelPartType.LEFT_ARM);
        boolean hasDefaultLeftArm =
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
      ModelPart leftLeg = this.getDefaultModelPart(ModelPartType.LEFT_LEG);
      hasAnimatedModelLegs =
          this.animateModelLegs(
              entity,
              attackData,
              modelData,
              rightLeg != null && this.hasDefaultModelPart(ModelPartType.RIGHT_LEG, rightLeg)
                  ? rightLeg
                  : null,
              leftLeg != null && this.hasDefaultModelPart(ModelPartType.LEFT_LEG, leftLeg)
                  ? leftLeg
                  : null,
              ageInTicks,
              limbSwing,
              limbSwingAmount);
    } else {
      // Animate model front legs.
      ModelPart rightFrontLeg = this.getDefaultModelPart(ModelPartType.RIGHT_FRONT_LEG);
      ModelPart leftFrontLeg = this.getDefaultModelPart(ModelPartType.LEFT_FRONT_LEG);
      hasAnimatedModelFrontLegs =
          this.animateModelFrontLegs(
              entity,
              attackData,
              modelData,
              rightFrontLeg != null
                      && this.hasDefaultModelPart(ModelPartType.RIGHT_FRONT_LEG, rightFrontLeg)
                  ? rightFrontLeg
                  : null,
              leftFrontLeg != null
                      && this.hasDefaultModelPart(ModelPartType.LEFT_FRONT_LEG, leftFrontLeg)
                  ? leftFrontLeg
                  : null,
              ageInTicks,
              limbSwing,
              limbSwingAmount);

      // Animate model hind legs.
      ModelPart rightHindLeg = this.getDefaultModelPart(ModelPartType.RIGHT_HIND_LEG);
      ModelPart leftHindLeg = this.getDefaultModelPart(ModelPartType.LEFT_HIND_LEG);
      hasAnimatedModelHindLegs =
          this.animateModelHindLegs(
              entity,
              attackData,
              modelData,
              rightHindLeg != null
                      && this.hasDefaultModelPart(ModelPartType.RIGHT_HIND_LEG, rightHindLeg)
                  ? rightHindLeg
                  : null,
              leftHindLeg != null
                      && this.hasDefaultModelPart(ModelPartType.LEFT_HIND_LEG, leftHindLeg)
                  ? leftHindLeg
                  : null,
              ageInTicks,
              limbSwing,
              limbSwingAmount);
    }

    return !(!hasAnimatedModelHead
        && !hasAnimatedModelBody
        && !hasAnimatedModelArms
        && !hasAnimatedModelArmPose
        && !hasAnimatedModelLegs
        && !hasAnimatedModelFrontLegs
        && !hasAnimatedModelHindLegs
        && hasDefaultHead);
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
    boolean hasAdditionalModelAnimation =
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
    this.adjustDefaultModelParts(entity, easyNPC);

    return isCustomModelPose
        || (isDefaultModelPose && hasAdjustedDefaultModelPose)
        || hasSmartAnimations
        || hasAdditionalModelAnimation;
  }
}
