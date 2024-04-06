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

package de.markusbordihn.easynpc.client.model.standard;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.animation.HumanoidArmAnimation;
import de.markusbordihn.easynpc.client.model.animation.HumanoidArmPoseAnimation;
import de.markusbordihn.easynpc.client.model.animation.HumanoidHeadAnimation;
import de.markusbordihn.easynpc.client.model.animation.HumanoidLegAnimation;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;

public class StandardPlayerModel<T extends LivingEntity> extends PlayerModel<T>
    implements EasyNPCModel<T>,
    HumanoidArmAnimation,
    HumanoidArmPoseAnimation,
    HumanoidHeadAnimation,
    HumanoidLegAnimation {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  public StandardPlayerModel(ModelPart modelPart, boolean slim) {
    super(modelPart, slim);
    defineModelPart(ModelPartType.HEAD, modelPart, "head");
    defineModelPart(ModelPartType.HAT, modelPart, "hat");
    defineModelPart(ModelPartType.BODY, modelPart, "body");
    defineModelPart(ModelPartType.RIGHT_ARM, modelPart, "right_arm");
    defineModelPart(ModelPartType.LEFT_ARM, modelPart, "left_arm");
    defineModelPart(ModelPartType.RIGHT_LEG, modelPart, "right_leg");
    defineModelPart(ModelPartType.LEFT_LEG, modelPart, "left_leg");
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.HAT, this.hat);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_ARM, this.rightArm);
    this.resetModelPart(ModelPartType.LEFT_ARM, this.leftArm);
    this.resetModelPart(ModelPartType.RIGHT_LEG, this.rightLeg);
    this.resetModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (!this.setupAnimation(
        entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch)) {
      super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    }
  }

  @Override
  public void adjustDefaultModelParts(T entity) {
    this.hat.copyFrom(this.head);
    this.leftPants.copyFrom(this.leftLeg);
    this.rightPants.copyFrom(this.rightLeg);
    this.leftSleeve.copyFrom(this.leftArm);
    this.rightSleeve.copyFrom(this.rightArm);
    this.jacket.copyFrom(this.body);
  }

  @Override
  public void setupCustomModelPose(
      T entity,
      ModelPose modelPose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    ModelHelper.setPositionRotationVisibility(
        this.head,
        modelData.getModelHeadPosition(),
        modelData.getModelHeadRotation(),
        modelData.isModelHeadVisible());
    ModelHelper.setPositionRotationVisibility(
        this.body,
        modelData.getModelBodyPosition(),
        modelData.getModelBodyRotation(),
        modelData.isModelBodyVisible());
    ModelHelper.setPositionRotationVisibility(
        this.leftArm,
        modelData.getModelLeftArmPosition(),
        modelData.getModelLeftArmRotation(),
        modelData.isModelLeftArmVisible());
    ModelHelper.setPositionRotationVisibility(
        this.rightArm,
        modelData.getModelRightArmPosition(),
        modelData.getModelRightArmRotation(),
        modelData.isModelRightArmVisible());
    ModelHelper.setPositionRotationVisibility(
        this.leftLeg,
        modelData.getModelLeftLegPosition(),
        modelData.getModelLeftLegRotation(),
        modelData.isModelLeftLegVisible());
    ModelHelper.setPositionRotationVisibility(
        this.rightLeg,
        modelData.getModelRightLegPosition(),
        modelData.getModelRightLegRotation(),
        modelData.isModelRightLegVisible());
  }

  @Override
  public void animateModelHead(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart headPart,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    HumanoidHeadAnimation.animateHumanoidModelHead(headPart, netHeadYaw, headPitch);
  }

  @Override
  public boolean animateModelArms(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    return HumanoidArmAnimation.animateHumanoidModelArms(
        rightArmPart, leftArmPart, ageInTicks, limbSwing, limbSwingAmount);
  }

  @Override
  public boolean animateModelArmPose(
      T entity,
      ModelArmPose modelArmPose,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return HumanoidArmPoseAnimation.animateHumanoidArmPose(
        (Mob) entity,
        modelArmPose,
        this.head,
        this.body,
        this.rightArm,
        this.leftArm,
        attackTime,
        ageInTicks);
  }

  @Override
  public void animateModelLegs(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    HumanoidLegAnimation.animateHumanoidModelLegs(
        rightLegPart, leftLegPart, limbSwing, limbSwingAmount);
  }

  @Override
  public boolean setupCrouchingModelPose(
      T entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.body.xRot = 0.5F;
    this.body.y = 3.2F;
    this.head.y = 4.2F;
    this.leftArm.xRot += 0.4F;
    this.leftArm.y = 5.2F;
    this.leftLeg.y = 12.2F;
    this.leftLeg.z = 4.0F;
    this.rightArm.xRot += 0.4F;
    this.rightArm.y = 5.2F;
    this.rightLeg.y = 12.2F;
    this.rightLeg.z = 4.0F;
    return true;
  }

  @Override
  public void setDefaultModelPartPosition(
      ModelPartType modelPartType, CustomPosition customPosition) {
    this.modelPartPositionMap.put(modelPartType, customPosition);
  }

  @Override
  public void setDefaultModelPartRotation(ModelPartType modelPartType, Rotations rotations) {
    this.modelPartRotationMap.put(modelPartType, rotations);
  }

  @Override
  public void setDefaultModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    this.modelPartMap.put(modelPartType, modelPart);
  }

  @Override
  public CustomPosition getDefaultModelPartPosition(ModelPartType modelPartType) {
    return this.modelPartPositionMap.getOrDefault(modelPartType, EMPTY_POSITION);
  }

  @Override
  public Rotations getDefaultModelPartRotation(ModelPartType modelPartType) {
    return this.modelPartRotationMap.getOrDefault(modelPartType, EMPTY_ROTATION);
  }

  @Override
  public ModelPart getDefaultModelPart(ModelPartType modelPartType) {
    return this.modelPartMap.getOrDefault(modelPartType, null);
  }
}
