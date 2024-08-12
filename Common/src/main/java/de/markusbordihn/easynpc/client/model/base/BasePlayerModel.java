package de.markusbordihn.easynpc.client.model.base;

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
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.PlayerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;

public class BasePlayerModel<T extends LivingEntity> extends PlayerModel<T>
    implements EasyNPCModel<T>,
        HumanoidArmAnimation,
        HumanoidArmPoseAnimation,
        HumanoidHeadAnimation,
        HumanoidLegAnimation {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, CustomRotation> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  public BasePlayerModel(ModelPart modelPart) {
    this(modelPart, false);
  }

  public BasePlayerModel(ModelPart modelPart, boolean slim) {
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
  public void adjustDefaultModelParts(T entity, EasyNPC<?> easyNPC) {
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
  public boolean animateModelHead(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart headPart,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    return HumanoidHeadAnimation.animateHumanoidModelHead(headPart, netHeadYaw, headPitch);
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
  public Map<ModelPartType, CustomPosition> getModelPartPositionMap() {
    return this.modelPartPositionMap;
  }

  @Override
  public Map<ModelPartType, CustomRotation> getModelPartRotationMap() {
    return this.modelPartRotationMap;
  }

  @Override
  public Map<ModelPartType, ModelPart> getModelPartMap() {
    return this.modelPartMap;
  }
}
