/*
 * Copyright 2021 Markus Bordihn
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

package de.markusbordihn.easynpc.client.model.custom;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.base.BaseHumanoidModel;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.PartPose;
import net.minecraft.client.model.geom.builders.CubeDeformation;
import net.minecraft.client.model.geom.builders.CubeListBuilder;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.client.model.geom.builders.MeshDefinition;
import net.minecraft.client.model.geom.builders.PartDefinition;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.LivingEntity;

public class FairyModel<T extends LivingEntity> extends BaseHumanoidModel<T> {

  public static final float MODEL_OFFSET_Y = -1.0F;
  private final ModelPart leftWing;
  private final ModelPart rightWing;

  public FairyModel(ModelPart modelPart) {
    super(modelPart);
    setDefaultModelPartVisibility(ModelPartType.LEFT_LEG, false);
    setDefaultModelPartVisibility(ModelPartType.HAT, false);
    this.leftWing = defineModelPart(ModelPartType.LEFT_WING, modelPart, "left_wing");
    this.rightWing = defineModelPart(ModelPartType.RIGHT_WING, modelPart, "right_wing");
  }

  public static LayerDefinition createBodyLayer() {
    MeshDefinition meshDefinition = HumanoidModel.createMesh(CubeDeformation.NONE, MODEL_OFFSET_Y);
    PartDefinition partDefinition = meshDefinition.getRoot();

    // Head
    partDefinition.addOrReplaceChild(
        "head",
        CubeListBuilder.create()
            .texOffs(32, 25)
            .addBox(-4.0F, -8.0F, -4.0F, 8.0F, 8.0F, 8.0F, new CubeDeformation(0.0F))
            .texOffs(0, 25)
            .addBox(-4.0F, -8.0F, -4.0F, 8.0F, 8.0F, 8.0F, new CubeDeformation(0.5F)),
        PartPose.offset(0.0F, 0.0F + MODEL_OFFSET_Y, 0.0F));

    // Body
    partDefinition.addOrReplaceChild(
        "body",
        CubeListBuilder.create()
            .texOffs(24, 41)
            .addBox(-4.0F, 0.0F, -2.0F, 8.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(0, 41)
            .addBox(-4.0F, 0.0F, -2.0F, 8.0F, 15.0F, 4.0F, new CubeDeformation(0.5F)),
        PartPose.offset(0.0F, 0.0F + MODEL_OFFSET_Y, 0.0F));

    // Smaller arms
    partDefinition.addOrReplaceChild(
        "right_arm",
        CubeListBuilder.create()
            .texOffs(56, 14)
            .addBox(-2.0F, -2.0F, -2.0F, 3.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(0, 60)
            .addBox(-2.0F, -2.0F, -2.0F, 3.0F, 8.0F, 4.0F, new CubeDeformation(0.5F)),
        PartPose.offset(-5.0F, 2.0F + MODEL_OFFSET_Y, 0.0F));
    partDefinition.addOrReplaceChild(
        "left_arm",
        CubeListBuilder.create()
            .texOffs(56, 14)
            .mirror()
            .addBox(-1.0F, -2.0F, -2.0F, 3.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .mirror(false)
            .texOffs(20, 57)
            .addBox(-1.0F, -2.0F, -2.0F, 3.0F, 8.0F, 4.0F, new CubeDeformation(0.5F)),
        PartPose.offset(5.0F, 2.0F + MODEL_OFFSET_Y, 0.0F));

    // Combined legs like Vex
    partDefinition.addOrReplaceChild(
        "right_leg",
        CubeListBuilder.create()
            .texOffs(44, 55)
            .addBox(-2.0F, 0.0F, -2.0F, 4.0F, 12.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(48, 41)
            .addBox(-1.0F, -1.0F, -2.0F, 6.0F, 10.0F, 4.0F, new CubeDeformation(0.0F))
            .texOffs(42, 0)
            .addBox(-1.0F, -1.0F, -2.0F, 6.0F, 10.0F, 4.0F, new CubeDeformation(0.5F)),
        PartPose.offset(-1.9F, 12.0F + MODEL_OFFSET_Y, 0.0F));

    // Adding Wings
    partDefinition.addOrReplaceChild(
        "left_wing",
        CubeListBuilder.create()
            .texOffs(0, 0)
            .mirror()
            .addBox(0.0F, -8.0F, 0.0F, 20.0F, 24.0F, 1.0F, new CubeDeformation(0.0F))
            .mirror(false),
        PartPose.offset(0.0F, 0.0F + MODEL_OFFSET_Y, 0.0F));
    partDefinition.addOrReplaceChild(
        "right_wing",
        CubeListBuilder.create()
            .texOffs(0, 0)
            .addBox(-20.0F, -8.0F, 0.0F, 20.0F, 24.0F, 1.0F, new CubeDeformation(0.0F)),
        PartPose.offset(0.0F, 0.0F + MODEL_OFFSET_Y, 0.0F));

    return LayerDefinition.create(meshDefinition, 128, 128);
  }

  @Override
  protected Iterable<ModelPart> bodyParts() {
    return java.util.List.of(
        this.head,
        this.body,
        this.rightArm,
        this.leftArm,
        this.rightLeg,
        this.leftWing,
        this.rightWing);
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.HAT, this.hat);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_ARM, this.rightArm);
    this.resetModelPart(ModelPartType.LEFT_ARM, this.leftArm);
    this.resetModelPart(ModelPartType.RIGHT_LEG, this.rightLeg);
    this.resetModelPart(ModelPartType.RIGHT_WING, this.rightWing);
    this.resetModelPart(ModelPartType.LEFT_WING, this.leftWing);
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
    this.body.y += 3.2F;
    this.body.z -= 1.2F;
    this.head.y += 4.2F;
    this.leftArm.xRot += 0.4F;
    this.leftArm.y += 5.2F;
    this.rightArm.xRot += 0.4F;
    this.rightArm.y += 5.2F;
    this.rightLeg.y += 4F;
    this.rightLeg.z = 4.4F;
    this.rightWing.y += 4.0F;
    this.leftWing.y += 4.0F;
    return true;
  }

  @Override
  public boolean additionalModelAnimation(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.rightWing.yRot =
        Constants.MATH_27DEG_TO_RAD
            + Mth.cos(ageInTicks * 20F * Constants.PI_180DEG) * (float) Math.PI * 0.15F;
    this.leftWing.yRot = -this.rightWing.yRot;
    this.leftWing.zRot = Constants.MATH_27DEG_TO_RAD_INVERTED;
    this.leftWing.xRot = Constants.MATH_27DEG_TO_RAD;
    this.rightWing.xRot = Constants.MATH_27DEG_TO_RAD;
    this.rightWing.zRot = Constants.MATH_27DEG_TO_RAD;
    return true;
  }

  @Override
  public boolean setupStandingModelPose(
      T entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    super.setupStandingModelPose(
        entity, modelData, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    this.rightLeg.xRot += ((float) Math.PI / 5F);
    return true;
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
        this.rightLeg,
        modelData.getModelRightLegPosition(),
        modelData.getModelRightLegRotation(),
        modelData.isModelRightLegVisible());
  }
}
