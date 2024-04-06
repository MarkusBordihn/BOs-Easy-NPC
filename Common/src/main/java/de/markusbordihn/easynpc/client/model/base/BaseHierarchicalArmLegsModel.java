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

package de.markusbordihn.easynpc.client.model.base;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.Mob;

public class BaseHierarchicalArmLegsModel<E extends Mob> extends HierarchicalModel<E>
    implements ArmedModel, EasyNPCModel<E> {

  public final ModelPart head;
  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);
  protected final ModelPart root;
  protected final ModelPart body;
  protected final ModelPart rightArm;
  protected final ModelPart leftArm;
  protected final ModelPart rightLeg;
  protected final ModelPart leftLeg;

  public BaseHierarchicalArmLegsModel(ModelPart modelPart) {
    this.root =
        ModelHelper.hasModelPart(modelPart, "root")
            ? defineModelPart(ModelPartType.ROOT, modelPart, "root")
            : defineModelPart(ModelPartType.ROOT, modelPart);
    this.head =
        ModelHelper.hasModelPart(modelPart, "head")
            ? defineModelPart(ModelPartType.HEAD, modelPart, "head")
            : defineModelPart(ModelPartType.HEAD, this.root, "head");
    this.body =
        ModelHelper.hasModelPart(modelPart, "body")
            ? defineModelPart(ModelPartType.BODY, modelPart, "body")
            : defineModelPart(ModelPartType.BODY, this.root, "body");
    this.rightArm =
        ModelHelper.hasModelPart(modelPart, "right_arm")
            ? defineModelPart(ModelPartType.RIGHT_ARM, modelPart, "right_arm")
            : defineModelPart(ModelPartType.RIGHT_ARM, this.body, "right_arm");
    this.leftArm =
        ModelHelper.hasModelPart(modelPart, "left_arm")
            ? defineModelPart(ModelPartType.LEFT_ARM, modelPart, "left_arm")
            : defineModelPart(ModelPartType.LEFT_ARM, this.body, "left_arm");
    this.rightLeg =
        ModelHelper.hasModelPart(modelPart, "right_leg")
            ? defineModelPart(ModelPartType.RIGHT_LEG, modelPart, "right_leg")
            : defineModelPart(ModelPartType.RIGHT_LEG, this.body, "right_leg");
    this.leftLeg =
        ModelHelper.hasModelPart(modelPart, "left_leg")
            ? defineModelPart(ModelPartType.LEFT_LEG, modelPart, "left_leg")
            : defineModelPart(ModelPartType.LEFT_LEG, this.body, "left_leg");
  }

  public ModelPart root() {
    return this.root;
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_ARM, this.rightArm);
    this.resetModelPart(ModelPartType.LEFT_ARM, this.leftArm);
    this.resetModelPart(ModelPartType.RIGHT_LEG, this.rightLeg);
    this.resetModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
  }

  @Override
  public void setupAnim(
      E entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.setupAnimation(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
  }

  @Override
  public void animateModelHead(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart headPart,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    headPart.xRot = headPitch * Constants.PI_180DEG;
    headPart.yRot = netHeadYaw * Constants.PI_180DEG;
  }

  @Override
  public boolean animateModelRightArm(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    AnimationUtils.bobModelPart(rightArmPart, ageInTicks, 1.0F);
    return true;
  }

  @Override
  public boolean animateModelLeftArm(
      E entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart leftArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    AnimationUtils.bobModelPart(leftArmPart, ageInTicks, -1.0F);
    return true;
  }

  @Override
  public void setupCustomModelPose(
      E entity,
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
  public void animateCustomModelPose(
      E entity,
      ModelPose modelPose,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {}

  @Override
  public boolean setupCrouchingModelPose(
      E entity,
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

  protected ModelPart getArm(HumanoidArm humanoidArm) {
    return humanoidArm == HumanoidArm.LEFT ? this.leftArm : this.rightArm;
  }

  @Override
  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    float xTranslation = humanoidArm == HumanoidArm.RIGHT ? -10.0F : 10.0F;
    float yTranslation = humanoidArm == HumanoidArm.RIGHT ? -16.0F : 16.0F;
    ModelPart modelpart = this.getArm(humanoidArm);
    modelpart.x += xTranslation;
    modelpart.y -= yTranslation;
    modelpart.translateAndRotate(poseStack);
    modelpart.x -= xTranslation;
    modelpart.y += yTranslation;
  }
}
