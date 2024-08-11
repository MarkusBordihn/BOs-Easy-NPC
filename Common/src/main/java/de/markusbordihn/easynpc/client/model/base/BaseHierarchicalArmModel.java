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
import com.mojang.math.Vector3f;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.animation.HumanoidArmAnimation;
import de.markusbordihn.easynpc.client.model.animation.HumanoidHeadAnimation;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.Mob;

public class BaseHierarchicalArmModel<E extends Mob> extends HierarchicalModel<E>
    implements ArmedModel, EasyNPCModel<E> {
  public final ModelPart head;
  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, CustomRotation> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);
  protected final ModelPart root;
  protected final ModelPart body;
  protected final ModelPart rightArm;
  protected final ModelPart leftArm;

  public BaseHierarchicalArmModel(ModelPart modelPart) {
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
  }

  @Override
  public boolean animateModelHead(
      E entity,
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
      E entity,
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
    this.rightArm.xRot += 0.4F;
    this.rightArm.y = 5.2F;
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

  protected ModelPart getArm(HumanoidArm humanoidArm) {
    return humanoidArm == HumanoidArm.LEFT ? this.leftArm : this.rightArm;
  }

  @Override
  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    this.root.translateAndRotate(poseStack);
    this.body.translateAndRotate(poseStack);
    poseStack.translate(0.0, -0.09375, 0.09375);
    poseStack.mulPose(Vector3f.XP.rotation(this.rightArm.xRot + 0.43633232F));
    poseStack.scale(0.7F, 0.7F, 0.7F);
    poseStack.translate(0.0625, 0.0, 0.0);
  }
}
