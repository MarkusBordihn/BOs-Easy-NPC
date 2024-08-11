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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.ChickenModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.HumanoidArm;

public class StandardChickenModel<T extends Entity> extends ChickenModel<T>
    implements EasyNPCModel<T>, ArmedModel {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, CustomRotation> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  private final ModelPart head;
  private final ModelPart body;
  private final ModelPart rightLeg;
  private final ModelPart leftLeg;
  private final ModelPart rightWing;
  private final ModelPart leftWing;
  private final ModelPart beak;
  private final ModelPart redThing;

  public StandardChickenModel(ModelPart modelPart) {
    super(modelPart);
    this.head = defineModelPart(ModelPartType.HEAD, modelPart, "head");
    this.beak = defineModelPart(ModelPartType.BEAK, modelPart, "beak");
    this.redThing = defineModelPart(ModelPartType.RED_THING, modelPart, "red_thing");
    this.body = defineModelPart(ModelPartType.BODY, modelPart, "body");
    this.rightLeg = defineModelPart(ModelPartType.RIGHT_LEG, modelPart, "right_leg");
    this.leftLeg = defineModelPart(ModelPartType.LEFT_LEG, modelPart, "left_leg");
    this.rightWing = defineModelPart(ModelPartType.RIGHT_WING, modelPart, "right_wing");
    this.leftWing = defineModelPart(ModelPartType.LEFT_WING, modelPart, "left_wing");
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_LEG, this.rightLeg);
    this.resetModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
    this.resetModelPart(ModelPartType.RIGHT_WING, this.rightWing);
    this.resetModelPart(ModelPartType.LEFT_WING, this.leftWing);
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (!setupAnimation(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch)) {
      super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    }
  }

  @Override
  public void adjustDefaultModelParts(T entity, EasyNPC<?> easyNPC) {
    this.beak.copyFrom(head);
    this.redThing.copyFrom(head);
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
    headPart.xRot = headPitch * Constants.PI_180DEG;
    headPart.yRot = netHeadYaw * Constants.PI_180DEG;
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
        this.leftWing,
        modelData.getModelLeftArmPosition(),
        modelData.getModelLeftArmRotation(),
        modelData.isModelLeftArmVisible());
    ModelHelper.setPositionRotationVisibility(
        this.rightWing,
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

  private ModelPart getArm(HumanoidArm humanoidArm) {
    return humanoidArm == HumanoidArm.LEFT ? this.leftWing : this.rightWing;
  }

  public ModelPart getHead() {
    return this.head;
  }

  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    this.getArm(humanoidArm).translateAndRotate(poseStack);
    poseStack.translate(0.0, -0.2, 0.1);
    poseStack.scale(0.7F, 0.7F, 0.7F);
    poseStack.translate(0.0625, 0.0, 0.0);
  }
}
