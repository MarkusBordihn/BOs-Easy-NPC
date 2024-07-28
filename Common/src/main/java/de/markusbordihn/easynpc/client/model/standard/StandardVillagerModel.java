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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.animation.HumanoidLegAnimation;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.VillagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;

public class StandardVillagerModel<T extends Entity> extends VillagerModel<T>
    implements EasyNPCModel<T> {
  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);
  private final ModelPart arms;
  private final ModelPart body;
  private final ModelPart head;
  private final ModelPart leftLeg;
  private final ModelPart rightLeg;

  public StandardVillagerModel(ModelPart modelPart) {
    super(modelPart);
    this.arms = defineModelPart(ModelPartType.ARMS, modelPart, "arms");
    this.body = defineModelPart(ModelPartType.BODY, modelPart, "body");
    this.head = defineModelPart(ModelPartType.HEAD, modelPart, "head");
    this.leftLeg = defineModelPart(ModelPartType.LEFT_LEG, modelPart, "left_leg");
    this.rightLeg = defineModelPart(ModelPartType.RIGHT_LEG, modelPart, "right_leg");
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
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.ARMS, this.arms);
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_LEG, this.rightLeg);
    this.resetModelPart(ModelPartType.LEFT_LEG, this.leftLeg);
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
        this.arms,
        modelData.getModelArmsPosition(),
        modelData.getModelArmsRotation(),
        modelData.isModelArmsVisible());
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
    headPart.xRot = headPitch * Constants.PI_180DEG;
    headPart.yRot = netHeadYaw * Constants.PI_180DEG;
    return true;
  }

  @Override
  public boolean animateModelLegs(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    return HumanoidLegAnimation.animateHumanoidModelLegs(
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
    this.arms.xRot += 0.4F;
    this.arms.y = 5.2F;
    this.body.xRot = 0.5F;
    this.body.y = 3.2F;
    this.head.y = 4.2F;
    this.leftLeg.y = 12.2F;
    this.leftLeg.z = 4.0F;
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
