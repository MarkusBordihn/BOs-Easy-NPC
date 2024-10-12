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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.client.model.ColorableAgeableListModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;

public class BaseColorableAgeableListModel<E extends Entity> extends ColorableAgeableListModel<E>
    implements EasyNPCModel<E> {
  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, CustomRotation> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Boolean> modelPartVisibilityMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);
  protected final ModelPart tail;
  protected final ModelPart head;
  protected final ModelPart body;
  protected final ModelPart rightHindLeg;
  protected final ModelPart leftHindLeg;
  protected final ModelPart rightFrontLeg;
  protected final ModelPart leftFrontLeg;

  public BaseColorableAgeableListModel(ModelPart modelPart) {
    this.head = defineModelPart(ModelPartType.HEAD, modelPart, "head");
    this.body = defineModelPart(ModelPartType.BODY, modelPart, "body");
    this.rightHindLeg = defineModelPart(ModelPartType.RIGHT_HIND_LEG, modelPart, "right_hind_leg");
    this.leftHindLeg = defineModelPart(ModelPartType.LEFT_HIND_LEG, modelPart, "left_hind_leg");
    this.rightFrontLeg =
        defineModelPart(ModelPartType.RIGHT_FRONT_LEG, modelPart, "right_front_leg");
    this.leftFrontLeg = defineModelPart(ModelPartType.LEFT_FRONT_LEG, modelPart, "left_front_leg");
    this.tail = defineModelPart(ModelPartType.TAIL, modelPart, "tail");
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_HIND_LEG, this.rightHindLeg);
    this.resetModelPart(ModelPartType.LEFT_HIND_LEG, this.leftHindLeg);
    this.resetModelPart(ModelPartType.RIGHT_FRONT_LEG, this.rightFrontLeg);
    this.resetModelPart(ModelPartType.LEFT_FRONT_LEG, this.leftFrontLeg);
    this.resetModelPart(ModelPartType.TAIL, this.tail);
  }

  @Override
  protected Iterable<ModelPart> headParts() {
    return List.of(this.head);
  }

  @Override
  protected Iterable<ModelPart> bodyParts() {
    return List.of(
        this.body,
        this.rightHindLeg,
        this.leftHindLeg,
        this.rightFrontLeg,
        this.leftFrontLeg,
        this.tail);
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
        leftFrontLeg,
        modelData.getModelLeftArmPosition(),
        modelData.getModelLeftArmRotation(),
        modelData.isModelLeftArmVisible());
    ModelHelper.setPositionRotationVisibility(
        rightFrontLeg,
        modelData.getModelRightArmPosition(),
        modelData.getModelRightArmRotation(),
        modelData.isModelRightArmVisible());
    ModelHelper.setPositionRotationVisibility(
        leftHindLeg,
        modelData.getModelLeftLegPosition(),
        modelData.getModelLeftLegRotation(),
        modelData.isModelLeftLegVisible());
    ModelHelper.setPositionRotationVisibility(
        rightHindLeg,
        modelData.getModelRightLegPosition(),
        modelData.getModelRightLegRotation(),
        modelData.isModelRightLegVisible());
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
    headPart.xRot = headPitch * Constants.PI_180DEG;
    headPart.yRot = netHeadYaw * Constants.PI_180DEG;
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
  public Map<ModelPartType, Boolean> getModelPartVisibilityMap() {
    return this.modelPartVisibilityMap;
  }

  @Override
  public Map<ModelPartType, ModelPart> getModelPartMap() {
    return this.modelPartMap;
  }

  @Override
  public boolean isHumanoidModel() {
    return false;
  }
}
