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
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.PigModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;

public class StandardPigModel<T extends Entity> extends PigModel<T>
    implements EasyNPCModel<T>, HeadedModel {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  public StandardPigModel(ModelPart modelPart) {
    super(modelPart);
    defineModelPart(ModelPartType.HEAD, modelPart, "head");
    defineModelPart(ModelPartType.BODY, modelPart, "body");
    defineModelPart(ModelPartType.RIGHT_FRONT_LEG, modelPart, "right_front_leg");
    defineModelPart(ModelPartType.LEFT_FRONT_LEG, modelPart, "left_front_leg");
    defineModelPart(ModelPartType.RIGHT_HIND_LEG, modelPart, "right_hind_leg");
    defineModelPart(ModelPartType.LEFT_HIND_LEG, modelPart, "left_hind_leg");
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_FRONT_LEG, this.rightFrontLeg);
    this.resetModelPart(ModelPartType.LEFT_FRONT_LEG, this.leftFrontLeg);
    this.resetModelPart(ModelPartType.RIGHT_HIND_LEG, this.rightHindLeg);
    this.resetModelPart(ModelPartType.LEFT_HIND_LEG, this.leftHindLeg);
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
        this.leftFrontLeg,
        modelData.getModelLeftArmPosition(),
        modelData.getModelLeftArmRotation(),
        modelData.isModelLeftArmVisible());
    ModelHelper.setPositionRotationVisibility(
        this.rightFrontLeg,
        modelData.getModelRightArmPosition(),
        modelData.getModelRightArmRotation(),
        modelData.isModelRightArmVisible());
    ModelHelper.setPositionRotationVisibility(
        this.leftHindLeg,
        modelData.getModelLeftLegPosition(),
        modelData.getModelLeftLegRotation(),
        modelData.isModelLeftLegVisible());
    ModelHelper.setPositionRotationVisibility(
        this.rightHindLeg,
        modelData.getModelRightLegPosition(),
        modelData.getModelRightLegRotation(),
        modelData.isModelRightLegVisible());
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

  @Override
  public ModelPart getHead() {
    return this.head;
  }
}
