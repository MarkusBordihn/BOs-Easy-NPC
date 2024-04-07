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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.OcelotModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;

public class StandardCatModel<T extends Entity> extends OcelotModel<T>
    implements EasyNPCModel<T>, HeadedModel {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  public StandardCatModel(ModelPart modelPart) {
    super(modelPart);
    defineModelPart(ModelPartType.HEAD, modelPart, "head");
    defineModelPart(ModelPartType.BODY, modelPart, "body");
    defineModelPart(ModelPartType.RIGHT_HIND_LEG, modelPart, "right_hind_leg");
    defineModelPart(ModelPartType.LEFT_HIND_LEG, modelPart, "left_hind_leg");
    defineModelPart(ModelPartType.RIGHT_FRONT_LEG, modelPart, "right_front_leg");
    defineModelPart(ModelPartType.LEFT_FRONT_LEG, modelPart, "left_front_leg");
    defineModelPart(ModelPartType.TAIL1, modelPart, "tail1");
    defineModelPart(ModelPartType.TAIL2, modelPart, "tail2");
  }

  public ModelPart getHead() {
    return this.head;
  }

  @Override
  public void resetModelParts() {
    this.resetModelPart(ModelPartType.HEAD, this.head);
    this.resetModelPart(ModelPartType.BODY, this.body);
    this.resetModelPart(ModelPartType.RIGHT_HIND_LEG, this.rightHindLeg);
    this.resetModelPart(ModelPartType.LEFT_HIND_LEG, this.leftHindLeg);
    this.resetModelPart(ModelPartType.RIGHT_FRONT_LEG, this.rightFrontLeg);
    this.resetModelPart(ModelPartType.LEFT_FRONT_LEG, this.leftFrontLeg);
    this.resetModelPart(ModelPartType.TAIL1, this.tail1);
    this.resetModelPart(ModelPartType.TAIL2, this.tail2);
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
    this.tail1.x = this.body.x;
    this.tail1.y = this.body.y + 3.0F;
    this.tail1.z = this.body.z + 18.0F;
    this.tail2.x = this.tail1.x;
    this.tail2.y = this.tail1.y + 5.0F;
    this.tail2.z = this.tail1.z + 6.0F;
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
