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
import com.mojang.blaze3d.vertex.VertexConsumer;
import de.markusbordihn.easynpc.client.model.ModelHelper;
import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.base.BaseEntityModel;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;

public class StandardHorseModel<T extends Entity> extends BaseEntityModel<T>
    implements HeadedModel {

  protected final Map<ModelPartType, CustomPosition> modelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, Rotations> modelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  protected final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  protected final ModelPart head;
  protected final ModelPart body;
  protected final ModelPart rightFrontLeg;
  protected final ModelPart leftFrontLeg;
  protected final ModelPart rightHindLeg;
  protected final ModelPart leftHindLeg;
  protected final ModelPart tail;
  protected final ModelPart headParts;
  protected final ModelPart saddle;
  protected final ModelPart leftSaddleMouth;
  protected final ModelPart rightSaddleMouth;
  protected final ModelPart leftSaddleLine;
  protected final ModelPart rightSaddleLine;
  protected final ModelPart headSaddle;
  protected final ModelPart mouthSaddleWrap;

  public StandardHorseModel(ModelPart modelPart) {
    this.body = defineModelPart(ModelPartType.BODY, modelPart, "body");
    this.headParts = defineModelPart(ModelPartType.HEAD_PARTS, modelPart, "head_parts");
    this.head = defineModelPart(ModelPartType.HEAD, this.headParts, "head");
    this.rightFrontLeg =
        defineModelPart(ModelPartType.RIGHT_FRONT_LEG, modelPart, "right_front_leg");
    this.leftFrontLeg = defineModelPart(ModelPartType.LEFT_FRONT_LEG, modelPart, "left_front_leg");
    this.rightHindLeg = defineModelPart(ModelPartType.RIGHT_HIND_LEG, modelPart, "right_hind_leg");
    this.leftHindLeg = defineModelPart(ModelPartType.LEFT_HIND_LEG, modelPart, "left_hind_leg");
    this.tail = defineModelPart(ModelPartType.TAIL, this.body, "tail");
    this.saddle = this.body.getChild("saddle");
    this.rightSaddleMouth = this.headParts.getChild("right_saddle_mouth");
    this.leftSaddleMouth = this.headParts.getChild("left_saddle_mouth");
    this.rightSaddleLine = this.headParts.getChild("right_saddle_line");
    this.leftSaddleLine = this.headParts.getChild("left_saddle_line");
    this.headSaddle = this.headParts.getChild("head_saddle");
    this.mouthSaddleWrap = this.headParts.getChild("mouth_saddle_wrap");
  }

  @Override
  public void resetModelParts() {
    resetModelPart(ModelPartType.HEAD_PARTS, this.headParts);
    resetModelPart(ModelPartType.HEAD, this.head);
    resetModelPart(ModelPartType.BODY, this.body);
    resetModelPart(ModelPartType.RIGHT_FRONT_LEG, this.rightFrontLeg);
    resetModelPart(ModelPartType.LEFT_FRONT_LEG, this.leftFrontLeg);
    resetModelPart(ModelPartType.RIGHT_HIND_LEG, this.rightHindLeg);
    resetModelPart(ModelPartType.LEFT_HIND_LEG, this.leftHindLeg);
    resetModelPart(ModelPartType.TAIL, this.tail);
  }

  @Override
  public void adjustDefaultModelParts(T entity, EasyNPC<?> easyNPC) {
    VariantData<?> variantData = easyNPC.getEasyNPCVariantData();
    boolean isSaddled = variantData.hasVariantSaddled();
    this.saddle.visible = isSaddled;
    this.rightSaddleMouth.visible = isSaddled;
    this.leftSaddleMouth.visible = isSaddled;
    this.rightSaddleLine.visible = isSaddled;
    this.leftSaddleLine.visible = isSaddled;
    this.headSaddle.visible = isSaddled;
    this.mouthSaddleWrap.visible = isSaddled;
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
        this.headParts,
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
  public boolean animateModelFrontLegs(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    if (rightLegPart == null && leftLegPart == null) {
      return false;
    }
    if (rightLegPart != null) {
      rightLegPart.xRot = Mth.cos(limbSwing * 0.6662F + 3.1415927F) * 1.4F * limbSwingAmount;
    }
    if (leftLegPart != null) {
      leftLegPart.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
    return true;
  }

  @Override
  public boolean animateModelHindLegs(
      T entity,
      AttackData<?> attackData,
      ModelData<?> modelData,
      ModelPart rightLegPart,
      ModelPart leftLegPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    if (rightLegPart == null && leftLegPart == null) {
      return false;
    }
    if (rightLegPart != null) {
      rightLegPart.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
    if (leftLegPart != null) {
      leftLegPart.xRot = Mth.cos(limbSwing * 0.6662F + 3.1415927F) * 1.4F * limbSwingAmount;
    }
    return true;
  }

  public Iterable<ModelPart> headParts() {
    return List.of(this.headParts);
  }

  protected Iterable<ModelPart> bodyParts() {
    return List.of(
        this.body, this.rightHindLeg, this.leftHindLeg, this.rightFrontLeg, this.leftFrontLeg);
  }

  @Override
  public void renderToBuffer(
      PoseStack poseStack,
      VertexConsumer vertexConsumer,
      int light,
      int overlay,
      float red,
      float green,
      float blue,
      float alpha) {
    this.headParts()
        .forEach(
            headPart ->
                headPart.render(
                    poseStack, vertexConsumer, light, overlay, red, green, blue, alpha));
    this.bodyParts()
        .forEach(
            bodyPart ->
                bodyPart.render(
                    poseStack, vertexConsumer, light, overlay, red, green, blue, alpha));
  }

  @Override
  public ModelPart getHead() {
    return this.head;
  }

  @Override
  public boolean isHumanoidModel() {
    return false;
  }
}
