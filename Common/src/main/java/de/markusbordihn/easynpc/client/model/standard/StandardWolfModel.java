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

import de.markusbordihn.easynpc.client.model.ModelPartType;
import de.markusbordihn.easynpc.client.model.base.BaseColorableAgeableListModel;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.List;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;

public class StandardWolfModel<T extends Entity> extends BaseColorableAgeableListModel<T>
    implements HeadedModel {

  private final ModelPart upperBody;

  public StandardWolfModel(ModelPart modelPart) {
    super(modelPart);
    this.upperBody = defineModelPart(ModelPartType.UPPER_BODY, modelPart, "upper_body");
    ModelPart realHead = this.head.getChild("real_head");
    ModelPart realTail = this.tail.getChild("real_tail");
  }

  @Override
  public void resetModelParts() {
    super.resetModelParts();
    this.resetModelPart(ModelPartType.UPPER_BODY, this.upperBody);
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
    if (attackData.isAggressive()) {
      this.tail.yRot = 0.0F;
    } else {
      this.tail.yRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
    return true;
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

  @Override
  public boolean setupCrouchingModelPose(
      T entity,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    this.upperBody.setPos(-1.0F, 16.0F, -3.0F);
    this.upperBody.xRot = 1.2566371F;
    this.upperBody.yRot = 0.0F;
    this.body.setPos(0.0F, 18.0F, 0.0F);
    this.body.xRot = 0.7853982F;
    this.tail.setPos(-1.0F, 21.0F, 6.0F);
    this.rightHindLeg.setPos(-2.5F, 22.7F, 2.0F);
    this.rightHindLeg.xRot = 4.712389F;
    this.leftHindLeg.setPos(0.5F, 22.7F, 2.0F);
    this.leftHindLeg.xRot = 4.712389F;
    this.rightFrontLeg.xRot = 5.811947F;
    this.rightFrontLeg.setPos(-2.49F, 17.0F, -4.0F);
    this.leftFrontLeg.xRot = 5.811947F;
    this.leftFrontLeg.setPos(0.51F, 17.0F, -4.0F);
    return true;
  }

  @Override
  protected Iterable<ModelPart> bodyParts() {
    return List.of(
        this.body,
        this.rightHindLeg,
        this.leftHindLeg,
        this.rightFrontLeg,
        this.leftFrontLeg,
        this.tail,
        this.upperBody);
  }

  @Override
  public ModelPart getHead() {
    return this.head;
  }
}
