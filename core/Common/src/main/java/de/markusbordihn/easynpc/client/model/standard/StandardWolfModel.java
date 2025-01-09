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
import de.markusbordihn.easynpc.client.model.base.BaseEntityModel;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.entity.state.WolfRenderState;
import net.minecraft.util.Mth;

public class StandardWolfModel<T extends WolfRenderState> extends BaseEntityModel<T>
    implements HeadedModel {

  private final ModelPart upperBody;
  private final ModelPart tail;
  private final ModelPart head;

  public StandardWolfModel(ModelPart modelPart) {
    super(modelPart);
    this.upperBody = defineModelPart(ModelPartType.UPPER_BODY, modelPart, "upper_body");
    this.head = modelPart.getChild("head");
    this.tail = this.getHead().getChild("tail");
  }

  @Override
  public void resetModelParts() {
    super.resetModelParts();
    this.resetModelPart(ModelPartType.UPPER_BODY, this.upperBody);
  }

  @Override
  public boolean additionalModelAnimation(
      T entityRenderState,
      AttackData<?> attackData,
      ModelData<?> modelData,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (entityRenderState.isAngry) {
      this.tail.yRot = 0.0F;
    } else {
      this.tail.yRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
    }
    return true;
  }

  @Override
  public boolean animateModelFrontLegs(
      T entityRenderState,
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
      T entityRenderState,
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
  public ModelPart getHead() {
    return this.head;
  }
}
