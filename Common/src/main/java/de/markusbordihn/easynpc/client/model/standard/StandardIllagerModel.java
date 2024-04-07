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
import de.markusbordihn.easynpc.client.model.animation.HumanoidLegAnimation;
import de.markusbordihn.easynpc.client.model.base.BaseHierarchicalArmHeadModel;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Mob;

public class StandardIllagerModel<T extends Mob> extends BaseHierarchicalArmHeadModel<T> {

  private final ModelPart arms;

  public StandardIllagerModel(ModelPart modelPart) {
    super(modelPart);
    ModelPart hat = this.head.getChild("hat");
    hat.visible = false;
    this.arms = defineModelPart(ModelPartType.ARMS, modelPart, "arms");
  }

  @Override
  public void resetModelParts() {
    super.resetModelParts();
    this.resetModelPart(ModelPartType.ARMS, this.arms);
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }
    // Handle crossed arms variants
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    VariantData<?> variantData = easyNPC.getEasyNPCVariantData();
    boolean isCrossedArms =
        (modelData.getModelArmPose() == ModelArmPose.CROSSED || variantData.hasVariantCrossedArms())
            && !AttackHandler.isHoldingWeapon(entity);
    this.arms.visible = isCrossedArms;
    this.leftArm.visible = !isCrossedArms;
    this.rightArm.visible = !isCrossedArms;

    this.setupAnimation(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
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
    this.body.xRot = 0.5F;
    this.body.y = 3.2F;
    this.head.y = 4.2F;
    if (this.arms.visible) {
      this.arms.xRot += 0.4F;
      this.arms.y = 5.2F;
    }
    if (this.leftArm.visible) {
      this.leftArm.xRot += 0.4F;
      this.leftArm.y = 5.2F;
    }
    this.leftLeg.y = 12.2F;
    this.leftLeg.z = 4.0F;
    if (this.rightArm.visible) {
      this.rightArm.xRot += 0.4F;
      this.rightArm.y = 5.2F;
    }
    this.rightLeg.y = 12.2F;
    this.rightLeg.z = 4.0F;
    return true;
  }
}
