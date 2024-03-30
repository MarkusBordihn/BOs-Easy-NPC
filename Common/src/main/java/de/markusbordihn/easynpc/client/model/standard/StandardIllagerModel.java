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
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HeadedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.IllagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;

public class StandardIllagerModel<T extends LivingEntity> extends HierarchicalModel<T>
    implements ArmedModel, HeadedModel, EasyNPCModel {

  private final ModelPart body;
  private final ModelPart root;
  private final ModelPart head;
  private final ModelPart hat;
  private final ModelPart arms;
  private final ModelPart leftLeg;
  private final ModelPart rightLeg;
  private final ModelPart rightArm;
  private final ModelPart leftArm;

  public StandardIllagerModel(ModelPart modelPart) {
    this.root = modelPart;
    this.head = modelPart.getChild("head");
    this.body = modelPart.getChild("body");
    this.hat = this.head.getChild("hat");
    this.arms = modelPart.getChild("arms");
    this.leftLeg = modelPart.getChild("left_leg");
    this.rightLeg = modelPart.getChild("right_leg");
    this.leftArm = modelPart.getChild("left_arm");
    this.rightArm = modelPart.getChild("right_arm");
  }

  public static LayerDefinition createBodyLayer() {
    return IllagerModel.createBodyLayer();
  }

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

    ModelData<?> modelData = easyNPC.getEasyNPCModelData();

    // Reset model to avoid any issues with other mods.
    EasyNPCModel.resetHierarchicalModel(
        this,
        this.head,
        this.body,
        this.arms,
        this.rightArm,
        this.leftArm,
        this.rightLeg,
        this.leftLeg);

    // Handle crossed arms variants
    VariantData<?> variantData = easyNPC.getEasyNPCVariantData();
    AttackData<?> attackData = easyNPC.getEasyNPCAttackData();
    boolean isCrossedArms =
        (modelData.getModelArmPose() == ModelArmPose.CROSSED || variantData.hasVariantCrossedArms())
            && !attackData.isHoldingWeapon();
    this.arms.visible = isCrossedArms;
    this.leftArm.visible = !isCrossedArms;
    this.rightArm.visible = !isCrossedArms;

    // Handle model data
    if (modelData.getModelPose() == ModelPose.CUSTOM) {
      setupHierarchicalModel(
          easyNPC,
          this.head,
          this.body,
          this.arms,
          this.rightArm,
          this.leftArm,
          this.rightLeg,
          this.leftLeg,
          netHeadYaw,
          headPitch);
    } else if (modelData.getDefaultPose() == Pose.CROUCHING) {
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
    }

    if (!EasyNPCModel.animateHierarchicalModel(
        this,
        easyNPC,
        this.head,
        this.body,
        this.rightArm,
        this.leftArm,
        this.rightLeg,
        this.leftLeg,
        ageInTicks,
        limbSwing,
        limbSwingAmount)) {
      // Adjust head rotation
      this.head.yRot = netHeadYaw * ((float) Math.PI / 180F);
      this.head.xRot = headPitch * ((float) Math.PI / 180F);

      // Add arm swing
      if (!isCrossedArms) {
        this.rightArm.xRot =
            Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 2.0F * limbSwingAmount * 0.5F;
        this.leftArm.xRot = Mth.cos(limbSwing * 0.6662F) * 2.0F * limbSwingAmount * 0.5F;
      }
    }
  }

  protected ModelPart getArm(HumanoidArm humanoidArm) {
    return humanoidArm == HumanoidArm.LEFT ? this.leftArm : this.rightArm;
  }

  public ModelPart root() {
    return this.root;
  }

  public ModelPart getHat() {
    return this.hat;
  }

  public ModelPart getHead() {
    return this.head;
  }

  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    this.getArm(humanoidArm).translateAndRotate(poseStack);
  }
}
