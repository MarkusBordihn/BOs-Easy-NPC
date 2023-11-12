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

package de.markusbordihn.easynpc.client.model.custom;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.IronGolemModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Pose;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class CustomIronGolemModel<T extends Mob> extends HierarchicalModel<T>
    implements ArmedModel, EasyNPCModel {

  // Model Information (rotation / position)
  public static final CustomPosition MODEL_BODY_POSITION = new CustomPosition(0.0F, -7.0F, 0.0F);
  public static final CustomPosition MODEL_HEAD_POSITION = new CustomPosition(0.0F, -7.0F, -2.0F);
  public static final CustomPosition MODEL_LEFT_ARM_POSITION = new CustomPosition(0f, -7f, 0f);
  public static final CustomPosition MODEL_RIGHT_ARM_POSITION = new CustomPosition(0f, -7f, 0f);
  public static final CustomPosition MODEL_LEFT_LEG_POSITION = new CustomPosition(-4f, 11f, 0f);
  public static final CustomPosition MODEL_RIGHT_LEG_POSITION = new CustomPosition(5f, 11f, 0f);

  // Model Parts
  private final ModelPart root;

  // Iron Golem Model specific positions
  private final ModelPart body;
  private final ModelPart head;
  private final ModelPart rightArm;
  private final ModelPart leftArm;
  private final ModelPart rightLeg;
  private final ModelPart leftLeg;

  public CustomIronGolemModel(ModelPart modelPart) {
    this.root = modelPart;
    this.head = modelPart.getChild("head");
    this.body = modelPart.getChild("body");
    this.rightArm = modelPart.getChild("right_arm");
    this.leftArm = modelPart.getChild("left_arm");
    this.rightLeg = modelPart.getChild("right_leg");
    this.leftLeg = modelPart.getChild("left_leg");
  }

  public static LayerDefinition createBodyLayer() {
    return IronGolemModel.createBodyLayer();
  }

  public ModelPart root() {
    return this.root;
  }

  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {

    if (entity instanceof EasyNPCEntity easyNPCEntity) {

      // Reset model to avoid any issues with other mods.
      resetHierarchicalModel(
          this.head, this.body, this.rightArm, this.leftArm, this.rightLeg, this.leftLeg);

      // Individual Part Modifications
      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {
        setupHierarchicalModel(
            easyNPCEntity, head, body, rightArm, leftArm, rightLeg, leftLeg, netHeadYaw, headPitch);
      } else if (easyNPCEntity.getPose() == Pose.SLEEPING) {
        // Sleeping Pose

      } else if (easyNPCEntity.getPose() == Pose.CROUCHING) {
        // Crouching Pose
        this.body.xRot = 0.5F;
        this.body.y = 3.2F;
        this.head.y = 4.2F;
        this.leftArm.xRot += 0.4F;
        this.leftArm.y = 5.2F;
        this.leftLeg.y = 12.2F;
        this.leftLeg.z = 4.0F;
        this.rightArm.xRot += 0.4F;
        this.rightArm.y = 5.2F;
        this.rightLeg.y = 12.2F;
        this.rightLeg.z = 4.0F;
      } else {
        this.head.yRot = netHeadYaw * ((float) Math.PI / 180F);
        this.head.xRot = headPitch * ((float) Math.PI / 180F);
        this.rightLeg.xRot = -1.5F * Mth.triangleWave(limbSwing, 13.0F) * limbSwingAmount;
        this.leftLeg.xRot = 1.5F * Mth.triangleWave(limbSwing, 13.0F) * limbSwingAmount;
        this.rightLeg.yRot = 0.0F;
        this.leftLeg.yRot = 0.0F;
      }
    }
  }

  @Override
  public void prepareMobModel(T entity, float p_102958_, float p_102959_, float p_102960_) {
    this.rightArm.xRot = (-0.2F + 1.5F * Mth.triangleWave(p_102958_, 13.0F)) * p_102959_;
    this.leftArm.xRot = (-0.2F - 1.5F * Mth.triangleWave(p_102958_, 13.0F)) * p_102959_;
  }

  protected ModelPart getArm(HumanoidArm humanoidArm) {
    return humanoidArm == HumanoidArm.LEFT ? this.leftArm : this.rightArm;
  }

  @Override
  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    float xTranslation = humanoidArm == HumanoidArm.RIGHT ? -10.0F : 10.0F;
    float yTranslation = humanoidArm == HumanoidArm.RIGHT ? -16.0F : 16.0F;
    ModelPart modelpart = this.getArm(humanoidArm);
    modelpart.x += xTranslation;
    modelpart.y -= yTranslation;
    modelpart.translateAndRotate(poseStack);
    modelpart.x -= xTranslation;
    modelpart.y += yTranslation;
  }

  @Override
  public CustomPosition getDefaultModelBodyPosition() {
    return MODEL_BODY_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelHeadPosition() {
    return MODEL_HEAD_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelLeftArmPosition() {
    return MODEL_LEFT_ARM_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightArmPosition() {
    return MODEL_RIGHT_ARM_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelLeftLegPosition() {
    return MODEL_LEFT_LEG_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightLegPosition() {
    return MODEL_RIGHT_LEG_POSITION;
  }
}
