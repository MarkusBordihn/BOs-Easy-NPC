/**
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

package de.markusbordihn.easynpc.client.model;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.math.Axis;

import net.minecraft.client.model.ArmedModel;
import net.minecraft.client.model.HierarchicalModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.model.geom.builders.LayerDefinition;
import net.minecraft.core.Rotations;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.model.ModelPose;

@OnlyIn(Dist.CLIENT)
public class CustomAllayModel<T extends LivingEntity> extends HierarchicalModel<T>
    implements ArmedModel {

  private final ModelPart root;
  protected final ModelPart head;
  private final ModelPart body;
  private final ModelPart rightArm;
  private final ModelPart leftArm;
  private final ModelPart rightWing;
  private final ModelPart leftWing;

  public CustomAllayModel(ModelPart modelPart) {
    this.root = modelPart.getChild("root");
    this.head = this.root.getChild("head");
    this.body = this.root.getChild("body");
    this.rightArm = this.body.getChild("right_arm");
    this.leftArm = this.body.getChild("left_arm");
    this.rightWing = this.body.getChild("right_wing");
    this.leftWing = this.body.getChild("left_wing");
  }

  public ModelPart root() {
    return this.root;
  }

  public ModelPart getHead() {
    return this.head;
  }

  public static LayerDefinition createBodyLayer() {
    return net.minecraft.client.model.AllayModel.createBodyLayer();
  }

  public void setupAnim(T entity, float limbSwing, float limbSwingAmount, float ageInTicks,
      float netHeadYaw, float headPitch) {

    if (entity instanceof EasyNPCEntity easyNPCEntity
        && easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {

      Rotations headRotations = easyNPCEntity.getModelHeadRotation();
      if (headRotations != null) {
        this.head.xRot = headRotations.getX();
        this.head.yRot = headRotations.getY();
        this.head.zRot = headRotations.getZ();
      }

      Rotations bodyRotations = easyNPCEntity.getModelBodyRotation();
      if (bodyRotations != null) {
        this.body.xRot = bodyRotations.getX();
        this.body.yRot = bodyRotations.getY();
        this.body.zRot = bodyRotations.getZ();
      }

      Rotations rightArmRotations = easyNPCEntity.getModelRightArmRotation();
      if (rightArmRotations != null) {
        this.rightArm.xRot = rightArmRotations.getX();
        this.rightArm.yRot = rightArmRotations.getY();
        this.rightArm.zRot = rightArmRotations.getZ();
      }

      Rotations leftArmRotations = easyNPCEntity.getModelLeftArmRotation();
      if (leftArmRotations != null) {
        this.leftArm.xRot = leftArmRotations.getX();
        this.leftArm.yRot = leftArmRotations.getY();
        this.leftArm.zRot = leftArmRotations.getZ();
      }
    } else {
      // Reset all positions to avoid any issues with other mods.
      this.root().getAllParts().forEach(ModelPart::resetPose);

      // Body animations
      float ageAmount = ageInTicks * 9.0F * ((float) Math.PI / 180F);
      float limbSwingRotation = Math.min(limbSwingAmount / 0.3F, 1.0F);
      float bodyRotationAmount = limbSwingRotation * 0.6981317F;
      this.body.xRot = bodyRotationAmount;
      this.root.y += (float) Math.cos(ageAmount) * 0.25F * 1.0F - limbSwingRotation;

      // Arm animations
      float armRotationAmount = 0.43633232F
          - Mth.cos(ageAmount + ((float) Math.PI * 1.5F)) * (float) Math.PI * 0.075F * 1.0F
          - limbSwingRotation;
      this.rightArm.xRot = Mth.lerp(1f, bodyRotationAmount,
          Mth.lerp(limbSwingRotation, (-(float) Math.PI / 3F), (-(float) Math.PI / 4F)));
      this.leftArm.xRot = this.rightArm.xRot;
      this.leftArm.zRot = -armRotationAmount;
      this.rightArm.zRot = armRotationAmount;
      this.rightArm.yRot = 0.27925268F;
      this.leftArm.yRot = -0.27925268F;
    }

    // Wing animations
    float wingRotationAmount =
        Mth.cos(ageInTicks * 20.0F * ((float) Math.PI / 180F) + limbSwingAmount) * (float) Math.PI
            * 0.15F;
    this.rightWing.xRot = 0.43633232F;
    this.rightWing.yRot = -0.61086524F + wingRotationAmount;
    this.leftWing.xRot = 0.43633232F;
    this.leftWing.yRot = 0.61086524F - wingRotationAmount;
  }

  public void translateToHand(HumanoidArm humanoidArm, PoseStack poseStack) {
    this.root.translateAndRotate(poseStack);
    this.body.translateAndRotate(poseStack);
    poseStack.translate(0.0F, 0.0625F, 0.1875F);
    poseStack.mulPose(Axis.XP.rotation(this.rightArm.xRot));
    poseStack.scale(0.7F, 0.7F, 0.7F);
    poseStack.translate(0.0625D, 0.0D, 0.0D);
  }

  @Override
  public void renderToBuffer(PoseStack poseStack, VertexConsumer vertexConsumer, int lightLevel,
      int overlay, float unused1, float unused2, float unused3, float unused4) {
    this.root.render(poseStack, vertexConsumer, lightLevel, overlay);
  }
}
