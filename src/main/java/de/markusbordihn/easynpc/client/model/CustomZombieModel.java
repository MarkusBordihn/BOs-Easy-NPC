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

import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.LivingEntity;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

@OnlyIn(Dist.CLIENT)
public class CustomZombieModel<T extends LivingEntity> extends HumanoidModel<T> {

  public CustomZombieModel(ModelPart modelPart) {
    super(modelPart);
  }

  @Override
  public void setupAnim(T entity, float limbSwing, float limbSwingAmount, float ageInTicks,
      float netHeadYaw, float headPitch) {
    boolean isAggressive = false;
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      // Aggressive Animation
      isAggressive = easyNPCEntity.isAggressive();

      // Individual Part Rotations
      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {
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

        Rotations rightLegRotations = easyNPCEntity.getModelRightLegRotation();
        if (rightLegRotations != null) {
          this.rightLeg.xRot = rightLegRotations.getX();
          this.rightLeg.yRot = rightLegRotations.getY();
          this.rightLeg.zRot = rightLegRotations.getZ();
        }

        Rotations leftLegRotations = easyNPCEntity.getModelLeftLegRotation();
        if (leftLegRotations != null) {
          this.leftLeg.xRot = leftLegRotations.getX();
          this.leftLeg.yRot = leftLegRotations.getY();
          this.leftLeg.zRot = leftLegRotations.getZ();
        }

        // Copy all outer model parts to the correct model parts.
        this.hat.copyFrom(this.head);
      } else {
        // Reset all rotations to avoid any issues with other mods.
        this.head.xRot = 0.0F;
        this.head.yRot = 0.0F;
        this.head.zRot = 0.0F;
        this.body.xRot = 0.0F;
        this.body.yRot = 0.0F;
        this.body.zRot = 0.0F;
        this.rightArm.xRot = 0.0F;
        this.rightArm.yRot = 0.0F;
        this.rightArm.zRot = 0.0F;
        this.leftArm.xRot = 0.0F;
        this.leftArm.yRot = 0.0F;
        this.leftArm.zRot = 0.0F;
        this.rightLeg.xRot = 0.0F;
        this.rightLeg.yRot = 0.0F;
        this.rightLeg.zRot = 0.0F;
        this.leftLeg.xRot = 0.0F;
        this.leftLeg.yRot = 0.0F;
        this.leftLeg.zRot = 0.0F;

        super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
        AnimationUtils.animateZombieArms(this.leftArm, this.rightArm, isAggressive, this.attackTime,
            ageInTicks);
      }
    }
  }

}
