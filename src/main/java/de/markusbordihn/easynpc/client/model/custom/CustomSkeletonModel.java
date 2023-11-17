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

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.client.model.SkeletonModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class CustomSkeletonModel<T extends Mob & RangedAttackMob> extends SkeletonModel<T>
    implements EasyNPCModel {

  public CustomSkeletonModel(ModelPart modelPart) {
    super(modelPart);
  }

  @Override
  public void setupAnim(
      T entity,
      float limbSwing,
      float limbSwingAmount,
      float ageInTicks,
      float netHeadYaw,
      float headPitch) {
    if (entity instanceof EasyNPCEntity easyNPCEntity) {

      // Reset player model to avoid any issues with other mods.
      EasyNPCModel.resetHumanoidModel(
          this, this.head, this.body, this.rightArm, this.leftArm, this.rightLeg, this.leftLeg);

      // Individual Part Modifications
      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {
        EasyNPCModel.setupHumanoidModel(
            easyNPCEntity,
            this.head,
            this.body,
            this.rightArm,
            this.leftArm,
            this.rightLeg,
            this.leftLeg,
            netHeadYaw,
            headPitch);
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
      }

      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM
          || easyNPCEntity.getPose() == Pose.CROUCHING) {

        // Handle animations, if model specific part was not adjusted.
        if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {
          EasyNPCModel.animateHumanoidModel(
              this,
              this.head,
              this.body,
              this.rightArm,
              this.leftArm,
              this.rightLeg,
              this.leftLeg,
              limbSwing,
              limbSwingAmount);
        }

        // Copy all outer model parts to the correct model parts.
        this.hat.copyFrom(this.head);
      } else {
        super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
      }
    }
  }
}
