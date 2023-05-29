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

import net.minecraft.client.model.VillagerModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

@OnlyIn(Dist.CLIENT)
public class CustomVillagerModel<T extends Entity> extends VillagerModel<T> {

  private final ModelPart arms;
  private final ModelPart body;
  private final ModelPart head;
  private final ModelPart leftLeg;
  private final ModelPart rightLeg;

  public CustomVillagerModel(ModelPart modelPart) {
    super(modelPart);
    this.arms = modelPart.getChild("arms");
    this.body = modelPart.getChild("body");
    this.head = modelPart.getChild("head");
    this.leftLeg = modelPart.getChild("left_leg");
    this.rightLeg = modelPart.getChild("right_leg");
  }

  @Override
  public void setupAnim(T entity, float limbSwing, float limbSwingAmount, float ageInTicks,
      float netHeadYaw, float headPitch) {
    if (entity instanceof EasyNPCEntity easyNPCEntity) {

      // Reset all rotations to avoid any issues with other mods.
      CustomModelHelper.resetRotation(this.body);
      CustomModelHelper.resetRotation(this.head);
      CustomModelHelper.setRotation(this.arms, -0.8F, 0.0F, 0.0F);
      CustomModelHelper.resetRotation(this.leftLeg);
      CustomModelHelper.resetRotation(this.rightLeg);

      // Reset all positions to avoid any issues with other mods.
      CustomModelHelper.resetPosition(this.body);
      CustomModelHelper.resetPosition(this.head);
      CustomModelHelper.setPosition(this.arms, 0.0F, 2.0F, 0.0F);
      CustomModelHelper.setPosition(this.leftLeg, 1.9F, 12.0F, 0.0F);
      CustomModelHelper.setPosition(this.rightLeg, -1.9F, 12.0F, 0.0F);

      // Reset all visibility to avoid any issues with other mods.
      this.body.visible = true;
      this.head.visible = true;
      this.arms.visible = true;
      this.leftLeg.visible = true;
      this.rightLeg.visible = true;

      // Individual Part Modifications
      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {

        // Head position, rotation and visibility.
        CustomModelHelper.setHeadPositionRotationVisibility(this.head,
            easyNPCEntity.getModelHeadPosition(), easyNPCEntity.getModelHeadRotation(),
            easyNPCEntity.isModelHeadVisible(), netHeadYaw, headPitch);

        // Body position, rotation and visibility.
        CustomModelHelper.setPositionRotationVisibility(this.body,
            easyNPCEntity.getModelBodyPosition(), easyNPCEntity.getModelBodyRotation(),
            easyNPCEntity.isModelBodyVisible());

        // Arms position, rotation and visibility.
        CustomModelHelper.setPositionRotationVisibility(this.arms,
            easyNPCEntity.getModelArmsPosition(), easyNPCEntity.getModelArmsRotation(),
            easyNPCEntity.isModelArmsVisible());

        // Right Leg position, rotation and visibility.
        CustomModelHelper.setPositionRotationVisibility(this.rightLeg,
            easyNPCEntity.getModelRightLegPosition(), easyNPCEntity.getModelRightLegRotation(),
            easyNPCEntity.isModelRightLegVisible());

        // Left Leg position, rotation and visibility.
        CustomModelHelper.setPositionRotationVisibility(this.leftLeg,
            easyNPCEntity.getModelLeftLegPosition(), easyNPCEntity.getModelLeftLegRotation(),
            easyNPCEntity.isModelLeftLegVisible());

      } else if (easyNPCEntity.getPose() == Pose.CROUCHING) {
        // Crouching Pose
        this.arms.xRot += 0.4F;
        this.arms.y = 5.2F;
        this.body.xRot = 0.5F;
        this.body.y = 3.2F;
        this.head.y = 4.2F;
        this.leftLeg.y = 12.2F;
        this.leftLeg.z = 4.0F;
        this.rightLeg.y = 12.2F;
        this.rightLeg.z = 4.0F;
      }

      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM
          || easyNPCEntity.getPose() == Pose.CROUCHING) {
      } else {
        super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
      }
    }
  }

}
