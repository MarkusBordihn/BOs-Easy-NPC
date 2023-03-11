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
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.model.ModelPose;

@OnlyIn(Dist.CLIENT)
public class CustomVillagerModel<T extends Entity> extends VillagerModel<T> {

  public CustomVillagerModel(ModelPart modelPart) {
    super(modelPart);
  }

  @Override
  public void setupAnim(T entity, float limbSwing, float limbSwingAmount, float ageInTicks,
      float netHeadYaw, float headPitch) {
    ModelPart root = root();
    ModelPart head = root.getChild("head");
    ModelPart rightLeg = root.getChild("right_leg");
    ModelPart leftLeg = root.getChild("left_leg");

    if (entity instanceof EasyNPCEntity easyNPCEntity
        && easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {

      Rotations headRotations = easyNPCEntity.getModelHeadRotation();
      if (headRotations != null) {
        head.xRot = headRotations.getX();
        head.yRot = headRotations.getY();
        head.zRot = headRotations.getZ();
      }

      Rotations bodyRotations = easyNPCEntity.getModelBodyRotation();
      if (bodyRotations != null) {
        root.xRot = bodyRotations.getX();
        root.yRot = bodyRotations.getY();
        root.zRot = bodyRotations.getZ();
      }

      Rotations rightLegRotations = easyNPCEntity.getModelRightLegRotation();
      if (rightLegRotations != null) {
        rightLeg.xRot = rightLegRotations.getX();
        rightLeg.yRot = rightLegRotations.getY();
        rightLeg.zRot = rightLegRotations.getZ();
      }

      Rotations leftLegRotations = easyNPCEntity.getModelLeftLegRotation();
      if (leftLegRotations != null) {
        leftLeg.xRot = leftLegRotations.getX();
        leftLeg.yRot = leftLegRotations.getY();
        leftLeg.zRot = leftLegRotations.getZ();
      }
    } else {
      // Reset all rotations to avoid any issues with other mods.
      head.xRot = 0.0F;
      head.yRot = 0.0F;
      head.zRot = 0.0F;
      root.xRot = 0.0F;
      root.yRot = 0.0F;
      root.zRot = 0.0F;
      rightLeg.xRot = 0.0F;
      rightLeg.yRot = 0.0F;
      rightLeg.zRot = 0.0F;
      leftLeg.xRot = 0.0F;
      leftLeg.yRot = 0.0F;
      leftLeg.zRot = 0.0F;

      super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    }
  }

}
