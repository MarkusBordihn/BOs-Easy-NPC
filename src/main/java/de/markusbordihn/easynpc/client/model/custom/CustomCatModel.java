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
import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.client.model.OcelotModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class CustomCatModel<T extends Entity> extends OcelotModel<T> implements EasyNPCModel {

  // Model Information (rotation / position)
  public static final Rotations MODEL_BODY_ROTATION =
      new Rotations(((float) Math.PI / 2F), 0.0F, 0.0F);
  public static final CustomPosition MODEL_BODY_POSITION = new CustomPosition(0.0F, 12.0F, -10.0F);
  public static final CustomPosition MODEL_HEAD_POSITION = new CustomPosition(0.0F, 15.0F, -9.0F);
  public static final CustomPosition MODEL_LEFT_FRONT_LEG_POSITION =
      new CustomPosition(1.2F, 14.1F, -5.0F);
  public static final CustomPosition MODEL_RIGHT_FRONT_LEG_POSITION =
      new CustomPosition(-1.2F, 14.1F, -5.0F);
  public static final CustomPosition MODEL_LEFT_HIND_LEG_POSITION =
      new CustomPosition(1.1F, 18.0F, 5.0F);
  public static final CustomPosition MODEL_RIGHT_HIND_LEG_POSITION =
      new CustomPosition(-1.1F, 18.0F, 5.0F);

  public CustomCatModel(ModelPart modelPart) {
    super(modelPart);
  }

  public ModelPart getHead() {
    return this.head;
  }

  @Override
  public Rotations getDefaultModelBodyRotation() {
    return MODEL_BODY_ROTATION;
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
  public CustomPosition getDefaultModelLeftFrontLegPosition() {
    return MODEL_LEFT_FRONT_LEG_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightFrontLegPosition() {
    return MODEL_RIGHT_FRONT_LEG_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelLeftHindLegPosition() {
    return MODEL_LEFT_HIND_LEG_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightHindLegPosition() {
    return MODEL_RIGHT_HIND_LEG_POSITION;
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
      resetAnimalModel(
          this.head,
          this.body,
          this.leftFrontLeg,
          this.rightFrontLeg,
          this.leftHindLeg,
          this.rightHindLeg);

      // Individual Part Modifications
      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {
        setupCustomAnimalModel(
            easyNPCEntity,
            head,
            body,
            leftFrontLeg,
            rightFrontLeg,
            leftHindLeg,
            rightHindLeg,
            netHeadYaw,
            headPitch);
      } else if (easyNPCEntity.getPose() != Pose.CROUCHING) {
        super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
      }
    }

    // Adjust position and rotation of the tail
    this.tail1.x = this.body.x;
    this.tail1.y = this.body.y + 3.0F;
    this.tail1.z = this.body.z + 18.0F;
    this.tail2.x = this.tail1.x;
    this.tail2.y = this.tail1.y + 5.0F;
    this.tail2.z = this.tail1.z + 6.0F;
  }
}
