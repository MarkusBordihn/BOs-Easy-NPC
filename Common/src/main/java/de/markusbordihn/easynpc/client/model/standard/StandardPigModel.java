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

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.PigModel;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;

public class StandardPigModel<T extends Entity> extends PigModel<T> implements EasyNPCModel {
  public static final Rotations MODEL_BODY_ROTATION = new Rotations(1.5707964F, 0.0F, 0.0F);
  public static final CustomPosition MODEL_BODY_POSITION = new CustomPosition(0.0F, 11.0F, 2.0F);
  public static final CustomPosition MODEL_HEAD_POSITION = new CustomPosition(0.0F, 12.0F, -6.0F);
  public static final CustomPosition MODEL_LEFT_FRONT_LEG_POSITION =
      new CustomPosition(3.0F, 18.0F, -5.0F);
  public static final CustomPosition MODEL_RIGHT_FRONT_LEG_POSITION =
      new CustomPosition(-3.0F, 18.0F, -5.0F);
  public static final CustomPosition MODEL_LEFT_HIND_LEG_POSITION =
      new CustomPosition(3.0F, 18.0F, 7.0F);
  public static final CustomPosition MODEL_RIGHT_HIND_LEG_POSITION =
      new CustomPosition(-3.0F, 18.0F, 7.0F);

  public StandardPigModel(ModelPart modelPart) {
    super(modelPart);
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
    if (!(entity instanceof EasyNPC<?> easyNPC)) {
      return;
    }
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();

    EasyNPCModel.resetAnimalModel(
        this,
        this.head,
        this.body,
        this.leftFrontLeg,
        this.rightFrontLeg,
        this.leftHindLeg,
        this.rightHindLeg);

    if (modelData.getModelPose() == ModelPose.CUSTOM) {
      EasyNPCModel.setupAnimalModel(
          easyNPC,
          head,
          body,
          leftFrontLeg,
          rightFrontLeg,
          leftHindLeg,
          rightHindLeg,
          netHeadYaw,
          headPitch);
    } else if (modelData.getDefaultPose() != Pose.CROUCHING) {
      super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
    }
  }
}
