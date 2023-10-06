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

package de.markusbordihn.easynpc.client.model.custom;

import net.minecraft.client.model.ChickenModel;
import net.minecraft.client.model.geom.ModelPart;

import net.minecraft.core.Rotations;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Pose;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.client.model.EasyNPCModel;
import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

@OnlyIn(Dist.CLIENT)
public class CustomChickenModel<T extends Entity> extends ChickenModel<T> implements EasyNPCModel {

  // Model Parts
  private final ModelPart head;
  private final ModelPart body;
  private final ModelPart rightLeg;
  private final ModelPart leftLeg;
  private final ModelPart rightWing;
  private final ModelPart leftWing;
  private final ModelPart beak;
  private final ModelPart redThing;

  // Model Information (rotation / position)
  public static final CustomPosition MODEL_HEAD_POSITION = new CustomPosition(0f, 15f, -4f);
  public static final CustomPosition MODEL_BODY_POSITION = new CustomPosition(0f, 16f, 0f);
  public static final CustomPosition MODEL_LEFT_WING_POSITION = new CustomPosition(4f, 13f, 0f);
  public static final CustomPosition MODEL_RIGHT_WING_POSITION = new CustomPosition(-4f, 13f, 0f);
  public static final CustomPosition MODEL_LEFT_LEG_POSITION = new CustomPosition(1f, 19f, 1f);
  public static final CustomPosition MODEL_RIGHT_LEG_POSITION = new CustomPosition(-2f, 19f, 1f);

  public static final Rotations MODEL_BODY_ROTATION =
      new Rotations(((float) Math.PI / 2f), 0.0f, 0.0f);

  public CustomChickenModel(ModelPart modelPart) {
    super(modelPart);
    this.head = modelPart.getChild("head");
    this.beak = modelPart.getChild("beak");
    this.redThing = modelPart.getChild("red_thing");
    this.body = modelPart.getChild("body");
    this.rightLeg = modelPart.getChild("right_leg");
    this.leftLeg = modelPart.getChild("left_leg");
    this.rightWing = modelPart.getChild("right_wing");
    this.leftWing = modelPart.getChild("left_wing");
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
  public CustomPosition getDefaultModelLeftWingPosition() {
    return MODEL_LEFT_WING_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightWingPosition() {
    return MODEL_RIGHT_WING_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelLeftLegPosition() {
    return MODEL_LEFT_LEG_POSITION;
  }

  @Override
  public CustomPosition getDefaultModelRightLegPosition() {
    return MODEL_RIGHT_LEG_POSITION;
  }

  @Override
  public Rotations getDefaultModelBodyRotation() {
    return MODEL_BODY_ROTATION;
  }

  @Override
  public void setupAnim(T entity, float limbSwing, float limbSwingAmount, float ageInTicks,
      float netHeadYaw, float headPitch) {

    if (entity instanceof EasyNPCEntity easyNPCEntity) {

      // Reset player model to avoid any issues with other mods.
      resetBirdModel(this.head, this.body, this.leftWing, this.rightWing, this.leftLeg,
          this.rightLeg);

      // Individual Part Modifications
      if (easyNPCEntity.getModelPose() == ModelPose.CUSTOM) {
        setupBirdModel(easyNPCEntity, head, body, leftWing, rightWing, leftLeg, rightLeg,
            netHeadYaw, headPitch);
      } else if (easyNPCEntity.getPose() != Pose.CROUCHING) {
        super.setupAnim(entity, limbSwing, limbSwingAmount, ageInTicks, netHeadYaw, headPitch);
      }

    }

    // Adjust position and rotation of additional parts
    this.beak.copyFrom(head);
    this.redThing.copyFrom(head);
  }

}
