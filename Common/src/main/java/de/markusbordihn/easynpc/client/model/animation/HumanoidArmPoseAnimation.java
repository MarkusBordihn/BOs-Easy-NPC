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

package de.markusbordihn.easynpc.client.model.animation;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.model.ModelArmPose;
import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Mob;

public interface HumanoidArmPoseAnimation {

  static <T extends Mob> void animateHumanoidModelArmPoseAttackingWithMeleeWeapon(
      ModelPart rightArmPart, ModelPart leftArmPart, T mob, float attackTime, float ageInTicks) {
    AnimationUtils.swingWeaponDown(rightArmPart, leftArmPart, mob, attackTime, ageInTicks);
  }

  static <T extends Mob> void animateHumanoidModelArmPoseAttacking(
      ModelPart rightArmPart, ModelPart leftArmPart, T mob, float attackTime, float ageInTicks) {
    AnimationUtils.swingWeaponDown(rightArmPart, leftArmPart, mob, attackTime, ageInTicks);
  }

  static void animateHumanoidModelArmPoseBowAndArrow(
      ModelPart rightArmPart, ModelPart leftArmPart, ModelPart headModelPart) {
    rightArmPart.yRot = -0.1F + headModelPart.yRot;
    rightArmPart.xRot = -Constants.HALF_OF_PI + headModelPart.xRot;
    leftArmPart.xRot = -0.9424779F + headModelPart.xRot;
    leftArmPart.yRot = headModelPart.yRot - 0.4F;
    leftArmPart.zRot = Constants.HALF_OF_PI;
  }

  static void animateHumanoidModelArmPoseCelebrating(
      ModelPart rightArmPart, ModelPart leftArmPart, float ageInTicks) {
    rightArmPart.z = 0.0F;
    rightArmPart.x = -5.0F;
    rightArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.05F;
    rightArmPart.zRot = 2.670354F;
    rightArmPart.yRot = 0.0F;
    leftArmPart.z = 0.0F;
    leftArmPart.x = 5.0F;
    leftArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.05F;
    leftArmPart.zRot = -2.3561945F;
    leftArmPart.yRot = 0.0F;
  }

  static <T extends Mob> void animateHumanoidModelArmPoseCrossbowCharge(
      ModelPart rightArmPart, ModelPart leftArmPart, T mob) {
    AnimationUtils.animateCrossbowCharge(rightArmPart, leftArmPart, mob, true);
  }

  static void animateHumanoidModelArmPoseCrossbowHold(
      ModelPart rightArmPart, ModelPart leftArmPart, ModelPart headModelPart) {
    AnimationUtils.animateCrossbowHold(rightArmPart, leftArmPart, headModelPart, true);
  }

  static void animateHumanoidModelArmPoseDancing(
      ModelPart headModelPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float ageInTicks) {
    float swingAmount = ageInTicks / 60.0F;
    headModelPart.x = Mth.sin(swingAmount * 10.0F);
    headModelPart.y = Mth.sin(swingAmount * 40.0F) + 0.4F;
    rightArmPart.zRot = Constants.PI_180DEG * (70.0F + Mth.cos(swingAmount * 40.0F) * 10.0F);
    leftArmPart.zRot = rightArmPart.zRot * -1.0F;
    rightArmPart.y = Mth.sin(swingAmount * 40.0F) * 0.5F + 1.5F;
    leftArmPart.y = Mth.sin(swingAmount * 40.0F) * 0.5F + 1.5F;
    bodyPart.y = Mth.sin(swingAmount * 40.0F) * 0.35F;
  }

  static void animateHumanoidModelArmPoseSpellcasting(
      ModelPart rightArmPart, ModelPart leftArmPart, float ageInTicks) {
    rightArmPart.z = 0.0F;
    rightArmPart.x = -5.0F;
    leftArmPart.z = 0.0F;
    leftArmPart.x = 5.0F;
    rightArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.25F;
    leftArmPart.xRot = Mth.cos(ageInTicks * 0.6662F) * 0.25F;
    rightArmPart.zRot = 2.3561945F;
    leftArmPart.zRot = -2.3561945F;
    rightArmPart.yRot = 0.0F;
    leftArmPart.yRot = 0.0F;
  }

  static void animateHumanoidModelArmPoseSpyglass(
      ModelPart rightArmPart, ModelPart leftArmPart, float ageInTicks) {
    AnimationUtils.bobModelPart(rightArmPart, ageInTicks, 1.0F);
  }

  static boolean animateHumanoidArmPose(
      Mob mob,
      ModelArmPose modelArmPose,
      ModelPart headModelPart,
      ModelPart bodyPart,
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float attackTime,
      float ageInTicks) {
    switch (modelArmPose) {
      case ATTACKING -> animateHumanoidModelArmPoseAttacking(
          rightArmPart, leftArmPart, mob, attackTime, ageInTicks);
      case ATTACKING_WITH_MELEE_WEAPON -> animateHumanoidModelArmPoseAttackingWithMeleeWeapon(
          rightArmPart, leftArmPart, mob, attackTime, ageInTicks);
      case BOW_AND_ARROW ->
          animateHumanoidModelArmPoseBowAndArrow(rightArmPart, leftArmPart, headModelPart);
      case CELEBRATING ->
          animateHumanoidModelArmPoseCelebrating(rightArmPart, leftArmPart, ageInTicks);
      case CROSSBOW_CHARGE ->
          animateHumanoidModelArmPoseCrossbowCharge(rightArmPart, leftArmPart, mob);
      case CROSSBOW_HOLD ->
          animateHumanoidModelArmPoseCrossbowHold(rightArmPart, leftArmPart, headModelPart);
      case DANCING -> animateHumanoidModelArmPoseDancing(
          headModelPart, bodyPart, rightArmPart, leftArmPart, ageInTicks);
      case SPELLCASTING ->
          animateHumanoidModelArmPoseSpellcasting(rightArmPart, leftArmPart, ageInTicks);
      case SPYGLASS -> animateHumanoidModelArmPoseSpyglass(rightArmPart, leftArmPart, ageInTicks);
      default -> {
        return false;
      }
    }
    return true;
  }
}
