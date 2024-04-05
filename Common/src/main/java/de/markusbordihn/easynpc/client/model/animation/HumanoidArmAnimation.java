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

import net.minecraft.client.model.AnimationUtils;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.util.Mth;

public interface HumanoidArmAnimation {

  static void animateHumanoidModelLeftArmSwing(
      ModelPart leftArmPart, float ageInTicks, float limbSwing, float limbSwingAmount) {
    leftArmPart.xRot =
        Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 2.0F * limbSwingAmount * 0.5F;
    if (limbSwing <= 0.0F && limbSwingAmount <= 0.0F) {
      AnimationUtils.bobModelPart(leftArmPart, ageInTicks, -1.0F);
    }
  }

  static void animateHumanoidModelRightArmSwing(
      ModelPart rightArmPart, float ageInTicks, float limbSwing, float limbSwingAmount) {
    rightArmPart.xRot = Mth.cos(limbSwing * 0.6662F) * 2.0F * limbSwingAmount * 0.5F;
    if (limbSwing <= 0.0F && limbSwingAmount <= 0.0F) {
      AnimationUtils.bobModelPart(rightArmPart, ageInTicks, 1.0F);
    }
  }

  static boolean animateHumanoidModelArms(
      ModelPart rightArmPart,
      ModelPart leftArmPart,
      float ageInTicks,
      float limbSwing,
      float limbSwingAmount) {
    boolean result = false;
    if (rightArmPart != null) {
      animateHumanoidModelRightArmSwing(rightArmPart, ageInTicks, limbSwing, limbSwingAmount);
      result = true;
    }
    if (leftArmPart != null) {
      animateHumanoidModelLeftArmSwing(leftArmPart, ageInTicks, limbSwing, limbSwingAmount);
      result = true;
    }
    return result;
  }
}
