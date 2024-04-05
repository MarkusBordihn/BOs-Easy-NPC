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

import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.util.Mth;

public interface HumanoidLegAnimation {

  static void animateHumanoidModelLeftLegSwing(
      ModelPart rightLegPart, float limbSwing, float limbSwingAmount) {
    rightLegPart.xRot = Mth.cos(limbSwing * 0.6662F) * 1.4F * limbSwingAmount;
  }

  static void animateHumanoidModelRightLegSwing(
      ModelPart leftLegPart, float limbSwing, float limbSwingAmount) {
    leftLegPart.xRot = Mth.cos(limbSwing * 0.6662F + (float) Math.PI) * 1.4F * limbSwingAmount;
  }

  static void animateHumanoidModelLegs(
      ModelPart rightLegPart, ModelPart leftLegPart, float limbSwing, float limbSwingAmount) {
    if (rightLegPart != null) {
      animateHumanoidModelRightLegSwing(rightLegPart, limbSwing, limbSwingAmount);
    }
    if (leftLegPart != null) {
      animateHumanoidModelLeftLegSwing(leftLegPart, limbSwing, limbSwingAmount);
    }
  }
}
