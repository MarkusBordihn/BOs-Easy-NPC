/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.model.ModelArmPose;
import net.minecraft.world.entity.HumanoidArm;

/**
 * Interface for providing arm poses for NPCs in 1.21.4. This replaces the mixin-based approach used
 * in previous versions.
 */
public interface ArmPoseProvider {

  /**
   * Gets the arm pose for the specified arm. This method should be implemented by NPCs that need
   * custom arm poses.
   *
   * @param humanoidArm the arm to get the pose for
   * @return the ModelArmPose for the specified arm
   */
  ModelArmPose getArmPose(HumanoidArm humanoidArm);

  /**
   * Gets the main hand arm pose. This is a convenience method that calls getArmPose with the main
   * hand.
   *
   * @return the ModelArmPose for the main hand
   */
  default ModelArmPose getMainHandArmPose() {
    return getArmPose(getMainArm());
  }

  /**
   * Gets the off hand arm pose. This is a convenience method that calls getArmPose with the off
   * hand.
   *
   * @return the ModelArmPose for the off hand
   */
  default ModelArmPose getOffHandArmPose() {
    return getArmPose(getMainArm().getOpposite());
  }

  /**
   * Gets the main arm of this entity. Override this if your entity has a different main arm.
   *
   * @return the main arm
   */
  default HumanoidArm getMainArm() {
    return HumanoidArm.RIGHT;
  }
}
