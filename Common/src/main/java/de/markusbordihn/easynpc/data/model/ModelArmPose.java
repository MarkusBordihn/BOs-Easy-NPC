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

package de.markusbordihn.easynpc.data.model;

public enum ModelArmPose {
  // @formatter:off
  DEFAULT,
  DANCING,
  BOW_AND_ARROW,
  SPELLCASTING,
  CELEBRATING,
  CROSSBOW_CHARGE,
  CROSSBOW_HOLD,
  ATTACKING,
  ATTACKING_WITH_MELEE_WEAPON,
  CROSSED,
  NEUTRAL,
  CUSTOM;

  // @formatter:on

  public static ModelArmPose get(String armPose) {
    if (armPose == null || armPose.isEmpty()) {
      return ModelArmPose.DEFAULT;
    }
    try {
      return ModelArmPose.valueOf(armPose);
    } catch (IllegalArgumentException e) {
      return ModelArmPose.DEFAULT;
    }
  }
}
