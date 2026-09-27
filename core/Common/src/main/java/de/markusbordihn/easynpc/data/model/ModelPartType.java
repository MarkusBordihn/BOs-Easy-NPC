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

import java.util.Locale;

public enum ModelPartType {
  ROOT("Root"),
  HEAD("Head"),
  HAT("Hat"),
  HELMET("Helmet"),
  BODY("Body"),
  CHESTPLATE("Chestplate"),
  BODY_JACKET("BodyJacket"),
  RIGHT_ARM("RightArm"),
  LEFT_ARM("LeftArm"),
  ARMS("Arms"),
  RIGHT_SLEEVE("RightSleeve"),
  LEFT_SLEEVE("LeftSleeve"),
  RIGHT_WING("RightWing"),
  LEFT_WING("LeftWing"),
  RIGHT_LEG("RightLeg"),
  LEFT_LEG("LeftLeg"),
  LEGGINGS("Leggings"),
  BOOTS("Boots"),
  RIGHT_PANTS("RightPants"),
  LEFT_PANTS("LeftPants"),
  RIGHT_FRONT_LEG("RightFrontLeg"),
  LEFT_FRONT_LEG("LeftFrontLeg"),
  RIGHT_HIND_LEG("RightHindLeg"),
  LEFT_HIND_LEG("LeftHindLeg"),
  TAIL("Tail"),
  TAIL1("Tail1"),
  TAIL2("Tail2"),
  UNKNOWN("Unknown");

  public final String tagName;

  ModelPartType(String tagName) {
    this.tagName = tagName;
  }

  public static ModelPartType get(String modelPart) {
    if (modelPart == null || modelPart.isEmpty()) {
      return ModelPartType.UNKNOWN;
    }

    try {
      return ModelPartType.valueOf(modelPart.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException e) {
      for (ModelPartType modelPartTypeEnum : ModelPartType.values()) {
        if (modelPartTypeEnum.tagName.equalsIgnoreCase(modelPart)) {
          return modelPartTypeEnum;
        }
      }

      return ModelPartType.UNKNOWN;
    }
  }

  public String getTagName() {
    return this.tagName;
  }
}
