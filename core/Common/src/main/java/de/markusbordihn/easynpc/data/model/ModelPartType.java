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
  // Root part (used as base for animations or transformations)
  ROOT("Root"),

  // Head parts
  HEAD("Head"),
  HAT("Hat"),
  HELMET("Helmet"),

  // Body parts
  BODY("Body"),
  CHESTPLATE("Chestplate"),

  // Outer body layer
  BODY_JACKET("BodyJacket"),

  // Arm parts
  RIGHT_ARM("RightArm"),
  LEFT_ARM("LeftArm"),
  ARMS("Arms"), // combined arms (e.g. crossed arms)

  // Outer arm layers
  RIGHT_SLEEVE("RightSleeve"),
  LEFT_SLEEVE("LeftSleeve"),

  // Wings
  RIGHT_WING("RightWing"),
  LEFT_WING("LeftWing"),

  // Leg parts
  RIGHT_LEG("RightLeg"),
  LEFT_LEG("LeftLeg"),
  LEGGINGS("Leggings"),
  BOOTS("Boots"),

  // Outer leg layers
  RIGHT_PANTS("RightPants"),
  LEFT_PANTS("LeftPants"),

  // Quadruped front legs
  RIGHT_FRONT_LEG("RightFrontLeg"),
  LEFT_FRONT_LEG("LeftFrontLeg"),

  // Quadruped hind legs
  RIGHT_HIND_LEG("RightHindLeg"),
  LEFT_HIND_LEG("LeftHindLeg"),

  // Tail parts
  TAIL("Tail"),
  TAIL1("Tail1"),
  TAIL2("Tail2"),

  // Fallback / unknown part
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
      // Alternative search for model part tag name.
      for (ModelPartType modelPartTypeEnum : ModelPartType.values()) {
        if (modelPartTypeEnum.tagName.equalsIgnoreCase(modelPart)) {
          return modelPartTypeEnum;
        }
      }

      // Return unknown model part if no match was found.
      return ModelPartType.UNKNOWN;
    }
  }

  public String getTagName() {
    return this.tagName;
  }
}
