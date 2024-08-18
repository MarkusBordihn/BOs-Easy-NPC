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

package de.markusbordihn.easynpc.client.model;

import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import net.minecraft.client.model.geom.ModelPart;

public class ModelHelper {

  protected ModelHelper() {}

  public static void setPosition(ModelPart modelPart, float x, float y, float z) {
    modelPart.x = x;
    modelPart.y = y;
    modelPart.z = z;
  }

  public static void setPositionRotationVisibility(
      ModelPart modelPart, CustomPosition position, CustomRotation rotations, boolean visible) {
    if (visible) {
      if (position != null) {
        modelPart.x += position.x();
        modelPart.y += position.y();
        modelPart.z += position.z();
      }
      if (rotations != null) {
        modelPart.xRot += rotations.x();
        modelPart.yRot += rotations.y();
        modelPart.zRot += rotations.z();
      }
    }
    modelPart.visible = visible;
  }

  public static boolean hasModelPart(ModelPart parentModelPart, String name) {
    if (parentModelPart == null || name == null || name.isEmpty()) {
      return false;
    }
    try {
      parentModelPart.getChild(name);
      return true;
    } catch (Exception e) {
      return false;
    }
  }
}
