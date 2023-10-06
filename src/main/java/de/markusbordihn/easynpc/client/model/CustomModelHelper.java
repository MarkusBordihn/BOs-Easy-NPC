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

package de.markusbordihn.easynpc.client.model;

import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.core.Rotations;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.data.CustomPosition;

@OnlyIn(Dist.CLIENT)
public class CustomModelHelper {

  public static final void resetRotation(ModelPart modelPart) {
    modelPart.xRot = 0.0F;
    modelPart.yRot = 0.0F;
    modelPart.zRot = 0.0F;
  }

  public static final void setRotation(ModelPart modelPart, float x, float y, float z) {
    modelPart.xRot = x;
    modelPart.yRot = y;
    modelPart.zRot = z;
  }

  public static final void setRotation(ModelPart modelPart, Rotations rotations) {
    modelPart.xRot = rotations.getX();
    modelPart.yRot = rotations.getY();
    modelPart.zRot = rotations.getZ();
  }

  public static final void resetPosition(ModelPart modelPart) {
    modelPart.x = 0.0F;
    modelPart.y = 0.0F;
    modelPart.z = 0.0F;
  }

  public static final void setPosition(ModelPart modelPart, float x, float y, float z) {
    modelPart.x = x;
    modelPart.y = y;
    modelPart.z = z;
  }

  public static final void setPosition(ModelPart modelPart, CustomPosition position) {
    modelPart.x = position.x();
    modelPart.y = position.y();
    modelPart.z = position.z();
  }

  public static final void setPositionRotationVisibility(ModelPart modelPart,
      CustomPosition position, Rotations rotations, boolean visible) {
    if (visible) {
      if (position != null) {
        modelPart.x += position.x();
        modelPart.y += position.y();
        modelPart.z += position.z();
      }
      if (rotations != null) {
        modelPart.xRot += rotations.getX();
        modelPart.yRot += rotations.getY();
        modelPart.zRot += rotations.getZ();
      }
    } else {
      modelPart.visible = false;
    }
  }

  public static final void setHeadPositionRotationVisibility(ModelPart modelPart,
      CustomPosition position, Rotations rotations, boolean visible, float netHeadYaw,
      float headPitch) {
    setPositionRotationVisibility(modelPart, position, rotations, visible);
    if (visible && (rotations == null
        || (rotations.getX() == 0.0f && rotations.getY() == 0.0f && rotations.getZ() == 0.0f))) {
      modelPart.yRot = netHeadYaw * ((float) Math.PI / 180F);
      modelPart.xRot = headPitch * ((float) Math.PI / 180F);
    }
  }

}
