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

package de.markusbordihn.easynpc.client.renderer.entity;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.renderer.MultiBufferSource;

public class EasyNPCLivingEntityRenderer {

  public static void handleRenderStart(
      EasyNPC<?> easyNPC, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {}

  public static void handleRenderEnd(
      EasyNPC<?> easyNPC, PoseStack poseStack, MultiBufferSource bufferSource, int packedLight) {}

  public static void handleScale(EasyNPC<?> easyNPC, PoseStack poseStack) {
    if (easyNPC == null || poseStack == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    // Apply default scale to the model.
    CustomScale defaultScale = modelData.getDefaultModelScale();
    if (defaultScale != null && defaultScale.hasChanged()) {
      poseStack.scale(defaultScale.x(), defaultScale.y(), defaultScale.z());
    }

    // Apply custom root scale to the model.
    RootModelData rootModelData = modelData.getModelRootData();
    CustomScale customScale = rootModelData.scale();
    if (customScale.hasChanged()) {
      poseStack.scale(customScale.x(), customScale.y(), customScale.z());
    }
  }

  public static void handleRotation(EasyNPC<?> easyNPC, PoseStack poseStack) {
    if (easyNPC == null || poseStack == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    CustomRotation rootRotation = modelData.getModelRootData().rotation();
    if (!rootRotation.hasChangedRotation()) {
      return;
    }

    float xDeg = (float) Math.toDegrees(rootRotation.x());
    float zDeg = (float) Math.toDegrees(rootRotation.z());

    if (xDeg != 0.0f || zDeg != 0.0f) {
      float pivotY = easyNPC.getLivingEntity().getBbHeight() * 0.5f;
      poseStack.translate(0.0f, pivotY, 0.0f);
      if (xDeg != 0.0f) {
        poseStack.mulPose(Axis.XP.rotationDegrees(xDeg));
      }
      if (zDeg != 0.0f) {
        poseStack.mulPose(Axis.ZP.rotationDegrees(zDeg));
      }
      poseStack.translate(0.0f, -pivotY, 0.0f);
    }
  }
}
