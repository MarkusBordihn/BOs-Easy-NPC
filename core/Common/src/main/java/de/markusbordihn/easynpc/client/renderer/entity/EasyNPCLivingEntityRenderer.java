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
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.client.texture.LivingEntityTextureManager;
import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.resources.Identifier;

public class EasyNPCLivingEntityRenderer {

  public static void handleRenderStart(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {}

  public static void handleRenderEnd(
      LivingEntityRenderState renderState,
      PoseStack poseStack,
      MultiBufferSource bufferSource,
      int packedLight) {}

  public static void handleScale(LivingEntityRenderState renderState, PoseStack poseStack) {
    EasyNPC<?> easyNPC = getEasyNPC(renderState);
    if (easyNPC == null) {
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

  public static EasyNPC<?> getEasyNPC(LivingEntityRenderState livingEntityRenderState) {
    if (livingEntityRenderState
        instanceof EasyNPCRenderStateExtension easyNPCRenderStateExtension) {
      UUID uuid = easyNPCRenderStateExtension.getEasyNpcUUID();
      if (uuid != null) {
        return LivingEntityManager.getClientEasyNPCEntityByUUID(uuid);
      }
    }
    return null;
  }

  public static Identifier getTexture(
      LivingEntityRenderState livingEntityRenderState, Identifier defaultTexture) {
    if (livingEntityRenderState
        instanceof EasyNPCRenderStateExtension easyNPCRenderStateExtension) {
      Identifier texture = easyNPCRenderStateExtension.getEasyNpcTexture();
      if (texture != null) {
        return texture;
      }
    }
    return defaultTexture;
  }

  public static Identifier extractEntityTexture(EasyNPC<?> easyNPC, Identifier defaultTexture) {
    return LivingEntityTextureManager.getEntityTexture(easyNPC, defaultTexture);
  }

  public static Identifier extractEntityPlayerTexture(
      EasyNPC<?> easyNPC, Identifier defaultTexture) {
    return LivingEntityTextureManager.getEntityPlayerTexture(easyNPC, defaultTexture);
  }

  public static Identifier extractEntityTextureWithDefaultCallback(
      EasyNPC<?> easyNPC, Identifier defaultTexture, Supplier<Identifier> defaultTextureSupplier) {
    return LivingEntityTextureManager.getEntityTextureWithDefaultCallback(
        easyNPC, defaultTexture, defaultTextureSupplier);
  }

  public static void extractAndCacheTexture(
      EasyNPC<?> easyNPC,
      EasyNPCRenderStateExtension renderStateExtension,
      LivingEntityRenderState renderState,
      LivingEntityRenderer<?, ?, ?> renderer) {
    if (renderer instanceof EasyNPCEntityRenderer easyNPCRenderer) {
      Identifier defaultTexture = easyNPCRenderer.getDefaultTexture();
      renderStateExtension.setEasyNpcTexture(
          easyNPCRenderer.supportsPlayerSkins()
              ? extractEntityPlayerTexture(easyNPC, defaultTexture)
              : extractEntityTexture(easyNPC, defaultTexture));
    }
  }

  public static void handleRotation(LivingEntityRenderState renderState, PoseStack poseStack) {
    handleRotation(getEasyNPC(renderState), poseStack);
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
