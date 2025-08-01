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

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Axis;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.LightLayer;

public class EasyNPCModel {

  public static boolean setupAnimationStart(
      final EasyNPCRenderStateExtension extension, EasyNPCModelManager modelManager) {
    if (extension == null || modelManager == null) {
      return false;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return false;
    }

    // Get Model Data
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return false;
    }

    return setupAnimation(easyNPC, modelData, modelManager);
  }

  public static boolean setupAnimation(
      final EasyNPC<?> easyNPC,
      final ModelDataCapable<?> modelData,
      final EasyNPCModelManager modelManager) {
    if (easyNPC == null || modelData == null || modelManager == null) {
      return false;
    }

    // Early return if no custom model pose is used.
    if (modelData.getModelPose() == ModelPose.DEFAULT) {
      return false;
    }

    // Handle Model Pose
    modelManager.resetModelParts();
    return modelManager.setupModelParts(modelData);
  }

  public static EasyNPC<?> getEasyNPC(final EasyNPCRenderStateExtension extension) {
    if (extension == null) {
      return null;
    }

    UUID uuid = extension.getEasyNpcUUID();
    if (uuid == null) {
      return null;
    }

    return LivingEntityManager.getEasyNPCEntityByUUID(uuid);
  }

  public static int getEntityLightLevel(
      final EasyNPC<?> easyNPC,
      final DisplayAttributeDataCapable<?> displayAttributeData,
      final BlockPos blockPos) {
    if (easyNPC == null || displayAttributeData == null || blockPos == null) {
      return 0;
    }
    int entityLightLevel =
        displayAttributeData.getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL);
    if (entityLightLevel > 0) {
      return entityLightLevel;
    }

    return easyNPC.getLivingEntity().level().getBrightness(LightLayer.BLOCK, blockPos);
  }

  public static void renderEntityNameTag(
      final EasyNPCRenderStateExtension extension,
      final PoseStack poseStack) {

    if (extension == null) {
      return ;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return;
    }

    // Get Model Data
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    CustomRotation rootRotation = modelData.getModelPartRotation(ModelPartType.ROOT);
    if (rootRotation != null) {
      poseStack.translate(0, 1, 0);
      poseStack.mulPose(Axis.XP.rotationDegrees(-rootRotation.x()));
      poseStack.mulPose(Axis.YP.rotationDegrees(-rootRotation.y()));
      poseStack.mulPose(Axis.ZP.rotationDegrees(-rootRotation.z()));
      poseStack.translate(0, -1, 0);
    }
  }
}
