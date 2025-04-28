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

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.client.model.HumanoidModel;
import net.minecraft.client.model.VillagerModel;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.client.renderer.entity.state.VillagerRenderState;

public class EasyNPCModel {

  public static boolean setupAnimation(
      EasyNPC<?> easyNPC,
      EasyNPCModelManager modelManager,
      VillagerModel model,
      VillagerRenderState renderState) {
    if (easyNPC == null || model == null) {
      return false;
    }

    // Get Model Data
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return false;
    }

    // Get Model Pose
    ModelPose modelPose = modelData.getModelPose();
    if (modelPose == null || modelPose == ModelPose.DEFAULT) {
      return false;
    }

    // Reset Model
    modelManager.resetModelParts();

    // Handle Model Position, Rotation and Visibility
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.HEAD),
        modelData.getModelHeadPosition(),
        modelData.getModelHeadRotation(),
        modelData.isModelHeadVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.BODY),
        modelData.getModelBodyPosition(),
        modelData.getModelBodyRotation(),
        modelData.isModelBodyVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.ARMS),
        modelData.getModelArmsPosition(),
        modelData.getModelArmsRotation(),
        modelData.isModelArmsVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.LEFT_LEG),
        modelData.getModelLeftLegPosition(),
        modelData.getModelLeftLegRotation(),
        modelData.isModelLeftLegVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.RIGHT_LEG),
        modelData.getModelRightLegPosition(),
        modelData.getModelRightLegRotation(),
        modelData.isModelRightLegVisible());

    return true;
  }

  public static boolean setupAnimation(
      EasyNPC<?> easyNPC,
      EasyNPCModelManager modelManager,
      HumanoidModel<?> model,
      HumanoidRenderState renderState) {
    if (easyNPC == null || model == null) {
      return false;
    }

    // Get Model Data
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return false;
    }

    // Get Model Pose
    ModelPose modelPose = modelData.getModelPose();
    if (modelPose == null || modelPose == ModelPose.DEFAULT) {
      return false;
    }

    // Reset Model
    modelManager.resetModelParts();

    // Handle Model Position, Rotation and Visibility
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.HEAD),
        modelData.getModelHeadPosition(),
        modelData.getModelHeadRotation(),
        modelData.isModelHeadVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.BODY),
        modelData.getModelBodyPosition(),
        modelData.getModelBodyRotation(),
        modelData.isModelBodyVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.LEFT_ARM),
        modelData.getModelLeftArmPosition(),
        modelData.getModelLeftArmRotation(),
        modelData.isModelLeftArmVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.RIGHT_ARM),
        modelData.getModelRightArmPosition(),
        modelData.getModelRightArmRotation(),
        modelData.isModelRightArmVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.LEFT_LEG),
        modelData.getModelLeftLegPosition(),
        modelData.getModelLeftLegRotation(),
        modelData.isModelLeftLegVisible());
    ModelHelper.setPositionRotationVisibility(
        modelManager.getModelPart(ModelPartType.RIGHT_LEG),
        modelData.getModelRightLegPosition(),
        modelData.getModelRightLegRotation(),
        modelData.isModelRightLegVisible());

    return true;
  }
}
