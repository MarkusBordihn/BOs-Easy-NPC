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
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;

public class EasyNPCModel {

  public static boolean setupAnimationStart(EasyNPC<?> easyNPC, EasyNPCModelManager modelManager) {
    if (easyNPC == null || modelManager == null) {
      return false;
    }

    // Get Model Data
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return false;
    }

    return setupAnimation(easyNPC, modelData, modelManager);
  }

  /**
   * Setup Animation for Model.
   *
   * @param easyNPC the EasyNPC entity
   * @param modelManager the model manager
   * @return true if model was adjusted, false otherwise
   */
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
}
