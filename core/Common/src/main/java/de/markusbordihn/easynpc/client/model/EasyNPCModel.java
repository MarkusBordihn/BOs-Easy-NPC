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

import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import java.util.UUID;

public class EasyNPCModel {

  /**
   * Setup Animation for Model.
   *
   * @param extension the EasyNPC render state extension
   * @param modelManager the model manager
   */
  public static boolean setupAnimationStart(
      final EasyNPCRenderStateExtension extension, final EasyNPCModelManager modelManager) {
    if (extension == null || modelManager == null) {
      return false;
    }

    // Get EasyNPC
    EasyNPC<?> easyNPC = getEasyNPC(extension);
    if (easyNPC == null) {
      return false;
    }

    // Get Model Data
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
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
      final ModelData<?> modelData,
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

  /**
   * Get EasyNPC from EasyNPCRenderStateExtension.
   *
   * @param extension the EasyNPC render state extension
   * @return EasyNPC or null if not found
   */
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
}
