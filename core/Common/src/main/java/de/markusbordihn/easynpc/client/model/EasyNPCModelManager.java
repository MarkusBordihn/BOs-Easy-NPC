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
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.resources.ResourceLocation;

public class EasyNPCModelManager {

  private final ModelPart modelPart;
  private final Function<ResourceLocation, RenderType> renderType;
  private Map<ModelPartType, CustomPosition> modelPartPositionMap = new HashMap<>();
  private Map<ModelPartType, CustomRotation> modelPartRotationMap = new HashMap<>();
  private Map<ModelPartType, Boolean> modelPartVisibilityMap = new HashMap<>();
  private Map<ModelPartType, ModelPart> modelPartMap = new HashMap<>();

  public EasyNPCModelManager(ModelPart modelPart) {
    this(modelPart, RenderType::entityCutoutNoCull);
  }

  public EasyNPCModelManager(
      ModelPart modelPart, Function<ResourceLocation, RenderType> renderType) {
    this.modelPart = modelPart;
    this.renderType = renderType;
  }

  public ModelPart defineModelPart(ModelPartType modelPartType, ModelPart modelPart) {
    setDefaultModelPartPosition(
        modelPartType, new CustomPosition(modelPart.x, modelPart.y, modelPart.z));
    setDefaultModelPartRotation(
        modelPartType, new CustomRotation(modelPart.xRot, modelPart.yRot, modelPart.zRot));
    setDefaultModelPartVisibility(modelPartType, modelPart.visible);
    setDefaultModelPart(modelPartType, modelPart);
    return modelPart;
  }

  public void setDefaultModelPartPosition(
      final ModelPartType modelPartType, final CustomPosition customPosition) {
    modelPartPositionMap.put(modelPartType, customPosition);
  }

  public void setDefaultModelPartRotation(
      final ModelPartType modelPartType, final CustomRotation rotation) {
    modelPartRotationMap.put(modelPartType, rotation);
  }

  public void setDefaultModelPartVisibility(
      final ModelPartType modelPartType, final boolean isVisible) {
    modelPartVisibilityMap.put(modelPartType, isVisible);
  }

  public void setDefaultModelPart(final ModelPartType modelPartType, final ModelPart modelPart) {
    modelPartMap.put(modelPartType, modelPart);
  }

  public ModelPart getModelPart(final ModelPartType modelPartType) {
    return modelPartMap.get(modelPartType);
  }

  public void resetModelParts() {
    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType modelPartType = entry.getKey();
      ModelPart modelPart = entry.getValue();
      CustomPosition customPosition = modelPartPositionMap.get(modelPartType);
      CustomRotation customRotation = modelPartRotationMap.get(modelPartType);
      Boolean isVisible = modelPartVisibilityMap.get(modelPartType);

      if (customPosition != null) {
        modelPart.setPos(customPosition.x(), customPosition.y(), customPosition.z());
      }
      if (customRotation != null) {
        modelPart.setRotation(customRotation.x(), customRotation.y(), customRotation.z());
      }
      if (isVisible != null) {
        modelPart.visible = isVisible;
      }
    }
  }
}
