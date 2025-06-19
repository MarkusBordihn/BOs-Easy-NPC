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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.EnumMap;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCModelManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final ModelPart rootModelPart;
  private final Function<ResourceLocation, RenderType> renderType;
  private final Map<ModelPartType, CustomPosition> defaultModelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, CustomRotation> defaultModelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, Boolean> defaultModelPartVisibilityMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);

  public EasyNPCModelManager(final ModelPart rootModelPart) {
    this(rootModelPart, RenderType::entityCutoutNoCull);
  }

  public EasyNPCModelManager(
      final ModelPart rootModelPart, final Function<ResourceLocation, RenderType> renderType) {
    this.rootModelPart = rootModelPart;
    this.renderType = renderType;
  }

  public EasyNPCModelManager defineModelPart(
      final ModelPartType modelPartType, final String modelPartName) {
    if (this.rootModelPart != null && this.rootModelPart.hasChild(modelPartName)) {
      return defineModelPart(modelPartType, this.rootModelPart.getChild(modelPartName));
    } else {
      log.error(
          "Model part '{}' not found for model part type '{}' in {}.",
          modelPartName,
          modelPartType.getTagName(),
          this.rootModelPart);
    }
    return this;
  }

  public EasyNPCModelManager defineModelPart(
      final ModelPartType modelPartType, final ModelPart modelPart) {
    setDefaultModelPartPosition(
        modelPartType, new CustomPosition(modelPart.x, modelPart.y, modelPart.z));
    setDefaultModelPartRotation(
        modelPartType, new CustomRotation(modelPart.xRot, modelPart.yRot, modelPart.zRot));
    setDefaultModelPartVisibility(modelPartType, modelPart.visible);
    setDefaultModelPart(modelPartType, modelPart);
    return this;
  }

  public void setDefaultModelPartPosition(
      final ModelPartType modelPartType, final CustomPosition customPosition) {
    defaultModelPartPositionMap.put(modelPartType, customPosition);
  }

  public void setDefaultModelPartRotation(
      final ModelPartType modelPartType, final CustomRotation rotation) {
    defaultModelPartRotationMap.put(modelPartType, rotation);
  }

  public void setDefaultModelPartVisibility(
      final ModelPartType modelPartType, final boolean isVisible) {
    defaultModelPartVisibilityMap.put(modelPartType, isVisible);
  }

  public void setDefaultModelPart(final ModelPartType modelPartType, final ModelPart modelPart) {
    modelPartMap.put(modelPartType, modelPart);
  }

  public ModelPart getModelPart(final ModelPartType modelPartType) {
    return modelPartMap.get(modelPartType);
  }

  public boolean setupModelParts(final ModelDataCapable<?> modelData) {
    if (modelData == null || modelData.getModelPose() == ModelPose.DEFAULT) {
      return false;
    }

    boolean hasChangedModelPart = false;
    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      // Check if model part is available.
      Boolean isVisible = modelData.getModelPartVisibility(partType);
      if (Boolean.FALSE.equals(isVisible)) {
        modelPart.visible = false;
        continue;
      } else if (Boolean.TRUE.equals(isVisible)) {
        modelPart.visible = true;
      }

      // Handle custom position.
      CustomPosition customPosition = modelData.getModelPartPosition(partType);
      if (customPosition != null && customPosition.hasChanged()) {
        modelPart.x = customPosition.x();
        modelPart.y = customPosition.y();
        modelPart.z = customPosition.z();
        hasChangedModelPart = true;
      }

      // Handle custom rotation.
      CustomRotation customRotation = modelData.getModelPartRotation(partType);
      if (customRotation != null && customRotation.hasChanged()) {
        modelPart.xRot = customRotation.x();
        modelPart.yRot = customRotation.y();
        modelPart.zRot = customRotation.z();
        hasChangedModelPart = true;
      }

      // Handle custom scale.
      CustomScale customScale = modelData.getModelPartScale(partType);
      if (customScale != null && customScale.hasChanged()) {
        modelPart.xScale = customScale.x();
        modelPart.yScale = customScale.y();
        modelPart.zScale = customScale.z();
        hasChangedModelPart = true;
      }
    }
    return hasChangedModelPart;
  }

  public void resetModelParts() {
    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType modelPartType = entry.getKey();
      ModelPart modelPartToRest = entry.getValue();
      CustomPosition customPosition = defaultModelPartPositionMap.get(modelPartType);
      CustomRotation customRotation = defaultModelPartRotationMap.get(modelPartType);
      Boolean isVisible = defaultModelPartVisibilityMap.get(modelPartType);

      if (customPosition != null) {
        modelPartToRest.setPos(customPosition.x(), customPosition.y(), customPosition.z());
      }
      if (customRotation != null) {
        modelPartToRest.setRotation(customRotation.x(), customRotation.y(), customRotation.z());
      }
      if (isVisible != null) {
        modelPartToRest.visible = isVisible;
      }
    }
  }
}
