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
  private final Map<ModelPartType, CustomScale> defaultModelPartScaleMap =
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
    }
    log.error(
        "Model part '{}' not found for model part type '{}' in {}.",
        modelPartName,
        modelPartType.getTagName(),
        this.rootModelPart);
    return this;
  }

  public EasyNPCModelManager defineModelPart(
      final ModelPartType modelPartType, final ModelPart modelPart) {
    if (modelPart == null) {
      log.error("Model part for model part type '{}' is null and can't be defined.", modelPartType);
      return this;
    }
    if (!defaultModelPartPositionMap.containsKey(modelPartType)) {
      setDefaultModelPartPosition(
          modelPartType, new CustomPosition(modelPart.x, modelPart.y, modelPart.z));
    }
    if (!defaultModelPartRotationMap.containsKey(modelPartType)) {
      setDefaultModelPartRotation(
          modelPartType, new CustomRotation(modelPart.xRot, modelPart.yRot, modelPart.zRot));
    }
    if (!defaultModelPartScaleMap.containsKey(modelPartType)) {
      setDefaultModelPartScale(
          modelPartType, new CustomScale(modelPart.xScale, modelPart.yScale, modelPart.zScale));
    }
    if (!defaultModelPartVisibilityMap.containsKey(modelPartType)) {
      setDefaultModelPartVisibility(modelPartType, modelPart.visible);
    }
    if (!modelPartMap.containsKey(modelPartType)) {
      setDefaultModelPart(modelPartType, modelPart);
    }
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

  public void setDefaultModelPartScale(
      final ModelPartType modelPartType, final CustomScale customScale) {
    defaultModelPartScaleMap.put(modelPartType, customScale);
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

  public boolean setupModelParts(
      final ModelDataCapable<?> modelData, final boolean applyVisibility) {
    if (modelData == null || modelData.getModelPose() == ModelPose.DEFAULT) {
      return false;
    }

    EnumMap<ModelPartType, Boolean> visibilityMap = modelData.getModelPartVisibility();
    boolean hasChangedModelPart = false;

    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      // Skip outer layer parts as they will be synced from their inner parts
      if (isOuterLayerPart(partType)) {
        continue;
      }

      // Check if model part is available.
      if (applyVisibility) {
        Boolean visibility = visibilityMap.get(partType);
        if (Boolean.FALSE.equals(visibility)) {
          modelPart.visible = false;
          continue;
        } else if (Boolean.TRUE.equals(visibility)
            && Boolean.TRUE.equals(defaultModelPartVisibilityMap.get(partType))) {
          modelPart.visible = true;
        }
      }

      // Handle custom position.
      CustomPosition customPosition = modelData.getModelPartPosition(partType);
      if (customPosition != null && customPosition.hasChanged()) {
        modelPart.x += customPosition.x();
        modelPart.y += customPosition.y();
        modelPart.z += customPosition.z();
        hasChangedModelPart = true;
      }

      // Handle custom rotation.
      CustomRotation customRotation = modelData.getModelPartRotation(partType);
      if (customRotation != null && customRotation.hasChanged()) {
        modelPart.xRot += customRotation.x();
        modelPart.yRot += customRotation.y();
        modelPart.zRot += customRotation.z();
        hasChangedModelPart = true;
      }

      // Handle custom scale.
      CustomScale customScale = modelData.getModelPartScale(partType);
      if (customScale != null && customScale.hasChanged()) {
        CustomScale defaultScale = defaultModelPartScaleMap.get(partType);
        if (defaultScale != null) {
          modelPart.xScale = defaultScale.x() * customScale.x();
          modelPart.yScale = defaultScale.y() * customScale.y();
          modelPart.zScale = defaultScale.z() * customScale.z();
        } else {
          modelPart.xScale = customScale.x();
          modelPart.yScale = customScale.y();
          modelPart.zScale = customScale.z();
        }
        hasChangedModelPart = true;
      }
    }

    // Sync model parts
    this.syncModelParts(modelData);

    return hasChangedModelPart;
  }

  public void syncModelParts(final ModelDataCapable<?> modelData) {
    if (modelData == null || modelData.getModelPose() == ModelPose.DEFAULT) {
      return;
    }

    EnumMap<ModelPartType, Boolean> visibilityMap = modelData.getModelPartVisibility();
    syncOuterLayer(modelData, visibilityMap, ModelPartType.HAT, ModelPartType.HEAD);
    syncOuterLayer(modelData, visibilityMap, ModelPartType.BODY_JACKET, ModelPartType.BODY);
    syncOuterLayer(modelData, visibilityMap, ModelPartType.LEFT_SLEEVE, ModelPartType.LEFT_ARM);
    syncOuterLayer(modelData, visibilityMap, ModelPartType.RIGHT_SLEEVE, ModelPartType.RIGHT_ARM);
    syncOuterLayer(modelData, visibilityMap, ModelPartType.LEFT_PANTS, ModelPartType.LEFT_LEG);
    syncOuterLayer(modelData, visibilityMap, ModelPartType.RIGHT_PANTS, ModelPartType.RIGHT_LEG);
  }

  private void syncOuterLayer(
      final ModelDataCapable<?> modelData,
      final EnumMap<ModelPartType, Boolean> visibilityMap,
      final ModelPartType outerPartType,
      final ModelPartType innerPartType) {

    ModelPart outerPart = modelPartMap.get(outerPartType);
    ModelPart innerPart = modelPartMap.get(innerPartType);

    // Skip if parts don't exist or outer part is not visible by default
    if (outerPart == null
        || innerPart == null
        || !Boolean.TRUE.equals(defaultModelPartVisibilityMap.get(outerPartType))) {
      return;
    }

    // Check if outer layer is explicitly hidden (set to false)
    if (Boolean.FALSE.equals(visibilityMap.get(outerPartType))) {
      outerPart.visible = false;
    } else {
      if (outerPartType == ModelPartType.HAT && modelData.getModelType().requiresHatSync()) {
        outerPart.copyFrom(innerPart);
      } else {
        outerPart.visible = innerPart.visible;
      }
    }
  }

  public boolean shouldCancelAnimation(final ModelDataCapable<?> modelData) {
    if (modelData == null) {
      return false;
    }

    // Check animation behavior
    switch (modelData.getModelAnimationBehavior()) {
      case NONE:
        return true;
      case DEFAULT:
        return modelData.getModelPose() == ModelPose.CUSTOM;
      case SMART:
      default:
        break;
    }

    // Check if all critical animation parts are modified
    int modifiedCriticalParts = 0;
    int totalCriticalParts = 0;
    for (ModelPartType partType : modelPartMap.keySet()) {
      if (!isCriticalAnimationPart(partType)) {
        continue;
      }

      totalCriticalParts++;

      CustomRotation rotation = modelData.getModelPartRotation(partType);
      CustomPosition position = modelData.getModelPartPosition(partType);
      CustomScale scale = modelData.getModelPartScale(partType);

      if ((rotation != null && rotation.hasChanged())
          || (position != null && position.hasChanged())
          || (scale != null && scale.hasChanged())) {
        modifiedCriticalParts++;
      }
    }

    return totalCriticalParts > 0 && modifiedCriticalParts >= totalCriticalParts;
  }

  private boolean isCriticalAnimationPart(ModelPartType partType) {
    return partType == ModelPartType.HEAD
        || partType == ModelPartType.RIGHT_ARM
        || partType == ModelPartType.LEFT_ARM
        || partType == ModelPartType.RIGHT_LEG
        || partType == ModelPartType.LEFT_LEG
        || partType == ModelPartType.RIGHT_FRONT_LEG
        || partType == ModelPartType.LEFT_FRONT_LEG
        || partType == ModelPartType.RIGHT_HIND_LEG
        || partType == ModelPartType.LEFT_HIND_LEG
        || partType == ModelPartType.ARMS;
  }

  private boolean isOuterLayerPart(ModelPartType partType) {
    return partType == ModelPartType.HAT
        || partType == ModelPartType.LEFT_SLEEVE
        || partType == ModelPartType.RIGHT_SLEEVE
        || partType == ModelPartType.LEFT_PANTS
        || partType == ModelPartType.RIGHT_PANTS
        || partType == ModelPartType.BODY_JACKET;
  }

  public void applySelectiveChanges(final ModelDataCapable<?> modelData) {
    if (modelData == null) {
      return;
    }

    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      if (partType == ModelPartType.HAT) {
        continue;
      }

      CustomScale customScale = modelData.getModelPartScale(partType);
      if (customScale != null && customScale.hasChanged()) {
        CustomScale defaultScale = defaultModelPartScaleMap.get(partType);
        if (defaultScale != null) {
          modelPart.xScale = defaultScale.x() * customScale.x();
          modelPart.yScale = defaultScale.y() * customScale.y();
          modelPart.zScale = defaultScale.z() * customScale.z();
        } else {
          modelPart.xScale = customScale.x();
          modelPart.yScale = customScale.y();
          modelPart.zScale = customScale.z();
        }
      }

      CustomRotation customRotation = modelData.getModelPartRotation(partType);
      if (customRotation != null && customRotation.hasChanged()) {
        modelPart.xRot += customRotation.x();
        modelPart.yRot += customRotation.y();
        modelPart.zRot += customRotation.z();
      }

      CustomPosition customPosition = modelData.getModelPartPosition(partType);
      if (customPosition != null && customPosition.hasChanged()) {
        modelPart.x += customPosition.x();
        modelPart.y += customPosition.y();
        modelPart.z += customPosition.z();
      }
    }

    syncModelParts(modelData);
  }

  public void applyVisibilityChanges(final ModelDataCapable<?> modelData) {
    if (modelData == null) {
      return;
    }

    EnumMap<ModelPartType, Boolean> visibilityMap = modelData.getModelPartVisibility();

    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      // Skip outer layer parts as they will be synced from their inner parts
      if (isOuterLayerPart(partType)) {
        continue;
      }

      // Apply visibility changes
      Boolean visibility = visibilityMap.get(partType);
      if (Boolean.FALSE.equals(visibility)) {
        modelPart.visible = false;
      } else if (Boolean.TRUE.equals(visibility)
          && Boolean.TRUE.equals(defaultModelPartVisibilityMap.get(partType))) {
        modelPart.visible = true;
      }
    }

    syncModelParts(modelData);
  }

  public void resetModelParts() {
    for (Map.Entry<ModelPartType, ModelPart> entry : modelPartMap.entrySet()) {
      ModelPartType modelPartType = entry.getKey();
      ModelPart modelPartToRest = entry.getValue();

      // Skip outer layer parts as they will be synced from their inner parts
      if (isOuterLayerPart(modelPartType)) {
        continue;
      }

      CustomPosition customPosition = defaultModelPartPositionMap.get(modelPartType);
      CustomRotation customRotation = defaultModelPartRotationMap.get(modelPartType);
      CustomScale customScale = defaultModelPartScaleMap.get(modelPartType);
      Boolean isVisible = defaultModelPartVisibilityMap.get(modelPartType);

      if (customPosition != null) {
        modelPartToRest.setPos(customPosition.x(), customPosition.y(), customPosition.z());
      }
      if (customRotation != null) {
        modelPartToRest.setRotation(customRotation.x(), customRotation.y(), customRotation.z());
      }
      if (customScale != null) {
        modelPartToRest.xScale = customScale.x();
        modelPartToRest.yScale = customScale.y();
        modelPartToRest.zScale = customScale.z();
      }
      if (isVisible != null) {
        modelPartToRest.visible = isVisible;
      }
    }
  }
}
