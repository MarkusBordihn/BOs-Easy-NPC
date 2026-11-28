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
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.client.renderer.rendertype.RenderType;
import net.minecraft.client.renderer.rendertype.RenderTypes;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCModelManager {

  public static final String MODEL_PART_ARMS = "arms";
  public static final String MODEL_PART_BODY = "body";
  public static final String MODEL_PART_CUBE = "cube";
  public static final String MODEL_PART_TAIL = "tail";

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final ModelPart rootModelPart;
  private final Function<Identifier, RenderType> renderType;
  private final Map<ModelPartType, CustomPosition> defaultModelPartPositionMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, CustomRotation> defaultModelPartRotationMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, CustomScale> defaultModelPartScaleMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, Boolean> defaultModelPartVisibilityMap =
      new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, ModelPart> modelPartMap = new EnumMap<>(ModelPartType.class);
  private final Map<ModelPartType, String> missingModelPartMap = new EnumMap<>(ModelPartType.class);
  private boolean modelPartsValidated;

  public EasyNPCModelManager(final ModelPart rootModelPart) {
    this(rootModelPart, RenderTypes::entityCutoutNoCull);
  }

  public EasyNPCModelManager(
      final ModelPart rootModelPart, final Function<Identifier, RenderType> renderType) {
    this.rootModelPart = rootModelPart;
    this.renderType = renderType;
  }

  public EasyNPCModelManager defineModelPart(
      final ModelPartType modelPartType, final ModelPart parentPart, final String childName) {
    if (parentPart != null && parentPart.hasChild(childName)) {
      return this.defineModelPart(modelPartType, parentPart.getChild(childName));
    }

    this.missingModelPartMap.put(modelPartType, childName);
    return this;
  }

  public EasyNPCModelManager defineModelPart(
      final ModelPartType modelPartType, final String modelPartName) {
    if (this.rootModelPart != null && this.rootModelPart.hasChild(modelPartName)) {
      return this.defineModelPart(modelPartType, this.rootModelPart.getChild(modelPartName));
    }

    this.missingModelPartMap.put(modelPartType, modelPartName);
    return this;
  }

  public EasyNPCModelManager defineModelPart(
      final ModelPartType modelPartType, final ModelPart modelPart) {
    if (modelPart == null) {
      this.missingModelPartMap.put(modelPartType, "<null>");
      return this;
    }

    if (!this.defaultModelPartPositionMap.containsKey(modelPartType)) {
      this.setDefaultModelPartPosition(
          modelPartType, new CustomPosition(modelPart.x, modelPart.y, modelPart.z));
    }
    if (!this.defaultModelPartRotationMap.containsKey(modelPartType)) {
      this.setDefaultModelPartRotation(
          modelPartType, new CustomRotation(modelPart.xRot, modelPart.yRot, modelPart.zRot));
    }
    if (!this.defaultModelPartScaleMap.containsKey(modelPartType)) {
      this.setDefaultModelPartScale(
          modelPartType, new CustomScale(modelPart.xScale, modelPart.yScale, modelPart.zScale));
    }
    if (!this.defaultModelPartVisibilityMap.containsKey(modelPartType)) {
      this.setDefaultModelPartVisibility(modelPartType, modelPart.visible);
    }
    if (!this.modelPartMap.containsKey(modelPartType)) {
      this.setDefaultModelPart(modelPartType, modelPart);
    }
    this.missingModelPartMap.remove(modelPartType);
    return this;
  }

  public void setDefaultModelPartPosition(
      final ModelPartType modelPartType, final CustomPosition customPosition) {
    this.defaultModelPartPositionMap.put(modelPartType, customPosition);
  }

  public void setDefaultModelPartRotation(
      final ModelPartType modelPartType, final CustomRotation rotation) {
    this.defaultModelPartRotationMap.put(modelPartType, rotation);
  }

  public void setDefaultModelPartScale(
      final ModelPartType modelPartType, final CustomScale customScale) {
    this.defaultModelPartScaleMap.put(modelPartType, customScale);
  }

  public void setDefaultModelPartVisibility(
      final ModelPartType modelPartType, final boolean isVisible) {
    this.defaultModelPartVisibilityMap.put(modelPartType, isVisible);
  }

  public void setDefaultModelPart(final ModelPartType modelPartType, final ModelPart modelPart) {
    this.modelPartMap.put(modelPartType, modelPart);
  }

  public ModelPart getModelPart(final ModelPartType modelPartType) {
    return this.modelPartMap.get(modelPartType);
  }

  public void validateModelPartsOnce(final EasyNPC<?> easyNPC) {
    if (this.modelPartsValidated || easyNPC == null || easyNPC instanceof EasyModelNPC) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return;
    }

    this.modelPartsValidated = true;

    ModelType modelType = modelData.getModelType();
    EnumSet<ModelPartType> missingModelParts = EnumSet.noneOf(ModelPartType.class);
    for (ModelPartType modelPartType : modelType.getModelParts()) {
      if (!this.modelPartMap.containsKey(modelPartType)) {
        missingModelParts.add(modelPartType);
      }
    }

    if (missingModelParts.isEmpty()) {
      return;
    }

    log.warn(
        "Missing Easy NPC model parts {} for entity type {} with model type {}, root model part {}, originally missing {}.",
        missingModelParts,
        easyNPC.getLivingEntity().getType(),
        modelType,
        this.rootModelPart,
        this.missingModelPartMap);
  }

  public boolean setupModelParts(
      final ModelDataCapable<?> modelData, final boolean applyVisibility) {
    if (modelData == null || modelData.getModelPose() == ModelPose.VANILLA) {
      return false;
    }

    EnumMap<ModelPartType, Boolean> visibilityMap = modelData.getModelPartVisibility();
    EnumMap<ModelPartType, CustomPosition> positionMap = modelData.getModelPartPosition();
    EnumMap<ModelPartType, CustomRotation> rotationMap = modelData.getModelPartRotation();
    EnumMap<ModelPartType, CustomScale> scaleMap = modelData.getModelPartScale();
    boolean hasChangedModelPart = false;

    for (Map.Entry<ModelPartType, ModelPart> entry : this.modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      // Skip outer layer parts as they will be synced from their inner parts
      if (this.isOuterLayerPart(partType)) {
        continue;
      }

      // Skip transforms for parts that are explicitly hidden.
      if (applyVisibility && this.applyModelPartVisibility(modelPart, partType, visibilityMap)) {
        continue;
      }

      boolean positionChanged = this.applyCustomPosition(modelPart, positionMap.get(partType));
      boolean rotationChanged = this.applyCustomRotation(modelPart, rotationMap.get(partType));
      boolean scaleChanged = this.applyCustomScale(modelPart, partType, scaleMap.get(partType));
      if (positionChanged || rotationChanged || scaleChanged) {
        hasChangedModelPart = true;
      }
    }

    this.syncOuterLayers(visibilityMap);

    return hasChangedModelPart;
  }

  public void syncModelParts(final ModelDataCapable<?> modelData) {
    if (modelData == null || modelData.getModelPose() == ModelPose.VANILLA) {
      return;
    }

    this.syncOuterLayers(modelData.getModelPartVisibility());
  }

  private void syncOuterLayers(final EnumMap<ModelPartType, Boolean> visibilityMap) {
    this.syncOuterLayer(visibilityMap, ModelPartType.HAT, ModelPartType.HEAD);
    this.syncOuterLayer(visibilityMap, ModelPartType.BODY_JACKET, ModelPartType.BODY);
    this.syncOuterLayer(visibilityMap, ModelPartType.LEFT_SLEEVE, ModelPartType.LEFT_ARM);
    this.syncOuterLayer(visibilityMap, ModelPartType.RIGHT_SLEEVE, ModelPartType.RIGHT_ARM);
    this.syncOuterLayer(visibilityMap, ModelPartType.LEFT_PANTS, ModelPartType.LEFT_LEG);
    this.syncOuterLayer(visibilityMap, ModelPartType.RIGHT_PANTS, ModelPartType.RIGHT_LEG);
  }

  private void syncOuterLayer(
      final EnumMap<ModelPartType, Boolean> visibilityMap,
      final ModelPartType outerPartType,
      final ModelPartType innerPartType) {

    ModelPart outerPart = this.modelPartMap.get(outerPartType);
    ModelPart innerPart = this.modelPartMap.get(innerPartType);

    if (outerPart == null
        || innerPart == null
        || !Boolean.TRUE.equals(this.defaultModelPartVisibilityMap.get(outerPartType))) {
      return;
    }

    if (Boolean.FALSE.equals(visibilityMap.get(outerPartType))) {
      outerPart.visible = false;
    } else {
      outerPart.visible = innerPart.visible;
    }
  }

  public boolean shouldCancelAnimation(final ModelDataCapable<?> modelData) {
    if (modelData == null) {
      return false;
    }

    switch (modelData.getModelAnimationBehavior()) {
      case NONE:
        return true;
      case DEFAULT:
        return modelData.getModelPose() == ModelPose.CUSTOM
            || modelData.getModelPose() == ModelPose.DEFAULT;
      case SMART:
      default:
        break;
    }

    EnumMap<ModelPartType, CustomPosition> positionMap = modelData.getModelPartPosition();
    EnumMap<ModelPartType, CustomRotation> rotationMap = modelData.getModelPartRotation();
    EnumMap<ModelPartType, CustomScale> scaleMap = modelData.getModelPartScale();
    int modifiedCriticalParts = 0;
    int totalCriticalParts = 0;
    for (ModelPartType partType : this.modelPartMap.keySet()) {
      if (!this.isCriticalAnimationPart(partType)) {
        continue;
      }

      totalCriticalParts++;

      CustomRotation rotation = rotationMap.get(partType);
      CustomPosition position = positionMap.get(partType);
      CustomScale scale = scaleMap.get(partType);
      boolean rotationChanged = rotation != null && rotation.hasChanged();
      boolean positionChanged =
          partType != ModelPartType.HEAD && position != null && position.hasChanged();
      boolean scaleChanged = scale != null && scale.hasChanged();
      if (rotationChanged || positionChanged || scaleChanged) {
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

  private boolean applyModelPartVisibility(
      final ModelPart modelPart,
      final ModelPartType partType,
      final EnumMap<ModelPartType, Boolean> visibilityMap) {
    Boolean visibility = visibilityMap.get(partType);
    if (Boolean.FALSE.equals(visibility)) {
      modelPart.visible = false;
      return true;
    }

    if (Boolean.TRUE.equals(visibility)
        && Boolean.TRUE.equals(this.defaultModelPartVisibilityMap.get(partType))) {
      modelPart.visible = true;
    }
    return false;
  }

  private boolean applyCustomPosition(
      final ModelPart modelPart, final CustomPosition customPosition) {
    if (customPosition == null || !customPosition.hasChanged()) {
      return false;
    }

    modelPart.x += customPosition.x();
    modelPart.y += customPosition.y();
    modelPart.z += customPosition.z();
    return true;
  }

  private boolean applyCustomRotation(
      final ModelPart modelPart, final CustomRotation customRotation) {
    if (customRotation == null || !customRotation.hasChanged()) {
      return false;
    }

    modelPart.xRot += customRotation.x();
    modelPart.yRot += customRotation.y();
    modelPart.zRot += customRotation.z();
    return true;
  }

  private boolean applyCustomScale(
      final ModelPart modelPart, final ModelPartType partType, final CustomScale customScale) {
    if (customScale == null || !customScale.hasChanged()) {
      return false;
    }

    CustomScale defaultScale = this.defaultModelPartScaleMap.get(partType);
    if (defaultScale != null) {
      modelPart.xScale = defaultScale.x() * customScale.x();
      modelPart.yScale = defaultScale.y() * customScale.y();
      modelPart.zScale = defaultScale.z() * customScale.z();
    } else {
      modelPart.xScale = customScale.x();
      modelPart.yScale = customScale.y();
      modelPart.zScale = customScale.z();
    }
    return true;
  }

  public void applySelectiveChanges(final ModelDataCapable<?> modelData) {
    if (modelData == null) {
      return;
    }

    EnumMap<ModelPartType, CustomPosition> positionMap = modelData.getModelPartPosition();
    EnumMap<ModelPartType, CustomRotation> rotationMap = modelData.getModelPartRotation();
    EnumMap<ModelPartType, CustomScale> scaleMap = modelData.getModelPartScale();
    for (Map.Entry<ModelPartType, ModelPart> entry : this.modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      if (partType == ModelPartType.HAT) {
        continue;
      }

      this.applyCustomScale(modelPart, partType, scaleMap.get(partType));
      this.applyCustomRotation(modelPart, rotationMap.get(partType));
      this.applyCustomPosition(modelPart, positionMap.get(partType));
    }

    this.syncModelParts(modelData);
  }

  public void applyVisibilityChanges(final ModelDataCapable<?> modelData) {
    if (modelData == null) {
      return;
    }

    EnumMap<ModelPartType, Boolean> visibilityMap = modelData.getModelPartVisibility();

    for (Map.Entry<ModelPartType, ModelPart> entry : this.modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();

      // Skip outer layer parts as they will be synced from their inner parts
      if (this.isOuterLayerPart(partType)) {
        continue;
      }

      this.applyModelPartVisibility(modelPart, partType, visibilityMap);
    }

    this.syncModelParts(modelData);
  }

  public void resetModelParts() {
    for (Map.Entry<ModelPartType, ModelPart> entry : this.modelPartMap.entrySet()) {
      ModelPartType modelPartType = entry.getKey();
      ModelPart modelPartToReset = entry.getValue();

      Boolean isVisible = this.defaultModelPartVisibilityMap.get(modelPartType);
      if (isVisible != null) {
        modelPartToReset.visible = isVisible;
      }

      // Skip outer layer parts as they will be synced from their inner parts
      if (this.isOuterLayerPart(modelPartType)) {
        continue;
      }

      this.resetModelPartTransform(modelPartType, modelPartToReset);
    }
  }

  public void reapplyChangedModelParts(final ModelDataCapable<?> modelData) {
    if (modelData == null || modelData.getModelPose() == ModelPose.VANILLA) {
      return;
    }

    EnumMap<ModelPartType, Boolean> visibilityMap = modelData.getModelPartVisibility();
    EnumMap<ModelPartType, CustomPosition> positionMap = modelData.getModelPartPosition();
    EnumMap<ModelPartType, CustomRotation> rotationMap = modelData.getModelPartRotation();
    EnumMap<ModelPartType, CustomScale> scaleMap = modelData.getModelPartScale();
    for (Map.Entry<ModelPartType, ModelPart> entry : this.modelPartMap.entrySet()) {
      ModelPartType partType = entry.getKey();
      ModelPart modelPart = entry.getValue();
      if (this.isOuterLayerPart(partType)
          || this.applyModelPartVisibility(modelPart, partType, visibilityMap)) {
        continue;
      }

      CustomPosition customPosition = positionMap.get(partType);
      CustomRotation customRotation = rotationMap.get(partType);
      CustomScale customScale = scaleMap.get(partType);
      if (!this.hasCustomTransform(customPosition, customRotation, customScale)) {
        continue;
      }

      this.resetModelPartTransform(partType, modelPart);
      this.applyCustomPosition(modelPart, customPosition);
      this.applyCustomRotation(modelPart, customRotation);
      this.applyCustomScale(modelPart, partType, customScale);
    }

    this.syncOuterLayers(visibilityMap);
  }

  private boolean hasCustomTransform(
      final CustomPosition customPosition,
      final CustomRotation customRotation,
      final CustomScale customScale) {
    return (customPosition != null && customPosition.hasChanged())
        || (customRotation != null && customRotation.hasChanged())
        || (customScale != null && customScale.hasChanged());
  }

  private void resetModelPartTransform(final ModelPartType partType, final ModelPart modelPart) {
    CustomPosition defaultPosition = this.defaultModelPartPositionMap.get(partType);
    CustomRotation defaultRotation = this.defaultModelPartRotationMap.get(partType);
    CustomScale defaultScale = this.defaultModelPartScaleMap.get(partType);

    if (defaultPosition != null) {
      modelPart.setPos(defaultPosition.x(), defaultPosition.y(), defaultPosition.z());
    }
    if (defaultRotation != null) {
      modelPart.setRotation(defaultRotation.x(), defaultRotation.y(), defaultRotation.z());
    }
    if (defaultScale != null) {
      modelPart.xScale = defaultScale.x();
      modelPart.yScale = defaultScale.y();
      modelPart.zScale = defaultScale.z();
    }
  }
}
