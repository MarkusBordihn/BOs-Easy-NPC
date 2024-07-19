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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelVisibilityData<T extends PathfinderMob> extends EasyNPC<T> {

  String EASY_NPC_DATA_MODEL_VISIBLE_TAG = "Visible";

  static void registerSyncedModelVisibilityData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Model Visibility Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.MODEL_HEAD_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_BODY_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_ARMS_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_LEFT_ARM_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_RIGHT_ARM_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_LEFT_LEG_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_RIGHT_LEG_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_HELMET_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_CHESTPLATE_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_LEGGINGS_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_BOOTS_VISIBLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
  }

  boolean hasHeadModelPart();

  boolean hasBodyModelPart();

  boolean hasArmsModelPart();

  boolean hasLeftArmModelPart();

  boolean hasRightArmModelPart();

  boolean hasLeftLegModelPart();

  boolean hasRightLegModelPart();

  boolean canUseArmor();

  default boolean isModelPartVisible(ModelPart modelPart) {
    return switch (modelPart) {
      case HEAD -> isModelHeadVisible();
      case BODY -> isModelBodyVisible();
      case ARMS -> isModelArmsVisible();
      case LEFT_ARM -> isModelLeftArmVisible();
      case RIGHT_ARM -> isModelRightArmVisible();
      case LEFT_LEG -> isModelLeftLegVisible();
      case RIGHT_LEG -> isModelRightLegVisible();
      default -> false;
    };
  }

  default boolean isModelEquipmentVisible(EquipmentSlot equipmentSlot) {
    if (equipmentSlot == null || !this.canUseArmor()) {
      return false;
    }
    return switch (equipmentSlot) {
      case HEAD -> isModelHelmetVisible();
      case CHEST -> isModelChestplateVisible();
      case LEGS -> isModelLeggingsVisible();
      case FEET -> isModelBootsVisible();
      default -> false;
    };
  }

  default void setModelPartVisible(ModelPart modelPart, boolean visible) {
    if (modelPart == ModelPart.ROOT) {
      return;
    }
    switch (modelPart) {
      case HEAD -> setModelHeadVisible(visible);
      case BODY -> setModelBodyVisible(visible);
      case ARMS -> setModelArmsVisible(visible);
      case LEFT_ARM -> setModelLeftArmVisible(visible);
      case RIGHT_ARM -> setModelRightArmVisible(visible);
      case LEFT_LEG -> setModelLeftLegVisible(visible);
      case RIGHT_LEG -> setModelRightLegVisible(visible);
      default -> log.error("Invalid visible model part {} for {}", modelPart, this);
    }
  }

  default boolean hasChangedModelVisibility() {
    return (hasHeadModelPart() && !isModelHeadVisible())
        || (hasBodyModelPart() && !isModelBodyVisible())
        || (hasArmsModelPart() && !isModelArmsVisible())
        || (hasLeftArmModelPart() && !isModelLeftArmVisible())
        || (hasRightArmModelPart() && !isModelRightArmVisible())
        || (hasLeftLegModelPart() && !isModelLeftLegVisible())
        || (hasRightLegModelPart() && !isModelRightLegVisible());
  }

  default boolean isModelHeadVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_HEAD_VISIBLE);
  }

  default void setModelHeadVisible(boolean modelHeadVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_HEAD_VISIBLE, modelHeadVisible);
  }

  default boolean isModelBodyVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_BODY_VISIBLE);
  }

  default void setModelBodyVisible(boolean modelBodyVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_BODY_VISIBLE, modelBodyVisible);
  }

  default boolean isModelArmsVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_ARMS_VISIBLE);
  }

  default void setModelArmsVisible(boolean modelArmsVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_ARMS_VISIBLE, modelArmsVisible);
  }

  default boolean isModelLeftArmVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_VISIBLE);
  }

  default void setModelLeftArmVisible(boolean modelLeftArmVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_VISIBLE, modelLeftArmVisible);
  }

  default boolean isModelRightArmVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_VISIBLE);
  }

  default void setModelRightArmVisible(boolean modelRightArmVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_VISIBLE, modelRightArmVisible);
  }

  default boolean isModelLeftLegVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_VISIBLE);
  }

  default void setModelLeftLegVisible(boolean modelLeftLegVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_VISIBLE, modelLeftLegVisible);
  }

  default boolean isModelRightLegVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_VISIBLE);
  }

  default void setModelRightLegVisible(boolean modelRightLegVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_VISIBLE, modelRightLegVisible);
  }

  default boolean isModelHelmetVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_HELMET_VISIBLE);
  }

  default void setModelHelmetVisible(boolean modelHelmetVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_HELMET_VISIBLE, modelHelmetVisible);
  }

  default boolean isModelChestplateVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_CHESTPLATE_VISIBLE);
  }

  default void setModelChestplateVisible(boolean modelChestplateVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_CHESTPLATE_VISIBLE, modelChestplateVisible);
  }

  default boolean isModelLeggingsVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEGGINGS_VISIBLE);
  }

  default void setModelLeggingsVisible(boolean modelLeggingsVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEGGINGS_VISIBLE, modelLeggingsVisible);
  }

  default boolean isModelBootsVisible() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_BOOTS_VISIBLE);
  }

  default void setModelBootsVisible(boolean modelBootsVisible) {
    setSynchedEntityData(SynchedDataIndex.MODEL_BOOTS_VISIBLE, modelBootsVisible);
  }

  default void defineSynchedModelVisibilityData() {
    defineSynchedEntityData(SynchedDataIndex.MODEL_HEAD_VISIBLE, this.hasHeadModelPart());
    defineSynchedEntityData(SynchedDataIndex.MODEL_BODY_VISIBLE, this.hasBodyModelPart());
    defineSynchedEntityData(SynchedDataIndex.MODEL_ARMS_VISIBLE, this.hasArmsModelPart());
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_VISIBLE, this.hasLeftArmModelPart());
    defineSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_VISIBLE, this.hasRightArmModelPart());
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_VISIBLE, this.hasLeftLegModelPart());
    defineSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_VISIBLE, this.hasRightLegModelPart());

    defineSynchedEntityData(SynchedDataIndex.MODEL_HELMET_VISIBLE, this.canUseArmor());
    defineSynchedEntityData(SynchedDataIndex.MODEL_CHESTPLATE_VISIBLE, this.canUseArmor());
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEGGINGS_VISIBLE, this.canUseArmor());
    defineSynchedEntityData(SynchedDataIndex.MODEL_BOOTS_VISIBLE, this.canUseArmor());
  }

  default void addAdditionalModelVisibilityData(CompoundTag compoundTag) {
    CompoundTag visibilityTag = new CompoundTag();
    if (this.isModelHeadVisible() != this.hasHeadModelPart()) {
      visibilityTag.putBoolean(ModelPart.HEAD.getTagName(), this.isModelHeadVisible());
    }
    if (this.isModelBodyVisible() != this.hasBodyModelPart()) {
      visibilityTag.putBoolean(ModelPart.BODY.getTagName(), this.isModelBodyVisible());
    }
    if (this.isModelArmsVisible() != this.hasArmsModelPart()) {
      visibilityTag.putBoolean(ModelPart.ARMS.getTagName(), this.isModelArmsVisible());
    }
    if (this.isModelLeftArmVisible() != this.hasLeftArmModelPart()) {
      visibilityTag.putBoolean(ModelPart.LEFT_ARM.getTagName(), this.isModelLeftArmVisible());
    }
    if (this.isModelRightArmVisible() != this.hasRightArmModelPart()) {
      visibilityTag.putBoolean(ModelPart.RIGHT_ARM.getTagName(), this.isModelRightArmVisible());
    }
    if (this.isModelLeftLegVisible() != this.hasLeftLegModelPart()) {
      visibilityTag.putBoolean(ModelPart.LEFT_LEG.getTagName(), this.isModelLeftLegVisible());
    }
    if (this.isModelRightLegVisible() != this.hasRightLegModelPart()) {
      visibilityTag.putBoolean(ModelPart.RIGHT_LEG.getTagName(), this.isModelRightLegVisible());
    }
    if (this.isModelHelmetVisible() != this.canUseArmor()) {
      visibilityTag.putBoolean(ModelPart.HELMET.getTagName(), this.isModelHelmetVisible());
    }
    if (this.isModelChestplateVisible() != this.canUseArmor()) {
      visibilityTag.putBoolean(ModelPart.CHESTPLATE.getTagName(), this.isModelChestplateVisible());
    }
    if (this.isModelLeggingsVisible() != this.canUseArmor()) {
      visibilityTag.putBoolean(ModelPart.LEGGINGS.getTagName(), this.isModelLeggingsVisible());
    }
    if (this.isModelBootsVisible() != this.canUseArmor()) {
      visibilityTag.putBoolean(ModelPart.BOOTS.getTagName(), this.isModelBootsVisible());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_VISIBLE_TAG, visibilityTag);
  }

  default void readAdditionalModelVisibilityData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_VISIBLE_TAG)) {
      return;
    }
    CompoundTag visibilityTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_VISIBLE_TAG);
    if (visibilityTag.contains(ModelPart.HEAD.getTagName())) {
      setModelHeadVisible(visibilityTag.getBoolean(ModelPart.HEAD.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.BODY.getTagName())) {
      setModelBodyVisible(visibilityTag.getBoolean(ModelPart.BODY.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.ARMS.getTagName())) {
      setModelArmsVisible(visibilityTag.getBoolean(ModelPart.ARMS.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.LEFT_ARM.getTagName())) {
      setModelLeftArmVisible(visibilityTag.getBoolean(ModelPart.LEFT_ARM.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.RIGHT_ARM.getTagName())) {
      setModelRightArmVisible(visibilityTag.getBoolean(ModelPart.RIGHT_ARM.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.LEFT_LEG.getTagName())) {
      setModelLeftLegVisible(visibilityTag.getBoolean(ModelPart.LEFT_LEG.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.RIGHT_LEG.getTagName())) {
      setModelRightLegVisible(visibilityTag.getBoolean(ModelPart.RIGHT_LEG.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.HELMET.getTagName())) {
      setModelHelmetVisible(visibilityTag.getBoolean(ModelPart.HELMET.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.CHESTPLATE.getTagName())) {
      setModelChestplateVisible(visibilityTag.getBoolean(ModelPart.CHESTPLATE.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.LEGGINGS.getTagName())) {
      setModelLeggingsVisible(visibilityTag.getBoolean(ModelPart.LEGGINGS.getTagName()));
    }
    if (visibilityTag.contains(ModelPart.BOOTS.getTagName())) {
      setModelBootsVisible(visibilityTag.getBoolean(ModelPart.BOOTS.getTagName()));
    }
  }
}
