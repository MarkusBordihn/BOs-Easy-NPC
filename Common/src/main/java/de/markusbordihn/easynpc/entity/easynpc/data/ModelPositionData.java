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
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelPositionData<T extends PathfinderMob> extends EasyNPC<T> {

  CustomPosition DEFAULT_MODEL_PART_POSITION = new CustomPosition(0, 0, 0);
  String EASY_NPC_DATA_MODEL_POSITION_TAG = "Position";

  static void registerSyncedModelPositionData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Model Position Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.MODEL_HEAD_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
    map.put(
        SynchedDataIndex.MODEL_BODY_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
    map.put(
        SynchedDataIndex.MODEL_ARMS_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
    map.put(
        SynchedDataIndex.MODEL_LEFT_ARM_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
    map.put(
        SynchedDataIndex.MODEL_RIGHT_ARM_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
    map.put(
        SynchedDataIndex.MODEL_LEFT_LEG_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
    map.put(
        SynchedDataIndex.MODEL_RIGHT_LEG_POSITION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.POSITION));
  }

  boolean hasHeadModelPart();

  boolean hasBodyModelPart();

  boolean hasArmsModelPart();

  boolean hasLeftArmModelPart();

  boolean hasRightArmModelPart();

  boolean hasLeftLegModelPart();

  boolean hasRightLegModelPart();

  default CustomPosition getModelPartPosition(ModelPart modelPart) {
    return switch (modelPart) {
      case HEAD -> getModelHeadPosition();
      case BODY -> getModelBodyPosition();
      case ARMS -> getModelArmsPosition();
      case LEFT_ARM -> getModelLeftArmPosition();
      case RIGHT_ARM -> getModelRightArmPosition();
      case LEFT_LEG -> getModelLeftLegPosition();
      case RIGHT_LEG -> getModelRightLegPosition();
      default -> DEFAULT_MODEL_PART_POSITION;
    };
  }

  default void setModelPartPosition(ModelPart modelPart, CustomPosition position) {
    if (modelPart == ModelPart.ROOT) {
      return;
    }
    switch (modelPart) {
      case HEAD -> setModelHeadPosition(position);
      case BODY -> setModelBodyPosition(position);
      case ARMS -> setModelArmsPosition(position);
      case LEFT_ARM -> setModelLeftArmPosition(position);
      case RIGHT_ARM -> setModelRightArmPosition(position);
      case LEFT_LEG -> setModelLeftLegPosition(position);
      case RIGHT_LEG -> setModelRightLegPosition(position);
      default -> log.error("Invalid position model part {} for {}", modelPart, this);
    }
  }

  default CustomPosition getModelHeadPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_HEAD_POSITION);
  }

  default void setModelHeadPosition(CustomPosition modelHeadPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_HEAD_POSITION, modelHeadPosition);
  }

  default CustomPosition getModelBodyPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_BODY_POSITION);
  }

  default void setModelBodyPosition(CustomPosition modelBodyPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_BODY_POSITION, modelBodyPosition);
  }

  default CustomPosition getModelArmsPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_ARMS_POSITION);
  }

  default void setModelArmsPosition(CustomPosition modelArmsPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_ARMS_POSITION, modelArmsPosition);
  }

  default CustomPosition getModelLeftArmPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_POSITION);
  }

  default void setModelLeftArmPosition(CustomPosition modelLeftArmPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_POSITION, modelLeftArmPosition);
  }

  default CustomPosition getModelRightArmPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_POSITION);
  }

  default void setModelRightArmPosition(CustomPosition modelRightArmPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_POSITION, modelRightArmPosition);
  }

  default CustomPosition getModelLeftLegPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_POSITION);
  }

  default void setModelLeftLegPosition(CustomPosition modelLeftLegPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_POSITION, modelLeftLegPosition);
  }

  default CustomPosition getModelRightLegPosition() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_POSITION);
  }

  default void setModelRightLegPosition(CustomPosition modelRightLegPosition) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_POSITION, modelRightLegPosition);
  }

  default boolean hasChangedModelPosition() {
    return (hasHeadModelPart() && getModelHeadPosition().hasChanged())
        || (hasBodyModelPart() && getModelBodyPosition().hasChanged())
        || (hasArmsModelPart() && getModelArmsPosition().hasChanged())
        || (hasLeftArmModelPart() && getModelLeftArmPosition().hasChanged())
        || (hasRightArmModelPart() && getModelRightArmPosition().hasChanged())
        || (hasLeftLegModelPart() && getModelLeftLegPosition().hasChanged())
        || (hasRightLegModelPart() && getModelRightLegPosition().hasChanged());
  }

  default void defineSynchedModelPositionData(SynchedEntityData.Builder builder) {
    // Position
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_HEAD_POSITION, new CustomPosition(0, 0, 0));
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_BODY_POSITION, new CustomPosition(0, 0, 0));
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_ARMS_POSITION, new CustomPosition(0, 0, 0));
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_LEFT_ARM_POSITION, new CustomPosition(0, 0, 0));
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_RIGHT_ARM_POSITION, new CustomPosition(0, 0, 0));
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_LEFT_LEG_POSITION, new CustomPosition(0, 0, 0));
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_RIGHT_LEG_POSITION, new CustomPosition(0, 0, 0));
  }

  default void addAdditionalModelPositionData(CompoundTag compoundTag) {
    CompoundTag positionsTag = new CompoundTag();
    if (hasHeadModelPart()
        && this.getModelHeadPosition() != null
        && this.getModelHeadPosition().hasChanged()) {
      positionsTag.put(ModelPart.HEAD.getTagName(), this.getModelHeadPosition().save());
    }
    if (hasBodyModelPart()
        && this.getModelBodyPosition() != null
        && this.getModelBodyPosition().hasChanged()) {
      positionsTag.put(ModelPart.BODY.getTagName(), this.getModelBodyPosition().save());
    }
    if (hasArmsModelPart()
        && this.getModelArmsPosition() != null
        && this.getModelArmsPosition().hasChanged()) {
      positionsTag.put(ModelPart.ARMS.getTagName(), this.getModelArmsPosition().save());
    }
    if (hasLeftArmModelPart()
        && this.getModelLeftArmPosition() != null
        && this.getModelLeftArmPosition().hasChanged()) {
      positionsTag.put(ModelPart.LEFT_ARM.getTagName(), this.getModelLeftArmPosition().save());
    }
    if (hasRightArmModelPart()
        && this.getModelRightArmPosition() != null
        && this.getModelRightArmPosition().hasChanged()) {
      positionsTag.put(ModelPart.RIGHT_ARM.getTagName(), this.getModelRightArmPosition().save());
    }
    if (hasLeftLegModelPart()
        && this.getModelLeftLegPosition() != null
        && this.getModelLeftLegPosition().hasChanged()) {
      positionsTag.put(ModelPart.LEFT_LEG.getTagName(), this.getModelLeftLegPosition().save());
    }
    if (hasRightLegModelPart()
        && this.getModelRightLegPosition() != null
        && this.getModelRightLegPosition().hasChanged()) {
      positionsTag.put(ModelPart.RIGHT_LEG.getTagName(), this.getModelRightLegPosition().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_POSITION_TAG, positionsTag);
  }

  default void readAdditionalModelPositionData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_POSITION_TAG)) {
      return;
    }
    CompoundTag positionTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_POSITION_TAG);
    if (positionTag.contains(ModelPart.HEAD.getTagName())) {
      setModelHeadPosition(new CustomPosition(positionTag.getList(ModelPart.HEAD.getTagName(), 5)));
    }
    if (positionTag.contains(ModelPart.BODY.getTagName())) {
      setModelBodyPosition(new CustomPosition(positionTag.getList(ModelPart.BODY.getTagName(), 5)));
    }
    if (positionTag.contains(ModelPart.ARMS.getTagName())) {
      setModelArmsPosition(new CustomPosition(positionTag.getList(ModelPart.ARMS.getTagName(), 5)));
    }
    if (positionTag.contains(ModelPart.LEFT_ARM.getTagName())) {
      setModelLeftArmPosition(
          new CustomPosition(positionTag.getList(ModelPart.LEFT_ARM.getTagName(), 5)));
    }
    if (positionTag.contains(ModelPart.RIGHT_ARM.getTagName())) {
      setModelRightArmPosition(
          new CustomPosition(positionTag.getList(ModelPart.RIGHT_ARM.getTagName(), 5)));
    }
    if (positionTag.contains(ModelPart.LEFT_LEG.getTagName())) {
      setModelLeftLegPosition(
          new CustomPosition(positionTag.getList(ModelPart.LEFT_LEG.getTagName(), 5)));
    }
    if (positionTag.contains(ModelPart.RIGHT_LEG.getTagName())) {
      setModelRightLegPosition(
          new CustomPosition(positionTag.getList(ModelPart.RIGHT_LEG.getTagName(), 5)));
    }
  }
}
