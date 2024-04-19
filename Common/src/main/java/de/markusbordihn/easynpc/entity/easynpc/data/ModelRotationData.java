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
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelRotationData<T extends PathfinderMob> extends EasyNPC<T> {

  EntityDataSerializer<CustomRotation> ROTATION =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, CustomRotation rotation) {
          buffer.writeFloat(rotation.x());
          buffer.writeFloat(rotation.y());
          buffer.writeFloat(rotation.z());
        }

        public CustomRotation read(FriendlyByteBuf buffer) {
          return new CustomRotation(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        public CustomRotation copy(CustomRotation rotation) {
          return rotation;
        }
      };
  String EASY_NPC_DATA_MODEL_ROTATION_TAG = "Rotation";
  String EASY_NPC_DATA_MODEL_LOCK_TAG = "Lock";
  CustomRotation DEFAULT_MODEL_PART_ROTATION = new CustomRotation(0, 0, 0);

  static void registerSyncedModelRotationData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Model Rotation Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.MODEL_LOCK_ROTATION,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_HEAD_ROTATION, SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_BODY_ROTATION, SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_ARMS_ROTATION, SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_LEFT_ARM_ROTATION,
        SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_RIGHT_ARM_ROTATION,
        SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_LEFT_LEG_ROTATION,
        SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_RIGHT_LEG_ROTATION,
        SynchedEntityData.defineId(entityClass, ROTATION));
    map.put(
        SynchedDataIndex.MODEL_ROOT_ROTATION, SynchedEntityData.defineId(entityClass, ROTATION));
  }

  static void registerModelRotationDataSerializer() {
    EntityDataSerializers.registerSerializer(ROTATION);
  }

  boolean hasHeadModelPart();

  boolean hasBodyModelPart();

  boolean hasArmsModelPart();

  boolean hasLeftArmModelPart();

  boolean hasRightArmModelPart();

  boolean hasLeftLegModelPart();

  boolean hasRightLegModelPart();

  default CustomRotation getModelPartRotation(ModelPart modelPart) {
    return switch (modelPart) {
      case HEAD -> getModelHeadRotation();
      case BODY -> getModelBodyRotation();
      case ARMS -> getModelArmsRotation();
      case LEFT_ARM -> getModelLeftArmRotation();
      case RIGHT_ARM -> getModelRightArmRotation();
      case LEFT_LEG -> getModelLeftLegRotation();
      case RIGHT_LEG -> getModelRightLegRotation();
      default -> DEFAULT_MODEL_PART_ROTATION;
    };
  }

  default boolean hasChangedModelRotation() {
    return (hasHeadModelPart() && getModelHeadRotation().hasChanged())
        || (hasBodyModelPart() && getModelBodyRotation().hasChanged())
        || (hasArmsModelPart() && getModelArmsRotation().hasChanged())
        || (hasLeftArmModelPart() && getModelLeftArmRotation().hasChanged())
        || (hasRightArmModelPart() && getModelRightArmRotation().hasChanged())
        || (hasLeftLegModelPart() && getModelLeftLegRotation().hasChanged())
        || (hasRightLegModelPart() && getModelRightLegRotation().hasChanged());
  }

  default boolean getModelLockRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LOCK_ROTATION);
  }

  default void setModelLockRotation(boolean modelLockRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LOCK_ROTATION, modelLockRotation);
  }

  default CustomRotation getModelHeadRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_HEAD_ROTATION);
  }

  default void setModelHeadRotation(CustomRotation modelHeadRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_HEAD_ROTATION, modelHeadRotation);
  }

  default CustomRotation getModelBodyRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_BODY_ROTATION);
  }

  default void setModelBodyRotation(CustomRotation modelBodyRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_BODY_ROTATION, modelBodyRotation);
  }

  default CustomRotation getModelArmsRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_ARMS_ROTATION);
  }

  default void setModelArmsRotation(CustomRotation modelArmsRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_ARMS_ROTATION, modelArmsRotation);
  }

  default CustomRotation getModelLeftArmRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_ROTATION);
  }

  default void setModelLeftArmRotation(CustomRotation modelLeftArmRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_ROTATION, modelLeftArmRotation);
  }

  default CustomRotation getModelRightArmRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_ROTATION);
  }

  default void setModelRightArmRotation(CustomRotation modelRightArmRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_ROTATION, modelRightArmRotation);
  }

  default CustomRotation getModelLeftLegRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_ROTATION);
  }

  default void setModelLeftLegRotation(CustomRotation modelLeftLegRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_ROTATION, modelLeftLegRotation);
  }

  default CustomRotation getModelRightLegRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_ROTATION);
  }

  default void setModelRightLegRotation(CustomRotation modelRightLegRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_ROTATION, modelRightLegRotation);
  }

  default CustomRotation getModelRootRotation() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_ROOT_ROTATION);
  }

  default void setModelRootRotation(CustomRotation modelRootRotation) {
    setSynchedEntityData(SynchedDataIndex.MODEL_ROOT_ROTATION, modelRootRotation);
  }

  default void defineSynchedModelRotationData() {
    defineSynchedEntityData(SynchedDataIndex.MODEL_LOCK_ROTATION, false);
    defineSynchedEntityData(SynchedDataIndex.MODEL_HEAD_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_BODY_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_ARMS_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_ROTATION, new CustomRotation(0, 0, 0));
    defineSynchedEntityData(SynchedDataIndex.MODEL_ROOT_ROTATION, new CustomRotation(0, 0, 0));
  }

  default void addAdditionalModelRotationData(CompoundTag compoundTag) {
    CompoundTag rotationsTag = new CompoundTag();
    if (this.getModelLockRotation()) {
      rotationsTag.putBoolean(EASY_NPC_DATA_MODEL_LOCK_TAG, this.getModelLockRotation());
    }
    if (this.hasHeadModelPart()
        && this.getModelHeadRotation() != null
        && this.getModelHeadRotation().hasChanged()) {
      rotationsTag.put(ModelPart.HEAD.getTagName(), this.getModelHeadRotation().save());
    }
    if (this.hasBodyModelPart()
        && this.getModelBodyRotation() != null
        && this.getModelBodyRotation().hasChanged()) {
      rotationsTag.put(ModelPart.BODY.getTagName(), this.getModelBodyRotation().save());
    }
    if (this.hasArmsModelPart()
        && this.getModelArmsRotation() != null
        && this.getModelArmsRotation().hasChanged()) {
      rotationsTag.put(ModelPart.ARMS.getTagName(), this.getModelArmsRotation().save());
    }
    if (this.hasLeftArmModelPart()
        && this.getModelLeftArmRotation() != null
        && this.getModelLeftArmRotation().hasChanged()) {
      rotationsTag.put(ModelPart.LEFT_ARM.getTagName(), this.getModelLeftArmRotation().save());
    }
    if (this.hasRightArmModelPart()
        && this.getModelRightArmRotation() != null
        && this.getModelRightArmRotation().hasChanged()) {
      rotationsTag.put(ModelPart.RIGHT_ARM.getTagName(), this.getModelRightArmRotation().save());
    }
    if (this.hasLeftLegModelPart()
        && this.getModelLeftLegRotation() != null
        && this.getModelLeftLegRotation().hasChanged()) {
      rotationsTag.put(ModelPart.LEFT_LEG.getTagName(), this.getModelLeftLegRotation().save());
    }
    if (this.hasRightLegModelPart()
        && this.getModelRightLegRotation() != null
        && this.getModelRightLegRotation().hasChanged()) {
      rotationsTag.put(ModelPart.RIGHT_LEG.getTagName(), this.getModelRightLegRotation().save());
    }
    if (this.getModelRootRotation() != null && this.getModelRootRotation().hasChanged()) {
      rotationsTag.put(ModelPart.ROOT.getTagName(), this.getModelRootRotation().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_ROTATION_TAG, rotationsTag);
  }

  default void readAdditionalModelRotationData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_ROTATION_TAG)) {
      return;
    }
    CompoundTag rotationsTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_ROTATION_TAG);
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_LOCK_TAG)) {
      setModelLockRotation(rotationsTag.getBoolean(EASY_NPC_DATA_MODEL_LOCK_TAG));
    }
    if (rotationsTag.contains(ModelPart.HEAD.getTagName())) {
      setModelHeadRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.HEAD.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.BODY.getTagName())) {
      setModelBodyRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.BODY.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.ARMS.getTagName())) {
      setModelArmsRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.ARMS.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.LEFT_ARM.getTagName())) {
      setModelLeftArmRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.LEFT_ARM.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.RIGHT_ARM.getTagName())) {
      setModelRightArmRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.RIGHT_ARM.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.LEFT_LEG.getTagName())) {
      setModelLeftLegRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.LEFT_LEG.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.RIGHT_LEG.getTagName())) {
      setModelRightLegRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.RIGHT_LEG.getTagName(), 5)));
    }
    if (rotationsTag.contains(ModelPart.ROOT.getTagName())) {
      setModelRootRotation(
          new CustomRotation(rotationsTag.getList(ModelPart.ROOT.getTagName(), 5)));
    }
  }
}
