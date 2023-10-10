/**
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

package de.markusbordihn.easynpc.entity.data;

import net.minecraft.core.Rotations;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;

import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;

public interface EntityModelData extends EntityDataInterface {

  // Synced entity data
  public static final EntityDataAccessor<ModelPose> DATA_MODEL_POSE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.MODEL_POSE);

  // Synced entity data for Model Part Positions
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_HEAD_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_BODY_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_ARMS_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_LEFT_ARM_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_RIGHT_ARM_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_LEFT_LEG_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  public static final EntityDataAccessor<CustomPosition> DATA_MODEL_RIGHT_LEG_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);

  // Synced entity data for Model Part Rotations
  public static final EntityDataAccessor<Boolean> DATA_MODEL_LOCK_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_HEAD_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_BODY_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_ARMS_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_LEFT_ARM_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_RIGHT_ARM_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_LEFT_LEG_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_RIGHT_LEG_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_ROOT_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);

  // Synced entity data for Model Part Visibility
  public static final EntityDataAccessor<Boolean> DATA_MODEL_HEAD_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_BODY_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_ARMS_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_LEFT_ARM_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_RIGHT_ARM_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_LEFT_LEG_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_RIGHT_LEG_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // CompoundTags
  public static final String DATA_MODEL_DATA_TAG = "ModelData";
  public static final String DATA_MODEL_POSE_TAG = "Pose";

  // CompoundTags for Model Part Positions
  public static final String DATA_MODEL_POSITION_TAG = "Position";
  public static final String DATA_MODEL_HEAD_POSITION_TAG = "HeadPosition";
  public static final String DATA_MODEL_BODY_POSITION_TAG = "BodyPosition";
  public static final String DATA_MODEL_ARMS_POSITION_TAG = "ArmsPosition";
  public static final String DATA_MODEL_LEFT_ARM_POSITION_TAG = "LeftArmPosition";
  public static final String DATA_MODEL_RIGHT_ARM_POSITION_TAG = "RightArmPosition";
  public static final String DATA_MODEL_LEFT_LEG_POSITION_TAG = "LeftLegPosition";
  public static final String DATA_MODEL_RIGHT_LEG_POSITION_TAG = "RightLegPosition";

  // CompoundTags for Model Part Rotations
  public static final String DATA_MODEL_ROTATION_TAG = "Rotation";
  public static final String DATA_MODEL_LOCK_ROTATION_TAG = "LockRotation";
  public static final String DATA_MODEL_HEAD_ROTATION_TAG = "HeadRotation";
  public static final String DATA_MODEL_BODY_ROTATION_TAG = "BodyRotation";
  public static final String DATA_MODEL_ARMS_ROTATION_TAG = "ArmsRotation";
  public static final String DATA_MODEL_LEFT_ARM_ROTATION_TAG = "LeftArmRotation";
  public static final String DATA_MODEL_RIGHT_ARM_ROTATION_TAG = "RightArmRotation";
  public static final String DATA_MODEL_LEFT_LEG_ROTATION_TAG = "LeftLegRotation";
  public static final String DATA_MODEL_RIGHT_LEG_ROTATION_TAG = "RightLegRotation";
  public static final String DATA_MODEL_ROOT_ROTATION_TAG = "RootRotation";

  // CompoundTags for Model Part Visibility
  public static final String DATA_MODEL_VISIBLE_TAG = "Visible";
  public static final String DATA_MODEL_HEAD_VISIBLE_TAG = "HeadVisible";
  public static final String DATA_MODEL_BODY_VISIBLE_TAG = "BodyVisible";
  public static final String DATA_MODEL_ARMS_VISIBLE_TAG = "ArmsVisible";
  public static final String DATA_MODEL_LEFT_ARM_VISIBLE_TAG = "LeftArmVisible";
  public static final String DATA_MODEL_RIGHT_ARM_VISIBLE_TAG = "RightArmVisible";
  public static final String DATA_MODEL_LEFT_LEG_VISIBLE_TAG = "LeftLegVisible";
  public static final String DATA_MODEL_RIGHT_LEG_VISIBLE_TAG = "RightLegVisible";

  // CompoundTags (Legacy)
  public static final String LEGACY_DATA_MODEL_POSE_TAG = "ModelPose";
  public static final String LEGACY_DATA_MODEL_LOCK_ROTATION_TAG = "ModelLockRotation";
  public static final String LEGACY_DATA_MODEL_HEAD_ROTATION_TAG = "ModelHeadRotation";
  public static final String LEGACY_DATA_MODEL_BODY_ROTATION_TAG = "ModelBodyRotation";
  public static final String LEGACY_DATA_MODEL_LEFT_ARM_ROTATION_TAG = "ModelLeftArmRotation";
  public static final String LEGACY_DATA_MODEL_RIGHT_ARM_ROTATION_TAG = "ModelRightArmRotation";
  public static final String LEGACY_DATA_MODEL_LEFT_LEG_ROTATION_TAG = "ModelLeftLegRotation";
  public static final String LEGACY_DATA_MODEL_RIGHT_LEG_ROTATION_TAG = "ModelRightLegRotation";
  public static final String LEGACY_DATA_MODEL_ROOT_ROTATION_TAG = "ModelRootRotation";

  // Defaults
  CustomPosition DEFAULT_MODEL_PART_POSITION = new CustomPosition(0, 0, 0);
  Rotations DEFAULT_MODEL_PART_ROTATION = new Rotations(0, 0, 0);

  default ModelPose getModelPose() {
    return getEntityData(DATA_MODEL_POSE);
  }

  default CustomPosition getModelPartPosition(ModelPart modelPart) {
    switch (modelPart) {
      case HEAD:
        return getModelHeadPosition();
      case BODY:
        return getModelBodyPosition();
      case ARMS:
        return getModelArmsPosition();
      case LEFT_ARM:
        return getModelLeftArmPosition();
      case RIGHT_ARM:
        return getModelRightArmPosition();
      case LEFT_LEG:
        return getModelLeftLegPosition();
      case RIGHT_LEG:
        return getModelRightLegPosition();
      default:
        return DEFAULT_MODEL_PART_POSITION;
    }
  }

  default Rotations getModelPartRotation(ModelPart modelPart) {
    switch (modelPart) {
      case HEAD:
        return getModelHeadRotation();
      case BODY:
        return getModelBodyRotation();
      case ARMS:
        return getModelArmsRotation();
      case LEFT_ARM:
        return getModelLeftArmRotation();
      case RIGHT_ARM:
        return getModelRightArmRotation();
      case LEFT_LEG:
        return getModelLeftLegRotation();
      case RIGHT_LEG:
        return getModelRightLegRotation();
      default:
        return DEFAULT_MODEL_PART_ROTATION;
    }
  }

  default boolean isModelPartVisible(ModelPart modelPart) {
    switch (modelPart) {
      case HEAD:
        return isModelHeadVisible();
      case BODY:
        return isModelBodyVisible();
      case ARMS:
        return isModelArmsVisible();
      case LEFT_ARM:
        return isModelLeftArmVisible();
      case RIGHT_ARM:
        return isModelRightArmVisible();
      case LEFT_LEG:
        return isModelLeftLegVisible();
      case RIGHT_LEG:
        return isModelRightLegVisible();
      default:
        return false;
    }
  }

  default void setModelPose(ModelPose modelPose) {
    setEntityData(DATA_MODEL_POSE, modelPose);
  }

  default CustomPosition getModelHeadPosition() {
    return getEntityData(DATA_MODEL_HEAD_POSITION);
  }

  default void setModelHeadPosition(CustomPosition modelHeadPosition) {
    setEntityData(DATA_MODEL_HEAD_POSITION, modelHeadPosition);
  }

  default CustomPosition getModelBodyPosition() {
    return getEntityData(DATA_MODEL_BODY_POSITION);
  }

  default void setModelBodyPosition(CustomPosition modelBodyPosition) {
    setEntityData(DATA_MODEL_BODY_POSITION, modelBodyPosition);
  }

  default CustomPosition getModelArmsPosition() {
    return getEntityData(DATA_MODEL_ARMS_POSITION);
  }

  default void setModelArmsPosition(CustomPosition modelArmsPosition) {
    setEntityData(DATA_MODEL_ARMS_POSITION, modelArmsPosition);
  }

  default CustomPosition getModelLeftArmPosition() {
    return getEntityData(DATA_MODEL_LEFT_ARM_POSITION);
  }

  default void setModelLeftArmPosition(CustomPosition modelLeftArmPosition) {
    setEntityData(DATA_MODEL_LEFT_ARM_POSITION, modelLeftArmPosition);
  }

  default CustomPosition getModelRightArmPosition() {
    return getEntityData(DATA_MODEL_RIGHT_ARM_POSITION);
  }

  default void setModelRightArmPosition(CustomPosition modelRightArmPosition) {
    setEntityData(DATA_MODEL_RIGHT_ARM_POSITION, modelRightArmPosition);
  }

  default CustomPosition getModelLeftLegPosition() {
    return getEntityData(DATA_MODEL_LEFT_LEG_POSITION);
  }

  default void setModelLeftLegPosition(CustomPosition modelLeftLegPosition) {
    setEntityData(DATA_MODEL_LEFT_LEG_POSITION, modelLeftLegPosition);
  }

  default CustomPosition getModelRightLegPosition() {
    return getEntityData(DATA_MODEL_RIGHT_LEG_POSITION);
  }

  default void setModelRightLegPosition(CustomPosition modelRightLegPosition) {
    setEntityData(DATA_MODEL_RIGHT_LEG_POSITION, modelRightLegPosition);
  }

  default boolean getModelLockRotation() {
    return getEntityData(DATA_MODEL_LOCK_ROTATION);
  }

  default void setModelLockRotation(boolean modelLockRotation) {
    setEntityData(DATA_MODEL_LOCK_ROTATION, modelLockRotation);
  }

  default Rotations getModelHeadRotation() {
    return getEntityData(DATA_MODEL_HEAD_ROTATION);
  }

  default void setModelHeadRotation(Rotations modelHeadRotation) {
    setEntityData(DATA_MODEL_HEAD_ROTATION, modelHeadRotation);
  }

  default Rotations getModelBodyRotation() {
    return getEntityData(DATA_MODEL_BODY_ROTATION);
  }

  default void setModelBodyRotation(Rotations modelBodyRotation) {
    setEntityData(DATA_MODEL_BODY_ROTATION, modelBodyRotation);
  }

  default Rotations getModelArmsRotation() {
    return getEntityData(DATA_MODEL_ARMS_ROTATION);
  }

  default void setModelArmsRotation(Rotations modelArmsRotation) {
    setEntityData(DATA_MODEL_ARMS_ROTATION, modelArmsRotation);
  }

  default Rotations getModelLeftArmRotation() {
    return getEntityData(DATA_MODEL_LEFT_ARM_ROTATION);
  }

  default void setModelLeftArmRotation(Rotations modelLeftArmRotation) {
    setEntityData(DATA_MODEL_LEFT_ARM_ROTATION, modelLeftArmRotation);
  }

  default Rotations getModelRightArmRotation() {
    return getEntityData(DATA_MODEL_RIGHT_ARM_ROTATION);
  }

  default void setModelRightArmRotation(Rotations modelRightArmRotation) {
    setEntityData(DATA_MODEL_RIGHT_ARM_ROTATION, modelRightArmRotation);
  }

  default Rotations getModelLeftLegRotation() {
    return getEntityData(DATA_MODEL_LEFT_LEG_ROTATION);
  }

  default void setModelLeftLegRotation(Rotations modelLeftLegRotation) {
    setEntityData(DATA_MODEL_LEFT_LEG_ROTATION, modelLeftLegRotation);
  }

  default Rotations getModelRightLegRotation() {
    return getEntityData(DATA_MODEL_RIGHT_LEG_ROTATION);
  }

  default void setModelRightLegRotation(Rotations modelRightLegRotation) {
    setEntityData(DATA_MODEL_RIGHT_LEG_ROTATION, modelRightLegRotation);
  }

  default Rotations getModelRootRotation() {
    return getEntityData(DATA_MODEL_ROOT_ROTATION);
  }

  default void setModelRootRotation(Rotations modelRootRotation) {
    setEntityData(DATA_MODEL_ROOT_ROTATION, modelRootRotation);
  }

  default boolean isModelHeadVisible() {
    return getEntityData(DATA_MODEL_HEAD_VISIBLE);
  }

  default void setModelHeadVisible(boolean modelHeadVisible) {
    setEntityData(DATA_MODEL_HEAD_VISIBLE, modelHeadVisible);
  }

  default boolean isModelBodyVisible() {
    return getEntityData(DATA_MODEL_BODY_VISIBLE);
  }

  default void setModelBodyVisible(boolean modelBodyVisible) {
    setEntityData(DATA_MODEL_BODY_VISIBLE, modelBodyVisible);
  }

  default boolean isModelArmsVisible() {
    return getEntityData(DATA_MODEL_ARMS_VISIBLE);
  }

  default void setModelArmsVisible(boolean modelArmsVisible) {
    setEntityData(DATA_MODEL_ARMS_VISIBLE, modelArmsVisible);
  }

  default boolean isModelLeftArmVisible() {
    return getEntityData(DATA_MODEL_LEFT_ARM_VISIBLE);
  }

  default void setModelLeftArmVisible(boolean modelLeftArmVisible) {
    setEntityData(DATA_MODEL_LEFT_ARM_VISIBLE, modelLeftArmVisible);
  }

  default boolean isModelRightArmVisible() {
    return getEntityData(DATA_MODEL_RIGHT_ARM_VISIBLE);
  }

  default void setModelRightArmVisible(boolean modelRightArmVisible) {
    setEntityData(DATA_MODEL_RIGHT_ARM_VISIBLE, modelRightArmVisible);
  }

  default boolean isModelLeftLegVisible() {
    return getEntityData(DATA_MODEL_LEFT_LEG_VISIBLE);
  }

  default void setModelLeftLegVisible(boolean modelLeftLegVisible) {
    setEntityData(DATA_MODEL_LEFT_LEG_VISIBLE, modelLeftLegVisible);
  }

  default boolean isModelRightLegVisible() {
    return getEntityData(DATA_MODEL_RIGHT_LEG_VISIBLE);
  }

  default void setModelRightLegVisible(boolean modelRightLegVisible) {
    setEntityData(DATA_MODEL_RIGHT_LEG_VISIBLE, modelRightLegVisible);
  }

  default boolean hasHeadModelPart() {
    return true;
  }

  default boolean hasBodyModelPart() {
    return true;
  }

  default boolean hasArmsModelPart() {
    return false;
  }

  default boolean hasLeftArmModelPart() {
    return true;
  }

  default boolean hasRightArmModelPart() {
    return true;
  }

  default boolean hasLeftLegModelPart() {
    return true;
  }

  default boolean hasRightLegModelPart() {
    return true;
  }

  default boolean canUseArmor() {
    return true;
  }

  default boolean canUseMainHand() {
    return true;
  }

  default boolean canUseOffHand() {
    return true;
  }

  default void defineSynchedModelData() {
    defineEntityData(DATA_MODEL_POSE, ModelPose.DEFAULT);

    // Position
    defineEntityData(DATA_MODEL_HEAD_POSITION, new CustomPosition(0, 0, 0));
    defineEntityData(DATA_MODEL_BODY_POSITION, new CustomPosition(0, 0, 0));
    defineEntityData(DATA_MODEL_ARMS_POSITION, new CustomPosition(0, 0, 0));
    defineEntityData(DATA_MODEL_LEFT_ARM_POSITION, new CustomPosition(0, 0, 0));
    defineEntityData(DATA_MODEL_RIGHT_ARM_POSITION, new CustomPosition(0, 0, 0));
    defineEntityData(DATA_MODEL_LEFT_LEG_POSITION, new CustomPosition(0, 0, 0));
    defineEntityData(DATA_MODEL_RIGHT_LEG_POSITION, new CustomPosition(0, 0, 0));

    // Rotation
    defineEntityData(DATA_MODEL_LOCK_ROTATION, false);
    defineEntityData(DATA_MODEL_HEAD_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_BODY_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_ARMS_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_LEFT_ARM_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_RIGHT_ARM_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_LEFT_LEG_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_RIGHT_LEG_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_ROOT_ROTATION, new Rotations(0, 0, 0));

    // Visibility
    defineEntityData(DATA_MODEL_HEAD_VISIBLE, this.hasHeadModelPart());
    defineEntityData(DATA_MODEL_BODY_VISIBLE, this.hasBodyModelPart());
    defineEntityData(DATA_MODEL_ARMS_VISIBLE, this.hasArmsModelPart());
    defineEntityData(DATA_MODEL_LEFT_ARM_VISIBLE, this.hasLeftArmModelPart());
    defineEntityData(DATA_MODEL_RIGHT_ARM_VISIBLE, this.hasRightArmModelPart());
    defineEntityData(DATA_MODEL_LEFT_LEG_VISIBLE, this.hasLeftLegModelPart());
    defineEntityData(DATA_MODEL_RIGHT_LEG_VISIBLE, this.hasRightLegModelPart());
  }

  default void addAdditionalModelData(CompoundTag compoundTag) {
    CompoundTag modelDataTag = new CompoundTag();

    // Model Pose
    if (this.getModelPose() != null) {
      modelDataTag.putString(DATA_MODEL_POSE_TAG, this.getModelPose().name());
    }

    // Model Head Position
    CompoundTag positionsTag = new CompoundTag();
    if (this.getModelHeadPosition() != null) {
      positionsTag.put(DATA_MODEL_HEAD_POSITION_TAG, this.getModelHeadPosition().save());
    }
    if (this.getModelBodyPosition() != null) {
      positionsTag.put(DATA_MODEL_BODY_POSITION_TAG, this.getModelBodyPosition().save());
    }
    if (this.getModelArmsPosition() != null) {
      positionsTag.put(DATA_MODEL_ARMS_POSITION_TAG, this.getModelArmsPosition().save());
    }
    if (this.getModelLeftArmPosition() != null) {
      positionsTag.put(DATA_MODEL_LEFT_ARM_POSITION_TAG, this.getModelLeftArmPosition().save());
    }
    if (this.getModelRightArmPosition() != null) {
      positionsTag.put(DATA_MODEL_RIGHT_ARM_POSITION_TAG, this.getModelRightArmPosition().save());
    }
    if (this.getModelLeftLegPosition() != null) {
      positionsTag.put(DATA_MODEL_LEFT_LEG_POSITION_TAG, this.getModelLeftLegPosition().save());
    }
    if (this.getModelRightLegPosition() != null) {
      positionsTag.put(DATA_MODEL_RIGHT_LEG_POSITION_TAG, this.getModelRightLegPosition().save());
    }
    modelDataTag.put(DATA_MODEL_POSITION_TAG, positionsTag);

    // Model Rotations
    CompoundTag rotationsTag = new CompoundTag();
    rotationsTag.putBoolean(DATA_MODEL_LOCK_ROTATION_TAG, this.getModelLockRotation());
    if (this.getModelHeadRotation() != null) {
      rotationsTag.put(DATA_MODEL_HEAD_ROTATION_TAG, this.getModelHeadRotation().save());
    }
    if (this.getModelBodyRotation() != null) {
      rotationsTag.put(DATA_MODEL_BODY_ROTATION_TAG, this.getModelBodyRotation().save());
    }
    if (this.getModelArmsRotation() != null) {
      rotationsTag.put(DATA_MODEL_ARMS_ROTATION_TAG, this.getModelArmsRotation().save());
    }
    if (this.getModelLeftArmRotation() != null) {
      rotationsTag.put(DATA_MODEL_LEFT_ARM_ROTATION_TAG, this.getModelLeftArmRotation().save());
    }
    if (this.getModelRightArmRotation() != null) {
      rotationsTag.put(DATA_MODEL_RIGHT_ARM_ROTATION_TAG, this.getModelRightArmRotation().save());
    }
    if (this.getModelLeftLegRotation() != null) {
      rotationsTag.put(DATA_MODEL_LEFT_LEG_ROTATION_TAG, this.getModelLeftLegRotation().save());
    }
    if (this.getModelRightLegRotation() != null) {
      rotationsTag.put(DATA_MODEL_RIGHT_LEG_ROTATION_TAG, this.getModelRightLegRotation().save());
    }
    if (this.getModelRootRotation() != null) {
      rotationsTag.put(DATA_MODEL_ROOT_ROTATION_TAG, this.getModelRootRotation().save());
    }
    modelDataTag.put(DATA_MODEL_ROTATION_TAG, rotationsTag);

    // Model Visibility
    CompoundTag visibilityTag = new CompoundTag();
    visibilityTag.putBoolean(DATA_MODEL_HEAD_VISIBLE_TAG, this.isModelHeadVisible());
    visibilityTag.putBoolean(DATA_MODEL_BODY_VISIBLE_TAG, this.isModelBodyVisible());
    visibilityTag.putBoolean(DATA_MODEL_ARMS_VISIBLE_TAG, this.isModelArmsVisible());
    visibilityTag.putBoolean(DATA_MODEL_LEFT_ARM_VISIBLE_TAG, this.isModelLeftArmVisible());
    visibilityTag.putBoolean(DATA_MODEL_RIGHT_ARM_VISIBLE_TAG, this.isModelRightArmVisible());
    visibilityTag.putBoolean(DATA_MODEL_LEFT_LEG_VISIBLE_TAG, this.isModelLeftLegVisible());
    visibilityTag.putBoolean(DATA_MODEL_RIGHT_LEG_VISIBLE_TAG, this.isModelRightLegVisible());
    modelDataTag.put(DATA_MODEL_VISIBLE_TAG, visibilityTag);

    compoundTag.put(DATA_MODEL_DATA_TAG, modelDataTag);
  }

  default void readAdditionalModelData(CompoundTag compoundTag) {

    // Legacy data support
    readAdditionalLegacyModelData(compoundTag);

    // Early exit if no model data is available
    if (!compoundTag.contains(DATA_MODEL_DATA_TAG)) {
      return;
    }

    // Read model data
    CompoundTag modelDataTag = compoundTag.getCompound(DATA_MODEL_DATA_TAG);

    // Model Pose
    if (modelDataTag.contains(DATA_MODEL_POSE_TAG)) {
      String modelPose = modelDataTag.getString(DATA_MODEL_POSE_TAG);
      if (modelPose != null && !modelPose.isEmpty()) {
        setModelPose(ModelPose.get(modelPose));
      }
    }

    // Model Position
    if (modelDataTag.contains(DATA_MODEL_POSITION_TAG)) {
      CompoundTag positionTag = modelDataTag.getCompound(DATA_MODEL_POSITION_TAG);
      if (positionTag.contains(DATA_MODEL_HEAD_POSITION_TAG)) {
        setModelHeadPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_HEAD_POSITION_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_BODY_POSITION_TAG)) {
        setModelBodyPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_BODY_POSITION_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_ARMS_POSITION_TAG)) {
        setModelArmsPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_ARMS_POSITION_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_LEFT_ARM_POSITION_TAG)) {
        setModelLeftArmPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_LEFT_ARM_POSITION_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_RIGHT_ARM_POSITION_TAG)) {
        setModelRightArmPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_RIGHT_ARM_POSITION_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_LEFT_LEG_POSITION_TAG)) {
        setModelLeftLegPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_LEFT_LEG_POSITION_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_RIGHT_LEG_POSITION_TAG)) {
        setModelRightLegPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_RIGHT_LEG_POSITION_TAG, 5)));
      }
    }

    // Model Rotations
    if (modelDataTag.contains(DATA_MODEL_ROTATION_TAG)) {
      CompoundTag rotationsTag = modelDataTag.getCompound(DATA_MODEL_ROTATION_TAG);
      if (rotationsTag.contains(DATA_MODEL_LOCK_ROTATION_TAG)) {
        setModelLockRotation(rotationsTag.getBoolean(DATA_MODEL_LOCK_ROTATION_TAG));
      }
      if (rotationsTag.contains(DATA_MODEL_HEAD_ROTATION_TAG)) {
        setModelHeadRotation(new Rotations(rotationsTag.getList(DATA_MODEL_HEAD_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_BODY_ROTATION_TAG)) {
        setModelBodyRotation(new Rotations(rotationsTag.getList(DATA_MODEL_BODY_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_ARMS_ROTATION_TAG)) {
        setModelArmsRotation(new Rotations(rotationsTag.getList(DATA_MODEL_ARMS_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
        setModelLeftArmRotation(
            new Rotations(rotationsTag.getList(DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
        setModelRightArmRotation(
            new Rotations(rotationsTag.getList(DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
        setModelLeftLegRotation(
            new Rotations(rotationsTag.getList(DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
        setModelRightLegRotation(
            new Rotations(rotationsTag.getList(DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_ROOT_ROTATION_TAG)) {
        setModelRootRotation(new Rotations(rotationsTag.getList(DATA_MODEL_ROOT_ROTATION_TAG, 5)));
      }
    }

    // Model Visibility
    if (modelDataTag.contains(DATA_MODEL_VISIBLE_TAG)) {
      CompoundTag visibilityTag = modelDataTag.getCompound(DATA_MODEL_VISIBLE_TAG);
      if (visibilityTag.contains(DATA_MODEL_HEAD_VISIBLE_TAG)) {
        setModelHeadVisible(visibilityTag.getBoolean(DATA_MODEL_HEAD_VISIBLE_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_BODY_VISIBLE_TAG)) {
        setModelBodyVisible(visibilityTag.getBoolean(DATA_MODEL_BODY_VISIBLE_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_ARMS_VISIBLE_TAG)) {
        setModelArmsVisible(visibilityTag.getBoolean(DATA_MODEL_ARMS_VISIBLE_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_LEFT_ARM_VISIBLE_TAG)) {
        setModelLeftArmVisible(visibilityTag.getBoolean(DATA_MODEL_LEFT_ARM_VISIBLE_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_RIGHT_ARM_VISIBLE_TAG)) {
        setModelRightArmVisible(visibilityTag.getBoolean(DATA_MODEL_RIGHT_ARM_VISIBLE_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_LEFT_LEG_VISIBLE_TAG)) {
        setModelLeftLegVisible(visibilityTag.getBoolean(DATA_MODEL_LEFT_LEG_VISIBLE_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_RIGHT_LEG_VISIBLE_TAG)) {
        setModelRightLegVisible(visibilityTag.getBoolean(DATA_MODEL_RIGHT_LEG_VISIBLE_TAG));
      }
    }
  }

  default void readAdditionalLegacyModelData(CompoundTag compoundTag) {
    if (compoundTag.contains(LEGACY_DATA_MODEL_POSE_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_LOCK_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_HEAD_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_BODY_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_LEFT_ARM_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_LEFT_LEG_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)
        || compoundTag.contains(LEGACY_DATA_MODEL_ROOT_ROTATION_TAG)) {
      log.info("Converting legacy model data to new format for {}", this);
      if (compoundTag.contains(LEGACY_DATA_MODEL_POSE_TAG)) {
        String modelPose = compoundTag.getString(LEGACY_DATA_MODEL_POSE_TAG);
        if (modelPose != null && !modelPose.isEmpty()) {
          setModelPose(ModelPose.get(modelPose));
        }
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_LOCK_ROTATION_TAG)) {
        setModelLockRotation(compoundTag.getBoolean(LEGACY_DATA_MODEL_LOCK_ROTATION_TAG));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_HEAD_ROTATION_TAG)) {
        setModelHeadRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_HEAD_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_BODY_ROTATION_TAG)) {
        setModelBodyRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_BODY_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
        setModelLeftArmRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
        setModelRightArmRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
        setModelLeftLegRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
        setModelRightLegRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_DATA_MODEL_ROOT_ROTATION_TAG)) {
        setModelRootRotation(
            new Rotations(compoundTag.getList(LEGACY_DATA_MODEL_ROOT_ROTATION_TAG, 5)));
      }
    }
  }

}
