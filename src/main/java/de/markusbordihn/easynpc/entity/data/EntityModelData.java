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

package de.markusbordihn.easynpc.entity.data;

import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.CustomScale;
import de.markusbordihn.easynpc.data.entity.CustomDataSerializers;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import de.markusbordihn.easynpc.entity.data.legacy.LegacyEntityModelData;
import net.minecraft.core.Rotations;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;

public interface EntityModelData extends EntityDataInterface {

  // Synced entity data
  EntityDataAccessor<ModelPose> DATA_MODEL_POSE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.MODEL_POSE);

  // Synced entity data for Model Part Positions
  EntityDataAccessor<CustomPosition> DATA_MODEL_HEAD_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  EntityDataAccessor<CustomPosition> DATA_MODEL_BODY_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  EntityDataAccessor<CustomPosition> DATA_MODEL_ARMS_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  EntityDataAccessor<CustomPosition> DATA_MODEL_LEFT_ARM_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  EntityDataAccessor<CustomPosition> DATA_MODEL_RIGHT_ARM_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  EntityDataAccessor<CustomPosition> DATA_MODEL_LEFT_LEG_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);
  EntityDataAccessor<CustomPosition> DATA_MODEL_RIGHT_LEG_POSITION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.POSITION);

  // Synced entity data for Model Part Rotations
  EntityDataAccessor<Boolean> DATA_MODEL_LOCK_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Rotations> DATA_MODEL_HEAD_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_BODY_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_ARMS_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_LEFT_ARM_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_RIGHT_ARM_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_LEFT_LEG_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_RIGHT_LEG_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  EntityDataAccessor<Rotations> DATA_MODEL_ROOT_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);

  // Synced entity data for Model Part Scaling
  EntityDataAccessor<CustomScale> DATA_MODEL_HEAD_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);
  EntityDataAccessor<CustomScale> DATA_MODEL_BODY_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);
  EntityDataAccessor<CustomScale> DATA_MODEL_ARMS_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);
  EntityDataAccessor<CustomScale> DATA_MODEL_LEFT_ARM_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);
  EntityDataAccessor<CustomScale> DATA_MODEL_RIGHT_ARM_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);
  EntityDataAccessor<CustomScale> DATA_MODEL_LEFT_LEG_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);
  EntityDataAccessor<CustomScale> DATA_MODEL_RIGHT_LEG_SCALE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SCALE);

  // Synced entity data for Model Part Visibility
  EntityDataAccessor<Boolean> DATA_MODEL_HEAD_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_MODEL_BODY_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_MODEL_ARMS_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_MODEL_LEFT_ARM_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_MODEL_RIGHT_ARM_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_MODEL_LEFT_LEG_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_MODEL_RIGHT_LEG_VISIBLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // CompoundTags
  String DATA_MODEL_VERSION_TAG = "Version";
  String DATA_MODEL_DATA_TAG = "ModelData";
  String DATA_MODEL_POSE_TAG = "Pose";
  String DATA_MODEL_HEAD_TAG = "Head";
  String DATA_MODEL_BODY_TAG = "Body";
  String DATA_MODEL_ARMS_TAG = "Arms";
  String DATA_MODEL_LEFT_ARM_TAG = "LeftArm";
  String DATA_MODEL_RIGHT_ARM_TAG = "RightArm";
  String DATA_MODEL_LEFT_LEG_TAG = "LeftLeg";
  String DATA_MODEL_RIGHT_LEG_TAG = "RightLeg";
  String DATA_MODEL_ROOT_TAG = "Root";
  String DATA_MODEL_LOCK_TAG = "Lock";

  // CompoundTags for Model Part Positions
  String DATA_MODEL_POSITION_TAG = "Position";

  // CompoundTags for Model Part Rotations
  String DATA_MODEL_ROTATION_TAG = "Rotation";

  // CompoundTags for Model Part Scaling
  String DATA_MODEL_SCALE_TAG = "Scale";

  // CompoundTags for Model Part Visibility
  String DATA_MODEL_VISIBLE_TAG = "Visible";

  // Defaults
  CustomPosition DEFAULT_MODEL_PART_POSITION = new CustomPosition(0, 0, 0);
  Rotations DEFAULT_MODEL_PART_ROTATION = new Rotations(0, 0, 0);
  CustomScale DEFAULT_MODEL_PART_SCALE = new CustomScale(1, 1, 1);

  default ModelPose getModelPose() {
    return getEntityData(DATA_MODEL_POSE);
  }

  default void setModelPose(ModelPose modelPose) {
    setEntityData(DATA_MODEL_POSE, modelPose);
  }

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

  default Rotations getModelPartRotation(ModelPart modelPart) {
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

  default CustomScale getModelPartScale(ModelPart modelPart) {
    return switch (modelPart) {
      case HEAD -> getModelHeadScale();
      case BODY -> getModelBodyScale();
      case ARMS -> getModelArmsScale();
      case LEFT_ARM -> getModelLeftArmScale();
      case RIGHT_ARM -> getModelRightArmScale();
      case LEFT_LEG -> getModelLeftLegScale();
      case RIGHT_LEG -> getModelRightLegScale();
      default -> DEFAULT_MODEL_PART_SCALE;
    };
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

  default CustomScale getModelHeadScale() {
    return getEntityData(DATA_MODEL_HEAD_SCALE);
  }

  default void setModelHeadScale(CustomScale modelHeadScale) {
    setEntityData(DATA_MODEL_HEAD_SCALE, modelHeadScale);
  }

  default CustomScale getModelBodyScale() {
    return getEntityData(DATA_MODEL_BODY_SCALE);
  }

  default void setModelBodyScale(CustomScale modelBodyScale) {
    setEntityData(DATA_MODEL_BODY_SCALE, modelBodyScale);
  }

  default CustomScale getModelArmsScale() {
    return getEntityData(DATA_MODEL_ARMS_SCALE);
  }

  default void setModelArmsScale(CustomScale modelArmsScale) {
    setEntityData(DATA_MODEL_ARMS_SCALE, modelArmsScale);
  }

  default CustomScale getModelLeftArmScale() {
    return getEntityData(DATA_MODEL_LEFT_ARM_SCALE);
  }

  default void setModelLeftArmScale(CustomScale modelLeftArmScale) {
    setEntityData(DATA_MODEL_LEFT_ARM_SCALE, modelLeftArmScale);
  }

  default CustomScale getModelRightArmScale() {
    return getEntityData(DATA_MODEL_RIGHT_ARM_SCALE);
  }

  default void setModelRightArmScale(CustomScale modelRightArmScale) {
    setEntityData(DATA_MODEL_RIGHT_ARM_SCALE, modelRightArmScale);
  }

  default CustomScale getModelLeftLegScale() {
    return getEntityData(DATA_MODEL_LEFT_LEG_SCALE);
  }

  default void setModelLeftLegScale(CustomScale modelLeftLegScale) {
    setEntityData(DATA_MODEL_LEFT_LEG_SCALE, modelLeftLegScale);
  }

  default CustomScale getModelRightLegScale() {
    return getEntityData(DATA_MODEL_RIGHT_LEG_SCALE);
  }

  default void setModelRightLegScale(CustomScale modelRightLegScale) {
    setEntityData(DATA_MODEL_RIGHT_LEG_SCALE, modelRightLegScale);
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

    // Scale
    defineEntityData(DATA_MODEL_HEAD_SCALE, new CustomScale(1, 1, 1));
    defineEntityData(DATA_MODEL_BODY_SCALE, new CustomScale(1, 1, 1));
    defineEntityData(DATA_MODEL_ARMS_SCALE, new CustomScale(1, 1, 1));
    defineEntityData(DATA_MODEL_LEFT_ARM_SCALE, new CustomScale(1, 1, 1));
    defineEntityData(DATA_MODEL_RIGHT_ARM_SCALE, new CustomScale(1, 1, 1));
    defineEntityData(DATA_MODEL_LEFT_LEG_SCALE, new CustomScale(1, 1, 1));
    defineEntityData(DATA_MODEL_RIGHT_LEG_SCALE, new CustomScale(1, 1, 1));

    // Visibility
    defineEntityData(DATA_MODEL_HEAD_VISIBLE, this.hasHeadModelPart());
    defineEntityData(DATA_MODEL_BODY_VISIBLE, this.hasBodyModelPart());
    defineEntityData(DATA_MODEL_ARMS_VISIBLE, this.hasArmsModelPart());
    defineEntityData(DATA_MODEL_LEFT_ARM_VISIBLE, this.hasLeftArmModelPart());
    defineEntityData(DATA_MODEL_RIGHT_ARM_VISIBLE, this.hasRightArmModelPart());
    defineEntityData(DATA_MODEL_LEFT_LEG_VISIBLE, this.hasLeftLegModelPart());
    defineEntityData(DATA_MODEL_RIGHT_LEG_VISIBLE, this.hasRightLegModelPart());
  }

  private void addAdditionalModelPositionData(CompoundTag compoundTag) {
    CompoundTag positionsTag = new CompoundTag();
    if (this.getModelHeadPosition() != null && !this.getModelHeadPosition().isZero()) {
      positionsTag.put(DATA_MODEL_HEAD_TAG, this.getModelHeadPosition().save());
    }
    if (this.getModelBodyPosition() != null && !this.getModelBodyPosition().isZero()) {
      positionsTag.put(DATA_MODEL_BODY_TAG, this.getModelBodyPosition().save());
    }
    if (this.getModelArmsPosition() != null && !this.getModelArmsPosition().isZero()) {
      positionsTag.put(DATA_MODEL_ARMS_TAG, this.getModelArmsPosition().save());
    }
    if (this.getModelLeftArmPosition() != null && !this.getModelLeftArmPosition().isZero()) {
      positionsTag.put(DATA_MODEL_LEFT_ARM_TAG, this.getModelLeftArmPosition().save());
    }
    if (this.getModelRightArmPosition() != null && !this.getModelRightArmPosition().isZero()) {
      positionsTag.put(DATA_MODEL_RIGHT_ARM_TAG, this.getModelRightArmPosition().save());
    }
    if (this.getModelLeftLegPosition() != null && !this.getModelLeftLegPosition().isZero()) {
      positionsTag.put(DATA_MODEL_LEFT_LEG_TAG, this.getModelLeftLegPosition().save());
    }
    if (this.getModelRightLegPosition() != null && !this.getModelRightLegPosition().isZero()) {
      positionsTag.put(DATA_MODEL_RIGHT_LEG_TAG, this.getModelRightLegPosition().save());
    }
    compoundTag.put(DATA_MODEL_POSITION_TAG, positionsTag);
  }

  private void addAdditionalModelRotationData(CompoundTag compoundTag) {
    CompoundTag rotationsTag = new CompoundTag();
    if (!this.getModelLockRotation()) {
      rotationsTag.putBoolean(DATA_MODEL_LOCK_TAG, this.getModelLockRotation());
    }
    if (this.getModelHeadRotation() != null
        && !this.getModelHeadRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_HEAD_TAG, this.getModelHeadRotation().save());
    }
    if (this.getModelBodyRotation() != null
        && !this.getModelBodyRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_BODY_TAG, this.getModelBodyRotation().save());
    }
    if (this.getModelArmsRotation() != null
        && !this.getModelArmsRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_ARMS_TAG, this.getModelArmsRotation().save());
    }
    if (this.getModelLeftArmRotation() != null
        && !this.getModelLeftArmRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_LEFT_ARM_TAG, this.getModelLeftArmRotation().save());
    }
    if (this.getModelRightArmRotation() != null
        && !this.getModelRightArmRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_RIGHT_ARM_TAG, this.getModelRightArmRotation().save());
    }
    if (this.getModelLeftLegRotation() != null
        && !this.getModelLeftLegRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_LEFT_LEG_TAG, this.getModelLeftLegRotation().save());
    }
    if (this.getModelRightLegRotation() != null
        && !this.getModelRightLegRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_RIGHT_LEG_TAG, this.getModelRightLegRotation().save());
    }
    if (this.getModelRootRotation() != null
        && !this.getModelRootRotation().equals(DEFAULT_MODEL_PART_ROTATION)) {
      rotationsTag.put(DATA_MODEL_ROOT_TAG, this.getModelRootRotation().save());
    }
    compoundTag.put(DATA_MODEL_ROTATION_TAG, rotationsTag);
  }

  private void addAdditionalModelVisibilityData(CompoundTag compoundTag) {
    CompoundTag visibilityTag = new CompoundTag();
    if (this.isModelHeadVisible() != this.hasHeadModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_HEAD_TAG, this.isModelHeadVisible());
    }
    if (this.isModelBodyVisible() != this.hasBodyModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_BODY_TAG, this.isModelBodyVisible());
    }
    if (this.isModelArmsVisible() != this.hasArmsModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_ARMS_TAG, this.isModelArmsVisible());
    }
    if (this.isModelLeftArmVisible() != this.hasLeftArmModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_LEFT_ARM_TAG, this.isModelLeftArmVisible());
    }
    if (this.isModelRightArmVisible() != this.hasRightArmModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_RIGHT_ARM_TAG, this.isModelRightArmVisible());
    }
    if (this.isModelLeftLegVisible() != this.hasLeftLegModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_LEFT_LEG_TAG, this.isModelLeftLegVisible());
    }
    if (this.isModelRightLegVisible() != this.hasRightLegModelPart()) {
      visibilityTag.putBoolean(DATA_MODEL_RIGHT_LEG_TAG, this.isModelRightLegVisible());
    }
    compoundTag.put(DATA_MODEL_VISIBLE_TAG, visibilityTag);
  }

  default void addAdditionalModelData(CompoundTag compoundTag) {
    CompoundTag modelDataTag = new CompoundTag();

    // Model Pose
    if (this.getModelPose() != null) {
      modelDataTag.putString(DATA_MODEL_POSE_TAG, this.getModelPose().name());
    }

    // Model Position
    this.addAdditionalModelPositionData(modelDataTag);

    // Model Rotations
    this.addAdditionalModelRotationData(modelDataTag);

    // Model Visibility
    this.addAdditionalModelVisibilityData(modelDataTag);

    // Version
    modelDataTag.putInt(DATA_MODEL_VERSION_TAG, 3);

    compoundTag.put(DATA_MODEL_DATA_TAG, modelDataTag);
  }

  default void readAdditionalModelData(CompoundTag compoundTag) {

    // Legacy data support
    if (LegacyEntityModelData.readAdditionalLegacyModelData(compoundTag, this.getEntity())) {
      return;
    }

    // Early exit if no model data is available
    if (!compoundTag.contains(DATA_MODEL_DATA_TAG)) {
      return;
    }

    // Read model data
    CompoundTag modelDataTag = compoundTag.getCompound(DATA_MODEL_DATA_TAG);

    // Model Pose
    if (modelDataTag.contains(DATA_MODEL_POSE_TAG)) {
      String modelPose = modelDataTag.getString(DATA_MODEL_POSE_TAG);
      if (!modelPose.isEmpty()) {
        setModelPose(ModelPose.get(modelPose));
      }
    }

    // Model Position
    if (modelDataTag.contains(DATA_MODEL_POSITION_TAG)) {
      CompoundTag positionTag = modelDataTag.getCompound(DATA_MODEL_POSITION_TAG);
      if (positionTag.contains(DATA_MODEL_HEAD_TAG)) {
        setModelHeadPosition(new CustomPosition(positionTag.getList(DATA_MODEL_HEAD_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_BODY_TAG)) {
        setModelBodyPosition(new CustomPosition(positionTag.getList(DATA_MODEL_BODY_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_ARMS_TAG)) {
        setModelArmsPosition(new CustomPosition(positionTag.getList(DATA_MODEL_ARMS_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_LEFT_ARM_TAG)) {
        setModelLeftArmPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_LEFT_ARM_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_RIGHT_ARM_TAG)) {
        setModelRightArmPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_RIGHT_ARM_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_LEFT_LEG_TAG)) {
        setModelLeftLegPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_LEFT_LEG_TAG, 5)));
      }
      if (positionTag.contains(DATA_MODEL_RIGHT_LEG_TAG)) {
        setModelRightLegPosition(
            new CustomPosition(positionTag.getList(DATA_MODEL_RIGHT_LEG_TAG, 5)));
      }
    }

    // Model Rotations
    if (modelDataTag.contains(DATA_MODEL_ROTATION_TAG)) {
      CompoundTag rotationsTag = modelDataTag.getCompound(DATA_MODEL_ROTATION_TAG);
      if (rotationsTag.contains(DATA_MODEL_LOCK_TAG)) {
        setModelLockRotation(rotationsTag.getBoolean(DATA_MODEL_LOCK_TAG));
      }
      if (rotationsTag.contains(DATA_MODEL_HEAD_TAG)) {
        setModelHeadRotation(new Rotations(rotationsTag.getList(DATA_MODEL_HEAD_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_BODY_TAG)) {
        setModelBodyRotation(new Rotations(rotationsTag.getList(DATA_MODEL_BODY_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_ARMS_TAG)) {
        setModelArmsRotation(new Rotations(rotationsTag.getList(DATA_MODEL_ARMS_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_LEFT_ARM_TAG)) {
        setModelLeftArmRotation(new Rotations(rotationsTag.getList(DATA_MODEL_LEFT_ARM_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_RIGHT_ARM_TAG)) {
        setModelRightArmRotation(new Rotations(rotationsTag.getList(DATA_MODEL_RIGHT_ARM_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_LEFT_LEG_TAG)) {
        setModelLeftLegRotation(new Rotations(rotationsTag.getList(DATA_MODEL_LEFT_LEG_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_RIGHT_LEG_TAG)) {
        setModelRightLegRotation(new Rotations(rotationsTag.getList(DATA_MODEL_RIGHT_LEG_TAG, 5)));
      }
      if (rotationsTag.contains(DATA_MODEL_ROOT_TAG)) {
        setModelRootRotation(new Rotations(rotationsTag.getList(DATA_MODEL_ROOT_TAG, 5)));
      }
    }

    // Model Visibility
    if (modelDataTag.contains(DATA_MODEL_VISIBLE_TAG)) {
      CompoundTag visibilityTag = modelDataTag.getCompound(DATA_MODEL_VISIBLE_TAG);
      if (visibilityTag.contains(DATA_MODEL_HEAD_TAG)) {
        setModelHeadVisible(visibilityTag.getBoolean(DATA_MODEL_HEAD_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_BODY_TAG)) {
        setModelBodyVisible(visibilityTag.getBoolean(DATA_MODEL_BODY_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_ARMS_TAG)) {
        setModelArmsVisible(visibilityTag.getBoolean(DATA_MODEL_ARMS_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_LEFT_ARM_TAG)) {
        setModelLeftArmVisible(visibilityTag.getBoolean(DATA_MODEL_LEFT_ARM_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_RIGHT_ARM_TAG)) {
        setModelRightArmVisible(visibilityTag.getBoolean(DATA_MODEL_RIGHT_ARM_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_LEFT_LEG_TAG)) {
        setModelLeftLegVisible(visibilityTag.getBoolean(DATA_MODEL_LEFT_LEG_TAG));
      }
      if (visibilityTag.contains(DATA_MODEL_RIGHT_LEG_TAG)) {
        setModelRightLegVisible(visibilityTag.getBoolean(DATA_MODEL_RIGHT_LEG_TAG));
      }
    }
  }
}
