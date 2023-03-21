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

import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import de.markusbordihn.easynpc.model.ModelPose;

public interface EntityModelData extends EntityDataInterface {

  // Synced entity data
  public static final EntityDataAccessor<ModelPose> DATA_MODEL_POSE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.MODEL_POSE);
  public static final EntityDataAccessor<Boolean> DATA_MODEL_LOCK_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_HEAD_ROTATION =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.ROTATIONS);
  public static final EntityDataAccessor<Rotations> DATA_MODEL_BODY_ROTATION =
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

  // CompoundTags
  public static final String DATA_MODEL_POSE_TAG = "ModelPose";
  public static final String DATA_MODEL_LOCK_ROTATION_TAG = "ModelLockRotation";
  public static final String DATA_MODEL_HEAD_ROTATION_TAG = "ModelHeadRotation";
  public static final String DATA_MODEL_BODY_ROTATION_TAG = "ModelBodyRotation";
  public static final String DATA_MODEL_LEFT_ARM_ROTATION_TAG = "ModelLeftArmRotation";
  public static final String DATA_MODEL_RIGHT_ARM_ROTATION_TAG = "ModelRightArmRotation";
  public static final String DATA_MODEL_LEFT_LEG_ROTATION_TAG = "ModelLeftLegRotation";
  public static final String DATA_MODEL_RIGHT_LEG_ROTATION_TAG = "ModelRightLegRotation";
  public static final String DATA_MODEL_ROOT_ROTATION_TAG = "ModelRootRotation";

  default ModelPose getModelPose() {
    return getEntityData(DATA_MODEL_POSE);
  }

  default void setModelPose(ModelPose modelPose) {
    setEntityData(DATA_MODEL_POSE, modelPose);
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

  default boolean hasHeadModelPart() {
    return true;
  }

  default boolean hasBodyModelPart() {
    return true;
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
    defineEntityData(DATA_MODEL_LOCK_ROTATION, false);
    defineEntityData(DATA_MODEL_HEAD_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_BODY_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_LEFT_ARM_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_RIGHT_ARM_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_LEFT_LEG_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_RIGHT_LEG_ROTATION, new Rotations(0, 0, 0));
    defineEntityData(DATA_MODEL_ROOT_ROTATION, new Rotations(0, 0, 0));
  }

  default void addAdditionalModelData(CompoundTag compoundTag) {
    if (this.getModelPose() != null) {
      compoundTag.putString(DATA_MODEL_POSE_TAG, this.getModelPose().name());
    }
    compoundTag.putBoolean(DATA_MODEL_LOCK_ROTATION_TAG, this.getModelLockRotation());
    if (this.getModelHeadRotation() != null) {
      compoundTag.put(DATA_MODEL_HEAD_ROTATION_TAG, this.getModelHeadRotation().save());
    }
    if (this.getModelBodyRotation() != null) {
      compoundTag.put(DATA_MODEL_BODY_ROTATION_TAG, this.getModelBodyRotation().save());
    }
    if (this.getModelLeftArmRotation() != null) {
      compoundTag.put(DATA_MODEL_LEFT_ARM_ROTATION_TAG, this.getModelLeftArmRotation().save());
    }
    if (this.getModelRightArmRotation() != null) {
      compoundTag.put(DATA_MODEL_RIGHT_ARM_ROTATION_TAG, this.getModelRightArmRotation().save());
    }
    if (this.getModelLeftLegRotation() != null) {
      compoundTag.put(DATA_MODEL_LEFT_LEG_ROTATION_TAG, this.getModelLeftLegRotation().save());
    }
    if (this.getModelRightLegRotation() != null) {
      compoundTag.put(DATA_MODEL_RIGHT_LEG_ROTATION_TAG, this.getModelRightLegRotation().save());
    }
    if (this.getModelRootRotation() != null) {
      compoundTag.put(DATA_MODEL_ROOT_ROTATION_TAG, this.getModelRootRotation().save());
    }
  }

  default void readAdditionalModelData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_MODEL_POSE_TAG)) {
      String modelPose = compoundTag.getString(DATA_MODEL_POSE_TAG);
      if (modelPose != null && !modelPose.isEmpty()) {
        setModelPose(ModelPose.get(modelPose));
      }
    }
    if (compoundTag.contains(DATA_MODEL_LOCK_ROTATION_TAG)) {
      setModelLockRotation(compoundTag.getBoolean(DATA_MODEL_LOCK_ROTATION_TAG));
    }
    if (compoundTag.contains(DATA_MODEL_HEAD_ROTATION_TAG)) {
      setModelHeadRotation(new Rotations(compoundTag.getList(DATA_MODEL_HEAD_ROTATION_TAG, 5)));
    }
    if (compoundTag.contains(DATA_MODEL_BODY_ROTATION_TAG)) {
      setModelBodyRotation(new Rotations(compoundTag.getList(DATA_MODEL_BODY_ROTATION_TAG, 5)));
    }
    if (compoundTag.contains(DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
      setModelLeftArmRotation(
          new Rotations(compoundTag.getList(DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
    }
    if (compoundTag.contains(DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
      setModelRightArmRotation(
          new Rotations(compoundTag.getList(DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
    }
    if (compoundTag.contains(DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
      setModelLeftLegRotation(
          new Rotations(compoundTag.getList(DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
    }
    if (compoundTag.contains(DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
      setModelRightLegRotation(
          new Rotations(compoundTag.getList(DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
    }
    if (compoundTag.contains(DATA_MODEL_ROOT_ROTATION_TAG)) {
      setModelRootRotation(new Rotations(compoundTag.getList(DATA_MODEL_ROOT_ROTATION_TAG, 5)));
    }
  }

}
