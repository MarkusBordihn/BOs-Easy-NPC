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

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;

public interface ModelDataCapable<T extends PathfinderMob>
    extends EasyNPC<T>,
        ModelAnimationDataCapable<T>,
        ModelPositionDataCapable<T>,
        ModelRotationDataCapable<T>,
        ModelScaleDataCapable<T>,
        ModelVisibilityDataCapable<T> {

  String EASY_NPC_DATA_MODEL_DATA_TAG = "ModelData";
  String EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG = "DefaultPose";
  String EASY_NPC_DATA_MODEL_POSE_TAG = "Pose";

  default Pose getDefaultPose() {
    return this.getEntity().getPose();
  }

  default void setDefaultPose(Pose pose) {
    this.getEntity().setPose(pose);
  }

  default ModelPose getModelPose() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_POSE);
  }

  default void setModelPose(ModelPose modelPose) {
    setSynchedEntityData(SynchedDataIndex.MODEL_POSE, modelPose);
  }

  default ModelType getModelType() {
    return ModelType.HUMANOID;
  }

  default boolean canUseArmor() {
    return false;
  }

  default boolean canUseMainHand() {
    return true;
  }

  default boolean canUseOffHand() {
    return true;
  }

  default boolean hasChangedModel() {
    return hasChangedModelPosition()
        || hasChangedModelRotation()
        || hasChangedModelScale()
        || hasChangedModelVisibility();
  }

  default void defineSynchedModelData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.MODEL_POSE, ModelPose.DEFAULT);
    defineSynchedModelAnimationData(builder);
    defineSynchedModelPositionData(builder);
    defineSynchedModelRotationData(builder);
    defineSynchedModelScaleData(builder);
    defineSynchedModelVisibilityData(builder);
  }

  default void addAdditionalModelData(CompoundTag compoundTag) {
    CompoundTag modelDataTag = new CompoundTag();

    if (this.getModelPose() != ModelPose.DEFAULT && this.hasChangedModel()) {
      modelDataTag.putString(EASY_NPC_DATA_MODEL_POSE_TAG, this.getModelPose().name());
      modelDataTag.putString(EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG, Pose.STANDING.name());
    } else {
      modelDataTag.putString(EASY_NPC_DATA_MODEL_POSE_TAG, ModelPose.DEFAULT.name());
      modelDataTag.putString(
          EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG,
          this.getDefaultPose() != null ? this.getDefaultPose().name() : Pose.STANDING.name());
    }

    // Add additional model data
    this.addAdditionalModelAnimationData(compoundTag);
    this.addAdditionalModelPositionData(modelDataTag);
    this.addAdditionalModelRotationData(modelDataTag);
    this.addAdditionalModelScaleData(modelDataTag);
    this.addAdditionalModelVisibilityData(modelDataTag);

    compoundTag.put(EASY_NPC_DATA_MODEL_DATA_TAG, modelDataTag);
  }

  default void readAdditionalModelData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_DATA_TAG)) {
      return;
    }

    CompoundTag modelDataTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_DATA_TAG);

    if (modelDataTag.contains(EASY_NPC_DATA_MODEL_POSE_TAG)) {
      String modelPose = modelDataTag.getString(EASY_NPC_DATA_MODEL_POSE_TAG);
      if (!modelPose.isEmpty()) {
        this.setModelPose(ModelPose.get(modelPose));
      }
    }

    if (this.getModelPose() == ModelPose.DEFAULT
        && modelDataTag.contains(EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG)) {
      String defaultPose = modelDataTag.getString(EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG);
      if (!defaultPose.isEmpty()) {
        this.setDefaultPose(Pose.valueOf(defaultPose));
      }
    }

    // Read additional model data
    this.readAdditionalModelAnimationData(compoundTag);
    this.readAdditionalModelPositionData(modelDataTag);
    this.readAdditionalModelRotationData(modelDataTag);
    this.readAdditionalModelScaleData(modelDataTag);
    this.readAdditionalModelVisibilityData(modelDataTag);
  }
}
