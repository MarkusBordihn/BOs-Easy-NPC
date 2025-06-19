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

import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.item.ItemStack;

public interface ModelDataCapable<T extends PathfinderMob>
    extends EasyNPC<T>,
        ModelPositionDataCapable<T>,
        ModelRotationDataCapable<T>,
        ModelScaleDataCapable<T>,
        ModelVisibilityDataCapable<T> {

  String EASY_NPC_DATA_MODEL_DATA_TAG = "ModelData";
  String EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG = "DefaultPose";
  String EASY_NPC_DATA_MODEL_POSE_TAG = "Pose";

  static void registerSyncedModelData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("Registering Synched Model Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.MODEL_POSE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.MODEL_POSE));
    ModelPositionDataCapable.registerSyncedModelPositionData(map, entityClass);
    ModelRotationDataCapable.registerSyncedModelRotationData(map, entityClass);
    ModelScaleDataCapable.registerSyncedModelScaleData(map, entityClass);
    ModelVisibilityDataCapable.registerSyncedModelVisibilityData(map, entityClass);
  }

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

  default ModelArmPose getModelArmPose() {
    return getModelArmPose(this.getLivingEntity());
  }

  default ModelArmPose getModelArmPose(LivingEntity livingEntity) {
    boolean isAggressive = livingEntity instanceof Mob mob && mob.isAggressive();
    ItemStack itemStack = livingEntity.getMainHandItem();

    // Bow arm pose
    if (isAggressive && AttackHandler.isBowWeapon(itemStack)) {
      return ModelArmPose.BOW_AND_ARROW;
    }

    // Crossbow arm pose
    AttackDataCapable<?> attackData = this.getEasyNPCAttackData();
    if (AttackHandler.isCrossbowWeapon(itemStack)) {
      if (attackData.isChargingCrossbow()) {
        return ModelArmPose.CROSSBOW_CHARGE;
      } else if (isAggressive) {
        return ModelArmPose.CROSSBOW_HOLD;
      }
    }

    // Gun arm pose
    if (isAggressive && AttackHandler.isGunWeapon(itemStack)) {
      return ModelArmPose.GUN_HOLD;
    }

    // Sword arm pose
    if (isAggressive && AttackHandler.isMeeleeWeapon(itemStack)) {
      return ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
    }

    return isAggressive ? ModelArmPose.ATTACKING : ModelArmPose.NEUTRAL;
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
    // General
    defineSynchedEntityData(builder, SynchedDataIndex.MODEL_POSE, ModelPose.DEFAULT);

    // Model Position Data
    defineSynchedModelPositionData(builder);

    // Rotation
    defineSynchedModelRotationData(builder);

    // Scale
    defineSynchedModelScaleData(builder);

    // Visibility
    defineSynchedModelVisibilityData(builder);
  }

  default void addAdditionalModelData(CompoundTag compoundTag) {
    CompoundTag modelDataTag = new CompoundTag();

    // Model Pose
    if (this.getModelPose() != ModelPose.DEFAULT && this.hasChangedModel()) {
      modelDataTag.putString(EASY_NPC_DATA_MODEL_POSE_TAG, this.getModelPose().name());
      modelDataTag.putString(EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG, Pose.STANDING.name());
    } else {
      modelDataTag.putString(EASY_NPC_DATA_MODEL_POSE_TAG, ModelPose.DEFAULT.name());
      modelDataTag.putString(
          EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG,
          this.getDefaultPose() != null ? this.getDefaultPose().name() : Pose.STANDING.name());
    }

    // Model Position
    this.addAdditionalModelPositionData(modelDataTag);

    // Model Rotation
    this.addAdditionalModelRotationData(modelDataTag);

    // Model Scale
    this.addAdditionalModelScaleData(modelDataTag);

    // Model Visibility
    this.addAdditionalModelVisibilityData(modelDataTag);

    compoundTag.put(EASY_NPC_DATA_MODEL_DATA_TAG, modelDataTag);
  }

  default void readAdditionalModelData(CompoundTag compoundTag) {

    // Early exit if no model data is available
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_DATA_TAG)) {
      return;
    }

    // Read model data
    CompoundTag modelDataTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_DATA_TAG);

    // Model Pose
    if (modelDataTag.contains(EASY_NPC_DATA_MODEL_POSE_TAG)) {
      String modelPose = modelDataTag.getString(EASY_NPC_DATA_MODEL_POSE_TAG);
      if (!modelPose.isEmpty()) {
        this.setModelPose(ModelPose.get(modelPose));
      }
    }

    // Default Pose
    if (this.getModelPose() == ModelPose.DEFAULT
        && modelDataTag.contains(EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG)) {
      String defaultPose = modelDataTag.getString(EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG);
      if (!defaultPose.isEmpty()) {
        this.setDefaultPose(Pose.valueOf(defaultPose));
      }
    }

    // Model Position
    this.readAdditionalModelPositionData(modelDataTag);

    // Model Rotation
    this.readAdditionalModelRotationData(modelDataTag);

    // Model Scale
    this.readAdditionalModelScaleData(modelDataTag);

    // Model Visibility
    this.readAdditionalModelVisibilityData(modelDataTag);
  }
}
