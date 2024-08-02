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
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.item.ItemStack;

public interface ModelData<T extends PathfinderMob>
    extends EasyNPC<T>, ModelPositionData<T>, ModelRotationData<T>, ModelVisibilityData<T> {

  CustomScale DEFAULT_MODEL_PART_SCALE = new CustomScale(1, 1, 1);
  String EASY_NPC_DATA_MODEL_DATA_TAG = "ModelData";
  String EASY_NPC_DATA_MODEL_DEFAULT_POSE_TAG = "DefaultPose";
  String EASY_NPC_DATA_MODEL_POSE_TAG = "Pose";
  String EASY_NPC_DATA_MODEL_SCALE_TAG = "Scale";
  String EASY_NPC_DATA_MODEL_SMART_ANIMATIONS_TAG = "SmartAnimations";
  EntityDataSerializer<ModelPose> MODEL_POSE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, ModelPose modelPose) {
          buffer.writeEnum(modelPose);
        }

        public ModelPose read(FriendlyByteBuf buffer) {
          return buffer.readEnum(ModelPose.class);
        }

        public ModelPose copy(ModelPose value) {
          return value;
        }
      };
  EntityDataSerializer<CustomScale> SCALE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, CustomScale scale) {
          buffer.writeFloat(scale.x());
          buffer.writeFloat(scale.y());
          buffer.writeFloat(scale.z());
        }

        public CustomScale read(FriendlyByteBuf buffer) {
          return new CustomScale(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        public CustomScale copy(CustomScale scale) {
          return scale;
        }
      };

  static void registerSyncedModelData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Model Data for {}.", entityClass.getSimpleName());
    map.put(SynchedDataIndex.MODEL_POSE, SynchedEntityData.defineId(entityClass, MODEL_POSE));
    map.put(SynchedDataIndex.MODEL_HEAD_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(SynchedDataIndex.MODEL_BODY_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(SynchedDataIndex.MODEL_ARMS_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(SynchedDataIndex.MODEL_LEFT_ARM_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(SynchedDataIndex.MODEL_RIGHT_ARM_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(SynchedDataIndex.MODEL_LEFT_LEG_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(SynchedDataIndex.MODEL_RIGHT_LEG_SCALE, SynchedEntityData.defineId(entityClass, SCALE));
    map.put(
        SynchedDataIndex.ITEM_SMART_ANIMATIONS,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.MODEL_SMART_ANIMATIONS,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));

    ModelPositionData.registerSyncedModelPositionData(map, entityClass);
    ModelRotationData.registerSyncedModelRotationData(map, entityClass);
    ModelVisibilityData.registerSyncedModelVisibilityData(map, entityClass);
  }

  static void registerModelDataSerializer() {
    ModelPositionData.registerModelPositionDataSerializer();
    ModelRotationData.registerModelRotationDataSerializer();
    EntityDataSerializers.registerSerializer(MODEL_POSE);
    EntityDataSerializers.registerSerializer(SCALE);
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

  default CustomScale getModelHeadScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_HEAD_SCALE);
  }

  default void setModelHeadScale(CustomScale modelHeadScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_HEAD_SCALE, modelHeadScale);
  }

  default CustomScale getModelBodyScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_BODY_SCALE);
  }

  default void setModelBodyScale(CustomScale modelBodyScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_BODY_SCALE, modelBodyScale);
  }

  default CustomScale getModelArmsScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_ARMS_SCALE);
  }

  default void setModelArmsScale(CustomScale modelArmsScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_ARMS_SCALE, modelArmsScale);
  }

  default CustomScale getModelLeftArmScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_SCALE);
  }

  default void setModelLeftArmScale(CustomScale modelLeftArmScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_SCALE, modelLeftArmScale);
  }

  default CustomScale getModelRightArmScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_SCALE);
  }

  default void setModelRightArmScale(CustomScale modelRightArmScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_SCALE, modelRightArmScale);
  }

  default CustomScale getModelLeftLegScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_SCALE);
  }

  default void setModelLeftLegScale(CustomScale modelLeftLegScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_SCALE, modelLeftLegScale);
  }

  default CustomScale getModelRightLegScale() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_SCALE);
  }

  default void setModelRightLegScale(CustomScale modelRightLegScale) {
    setSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_SCALE, modelRightLegScale);
  }

  default boolean useSmartAnimations() {
    return supportsSmartAnimations()
        && getModelSupportsSmartAnimations()
        && getItemSupportsSmartAnimations();
  }

  default boolean getModelSupportsSmartAnimations() {
    return getSynchedEntityData(SynchedDataIndex.MODEL_SMART_ANIMATIONS) != null
        && getSynchedEntityData(SynchedDataIndex.MODEL_SMART_ANIMATIONS).equals(true);
  }

  default void setModelSupportsSmartAnimations(boolean useSmartAnimations) {
    setSynchedEntityData(SynchedDataIndex.MODEL_SMART_ANIMATIONS, useSmartAnimations);
  }

  default boolean getItemSupportsSmartAnimations() {
    return getSynchedEntityData(SynchedDataIndex.ITEM_SMART_ANIMATIONS) != null
        && getSynchedEntityData(SynchedDataIndex.ITEM_SMART_ANIMATIONS).equals(true);
  }

  default void setItemSupportsSmartAnimations(boolean useItemSmartAnimations) {
    setSynchedEntityData(SynchedDataIndex.ITEM_SMART_ANIMATIONS, useItemSmartAnimations);
  }

  default boolean supportsSmartAnimations() {
    return true;
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
    AttackData<?> attackData = this.getEasyNPCAttackData();
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

  @Override
  default boolean hasHeadModelPart() {
    return true;
  }

  @Override
  default boolean hasBodyModelPart() {
    return true;
  }

  @Override
  default boolean hasArmsModelPart() {
    return false;
  }

  @Override
  default boolean hasLeftArmModelPart() {
    return true;
  }

  @Override
  default boolean hasRightArmModelPart() {
    return true;
  }

  @Override
  default boolean hasLeftLegModelPart() {
    return true;
  }

  @Override
  default boolean hasRightLegModelPart() {
    return true;
  }

  @Override
  default boolean canUseArmor() {
    return true;
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

  default boolean hasChangedModelScale() {
    return (hasHeadModelPart() && getModelHeadScale().hasChanged())
        || (hasBodyModelPart() && getModelBodyScale().hasChanged())
        || (hasArmsModelPart() && getModelArmsScale().hasChanged())
        || (hasLeftArmModelPart() && getModelLeftArmScale().hasChanged())
        || (hasRightArmModelPart() && getModelRightArmScale().hasChanged())
        || (hasLeftLegModelPart() && getModelLeftLegScale().hasChanged())
        || (hasRightLegModelPart() && getModelRightLegScale().hasChanged());
  }

  default void defineSynchedModelData() {
    // General
    defineSynchedEntityData(SynchedDataIndex.MODEL_POSE, ModelPose.DEFAULT);
    defineSynchedEntityData(SynchedDataIndex.MODEL_SMART_ANIMATIONS, true);
    defineSynchedEntityData(SynchedDataIndex.ITEM_SMART_ANIMATIONS, true);

    // Model Position Data
    defineSynchedModelPositionData();

    // Rotation
    defineSynchedModelRotationData();

    // Scale
    defineSynchedEntityData(SynchedDataIndex.MODEL_HEAD_SCALE, new CustomScale(1, 1, 1));
    defineSynchedEntityData(SynchedDataIndex.MODEL_BODY_SCALE, new CustomScale(1, 1, 1));
    defineSynchedEntityData(SynchedDataIndex.MODEL_ARMS_SCALE, new CustomScale(1, 1, 1));
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEFT_ARM_SCALE, new CustomScale(1, 1, 1));
    defineSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_ARM_SCALE, new CustomScale(1, 1, 1));
    defineSynchedEntityData(SynchedDataIndex.MODEL_LEFT_LEG_SCALE, new CustomScale(1, 1, 1));
    defineSynchedEntityData(SynchedDataIndex.MODEL_RIGHT_LEG_SCALE, new CustomScale(1, 1, 1));

    // Visibility
    defineSynchedModelVisibilityData();
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

    // Model Visibility
    this.addAdditionalModelVisibilityData(modelDataTag);

    // Smart Animations
    modelDataTag.putBoolean(EASY_NPC_DATA_MODEL_SMART_ANIMATIONS_TAG, this.useSmartAnimations());

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

    // Model Visibility
    this.readAdditionalModelVisibilityData(modelDataTag);

    // Smart Animations
    if (modelDataTag.contains(EASY_NPC_DATA_MODEL_SMART_ANIMATIONS_TAG)) {
      this.setModelSupportsSmartAnimations(
          modelDataTag.getBoolean(EASY_NPC_DATA_MODEL_SMART_ANIMATIONS_TAG));
    }
  }
}
