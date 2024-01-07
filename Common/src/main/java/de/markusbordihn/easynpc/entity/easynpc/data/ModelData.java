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
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.legacy.LegacyModelData;
import javax.annotation.Nonnull;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.CrossbowItem;

public interface ModelData<T extends LivingEntity> extends EasyNPC<T> {

  EntityDataSerializer<ModelPose> MODEL_POSE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, @Nonnull ModelPose modelPose) {
          buffer.writeEnum(modelPose);
        }

        @Nonnull
        public ModelPose read(FriendlyByteBuf buffer) {
          return buffer.readEnum(ModelPose.class);
        }

        @Nonnull
        public ModelPose copy(@Nonnull ModelPose value) {
          return value;
        }
      };
  EntityDataSerializer<CustomPosition> POSITION =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, @Nonnull CustomPosition position) {
          buffer.writeFloat(position.x());
          buffer.writeFloat(position.y());
          buffer.writeFloat(position.z());
        }

        @Nonnull
        public CustomPosition read(FriendlyByteBuf buffer) {
          return new CustomPosition(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        @Nonnull
        public CustomPosition copy(@Nonnull CustomPosition position) {
          return position;
        }
      };
  EntityDataSerializer<CustomRotation> ROTATION =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, @Nonnull CustomRotation rotation) {
          buffer.writeFloat(rotation.x());
          buffer.writeFloat(rotation.y());
          buffer.writeFloat(rotation.z());
        }

        @Nonnull
        public CustomRotation read(FriendlyByteBuf buffer) {
          return new CustomRotation(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        @Nonnull
        public CustomRotation copy(@Nonnull CustomRotation rotation) {
          return rotation;
        }
      };
  EntityDataSerializer<CustomScale> SCALE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, @Nonnull CustomScale scale) {
          buffer.writeFloat(scale.x());
          buffer.writeFloat(scale.y());
          buffer.writeFloat(scale.z());
        }

        @Nonnull
        public CustomScale read(FriendlyByteBuf buffer) {
          return new CustomScale(buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
        }

        @Nonnull
        public CustomScale copy(@Nonnull CustomScale scale) {
          return scale;
        }
      };
  EntityDataAccessor<ModelPose> EASY_NPC_DATA_MODEL_POSE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), MODEL_POSE);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_HEAD_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_BODY_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_ARMS_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_LEFT_ARM_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_RIGHT_ARM_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_LEFT_LEG_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<CustomPosition> EASY_NPC_DATA_MODEL_RIGHT_LEG_POSITION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), POSITION);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_LOCK_ROTATION =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_HEAD_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_BODY_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_ARMS_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_LEFT_ARM_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_RIGHT_ARM_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_LEFT_LEG_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_RIGHT_LEG_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);
  EntityDataAccessor<CustomRotation> EASY_NPC_DATA_MODEL_ROOT_ROTATION =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), ROTATION);

  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_HEAD_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);
  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_BODY_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);
  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_ARMS_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);
  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_LEFT_ARM_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);
  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_RIGHT_ARM_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);
  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_LEFT_LEG_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);
  EntityDataAccessor<CustomScale> EASY_NPC_DATA_MODEL_RIGHT_LEG_SCALE =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), SCALE);

  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_HEAD_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_BODY_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_ARMS_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_LEFT_ARM_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_RIGHT_ARM_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_LEFT_LEG_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_MODEL_RIGHT_LEG_VISIBLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);

  String EASY_NPC_DATA_MODEL_VERSION_TAG = "Version";
  String EASY_NPC_DATA_MODEL_DATA_TAG = "ModelData";
  String EASY_NPC_DATA_MODEL_POSE_TAG = "Pose";
  String EASY_NPC_DATA_MODEL_HEAD_TAG = "Head";
  String EASY_NPC_DATA_MODEL_BODY_TAG = "Body";
  String EASY_NPC_DATA_MODEL_ARMS_TAG = "Arms";
  String EASY_NPC_DATA_MODEL_LEFT_ARM_TAG = "LeftArm";
  String EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG = "RightArm";
  String EASY_NPC_DATA_MODEL_LEFT_LEG_TAG = "LeftLeg";
  String EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG = "RightLeg";
  String EASY_NPC_DATA_MODEL_ROOT_TAG = "Root";
  String EASY_NPC_DATA_MODEL_LOCK_TAG = "Lock";
  String EASY_NPC_DATA_MODEL_POSITION_TAG = "Position";
  String EASY_NPC_DATA_MODEL_ROTATION_TAG = "Rotation";
  String EASY_NPC_DATA_MODEL_SCALE_TAG = "Scale";
  String EASY_NPC_DATA_MODEL_VISIBLE_TAG = "Visible";
  CustomPosition DEFAULT_MODEL_PART_POSITION = new CustomPosition(0, 0, 0);
  CustomRotation DEFAULT_MODEL_PART_ROTATION = new CustomRotation(0, 0, 0);
  CustomScale DEFAULT_MODEL_PART_SCALE = new CustomScale(1, 1, 1);

  public static void registerModelDataSerializer() {
    EntityDataSerializers.registerSerializer(MODEL_POSE);
    EntityDataSerializers.registerSerializer(POSITION);
    EntityDataSerializers.registerSerializer(ROTATION);
    EntityDataSerializers.registerSerializer(SCALE);
  }

  default ModelPose getModelPose() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_POSE);
  }

  default void setModelPose(ModelPose modelPose) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_POSE, modelPose);
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
    return getEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_POSITION);
  }

  default void setModelHeadPosition(CustomPosition modelHeadPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_POSITION, modelHeadPosition);
  }

  default CustomPosition getModelBodyPosition() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_BODY_POSITION);
  }

  default void setModelBodyPosition(CustomPosition modelBodyPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_BODY_POSITION, modelBodyPosition);
  }

  default CustomPosition getModelArmsPosition() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_POSITION);
  }

  default void setModelArmsPosition(CustomPosition modelArmsPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_POSITION, modelArmsPosition);
  }

  default CustomPosition getModelLeftArmPosition() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_POSITION);
  }

  default void setModelLeftArmPosition(CustomPosition modelLeftArmPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_POSITION, modelLeftArmPosition);
  }

  default CustomPosition getModelRightArmPosition() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_POSITION);
  }

  default void setModelRightArmPosition(CustomPosition modelRightArmPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_POSITION, modelRightArmPosition);
  }

  default CustomPosition getModelLeftLegPosition() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_POSITION);
  }

  default void setModelLeftLegPosition(CustomPosition modelLeftLegPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_POSITION, modelLeftLegPosition);
  }

  default CustomPosition getModelRightLegPosition() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_POSITION);
  }

  default void setModelRightLegPosition(CustomPosition modelRightLegPosition) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_POSITION, modelRightLegPosition);
  }

  default boolean getModelLockRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LOCK_ROTATION);
  }

  default void setModelLockRotation(boolean modelLockRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LOCK_ROTATION, modelLockRotation);
  }

  default CustomRotation getModelHeadRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_ROTATION);
  }

  default void setModelHeadRotation(CustomRotation modelHeadRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_ROTATION, modelHeadRotation);
  }

  default CustomRotation getModelBodyRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_BODY_ROTATION);
  }

  default void setModelBodyRotation(CustomRotation modelBodyRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_BODY_ROTATION, modelBodyRotation);
  }

  default CustomRotation getModelArmsRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_ROTATION);
  }

  default void setModelArmsRotation(CustomRotation modelArmsRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_ROTATION, modelArmsRotation);
  }

  default CustomRotation getModelLeftArmRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_ROTATION);
  }

  default void setModelLeftArmRotation(CustomRotation modelLeftArmRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_ROTATION, modelLeftArmRotation);
  }

  default CustomRotation getModelRightArmRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_ROTATION);
  }

  default void setModelRightArmRotation(CustomRotation modelRightArmRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_ROTATION, modelRightArmRotation);
  }

  default CustomRotation getModelLeftLegRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_ROTATION);
  }

  default void setModelLeftLegRotation(CustomRotation modelLeftLegRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_ROTATION, modelLeftLegRotation);
  }

  default CustomRotation getModelRightLegRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_ROTATION);
  }

  default void setModelRightLegRotation(CustomRotation modelRightLegRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_ROTATION, modelRightLegRotation);
  }

  default CustomRotation getModelRootRotation() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_ROOT_ROTATION);
  }

  default void setModelRootRotation(CustomRotation modelRootRotation) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_ROOT_ROTATION, modelRootRotation);
  }

  default CustomScale getModelHeadScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_SCALE);
  }

  default void setModelHeadScale(CustomScale modelHeadScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_SCALE, modelHeadScale);
  }

  default CustomScale getModelBodyScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_BODY_SCALE);
  }

  default void setModelBodyScale(CustomScale modelBodyScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_BODY_SCALE, modelBodyScale);
  }

  default CustomScale getModelArmsScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_SCALE);
  }

  default void setModelArmsScale(CustomScale modelArmsScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_SCALE, modelArmsScale);
  }

  default CustomScale getModelLeftArmScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_SCALE);
  }

  default void setModelLeftArmScale(CustomScale modelLeftArmScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_SCALE, modelLeftArmScale);
  }

  default CustomScale getModelRightArmScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_SCALE);
  }

  default void setModelRightArmScale(CustomScale modelRightArmScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_SCALE, modelRightArmScale);
  }

  default CustomScale getModelLeftLegScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_SCALE);
  }

  default void setModelLeftLegScale(CustomScale modelLeftLegScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_SCALE, modelLeftLegScale);
  }

  default CustomScale getModelRightLegScale() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_SCALE);
  }

  default void setModelRightLegScale(CustomScale modelRightLegScale) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_SCALE, modelRightLegScale);
  }

  default boolean isModelHeadVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_VISIBLE);
  }

  default void setModelHeadVisible(boolean modelHeadVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_VISIBLE, modelHeadVisible);
  }

  default boolean isModelBodyVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_BODY_VISIBLE);
  }

  default void setModelBodyVisible(boolean modelBodyVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_BODY_VISIBLE, modelBodyVisible);
  }

  default boolean isModelArmsVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_VISIBLE);
  }

  default void setModelArmsVisible(boolean modelArmsVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_VISIBLE, modelArmsVisible);
  }

  default boolean isModelLeftArmVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_VISIBLE);
  }

  default void setModelLeftArmVisible(boolean modelLeftArmVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_VISIBLE, modelLeftArmVisible);
  }

  default boolean isModelRightArmVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_VISIBLE);
  }

  default void setModelRightArmVisible(boolean modelRightArmVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_VISIBLE, modelRightArmVisible);
  }

  default boolean isModelLeftLegVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_VISIBLE);
  }

  default void setModelLeftLegVisible(boolean modelLeftLegVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_VISIBLE, modelLeftLegVisible);
  }

  default boolean isModelRightLegVisible() {
    return getEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_VISIBLE);
  }

  default void setModelRightLegVisible(boolean modelRightLegVisible) {
    setEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_VISIBLE, modelRightLegVisible);
  }

  default ModelArmPose getModelArmPose(LivingEntity livingEntity) {
    boolean isAggressive = this.getEasyNPCAttackData().isAggressive();

    // Bow arm pose
    if (isAggressive && livingEntity.isHolding(is -> is.getItem() instanceof BowItem)) {
      return ModelArmPose.BOW_AND_ARROW;
    }

    // Crossbow arm pose
    if (livingEntity.isHolding(is -> is.getItem() instanceof CrossbowItem)) {
      if (this.getEasyNPCAttackData().isChargingCrossbow()) {
        return ModelArmPose.CROSSBOW_CHARGE;
      } else if (isAggressive) {
        return ModelArmPose.CROSSBOW_HOLD;
      }
    }

    // Sword arm pose
    if (isAggressive && this.getEasyNPCAttackData().isHoldingMeleeWeapon(livingEntity)) {
      return ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
    }

    return isAggressive ? ModelArmPose.ATTACKING : ModelArmPose.NEUTRAL;
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

  default boolean hasChangedModel() {
    return hasChangedModelPosition()
        || hasChangedModelRotation()
        || hasChangedModelScale()
        || hasChangedModelVisibility();
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

  default boolean hasChangedModelRotation() {
    return (hasHeadModelPart() && getModelHeadRotation().hasChanged())
        || (hasBodyModelPart() && getModelBodyRotation().hasChanged())
        || (hasArmsModelPart() && getModelArmsRotation().hasChanged())
        || (hasLeftArmModelPart() && getModelLeftArmRotation().hasChanged())
        || (hasRightArmModelPart() && getModelRightArmRotation().hasChanged())
        || (hasLeftLegModelPart() && getModelLeftLegRotation().hasChanged())
        || (hasRightLegModelPart() && getModelRightLegRotation().hasChanged());
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

  default boolean hasChangedModelVisibility() {
    return (hasHeadModelPart() && !isModelHeadVisible())
        || (hasBodyModelPart() && !isModelBodyVisible())
        || (hasArmsModelPart() && !isModelArmsVisible())
        || (hasLeftArmModelPart() && !isModelLeftArmVisible())
        || (hasRightArmModelPart() && !isModelRightArmVisible())
        || (hasLeftLegModelPart() && !isModelLeftLegVisible())
        || (hasRightLegModelPart() && !isModelRightLegVisible());
  }

  default void defineSynchedModelData() {
    defineEasyNPCData(EASY_NPC_DATA_MODEL_POSE, ModelPose.DEFAULT);

    // Position
    defineEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_POSITION, new CustomPosition(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_BODY_POSITION, new CustomPosition(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_POSITION, new CustomPosition(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_POSITION, new CustomPosition(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_POSITION, new CustomPosition(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_POSITION, new CustomPosition(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_POSITION, new CustomPosition(0, 0, 0));

    // Rotation
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LOCK_ROTATION, false);
    defineEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_BODY_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_ROTATION, new CustomRotation(0, 0, 0));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_ROOT_ROTATION, new CustomRotation(0, 0, 0));

    // Scale
    defineEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_SCALE, new CustomScale(1, 1, 1));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_BODY_SCALE, new CustomScale(1, 1, 1));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_SCALE, new CustomScale(1, 1, 1));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_SCALE, new CustomScale(1, 1, 1));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_SCALE, new CustomScale(1, 1, 1));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_SCALE, new CustomScale(1, 1, 1));
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_SCALE, new CustomScale(1, 1, 1));

    // Visibility
    defineEasyNPCData(EASY_NPC_DATA_MODEL_HEAD_VISIBLE, this.hasHeadModelPart());
    defineEasyNPCData(EASY_NPC_DATA_MODEL_BODY_VISIBLE, this.hasBodyModelPart());
    defineEasyNPCData(EASY_NPC_DATA_MODEL_ARMS_VISIBLE, this.hasArmsModelPart());
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_ARM_VISIBLE, this.hasLeftArmModelPart());
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_ARM_VISIBLE, this.hasRightArmModelPart());
    defineEasyNPCData(EASY_NPC_DATA_MODEL_LEFT_LEG_VISIBLE, this.hasLeftLegModelPart());
    defineEasyNPCData(EASY_NPC_DATA_MODEL_RIGHT_LEG_VISIBLE, this.hasRightLegModelPart());
  }

  private void addAdditionalModelPositionData(CompoundTag compoundTag) {
    CompoundTag positionsTag = new CompoundTag();
    if (hasHeadModelPart()
        && this.getModelHeadPosition() != null
        && this.getModelHeadPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_HEAD_TAG, this.getModelHeadPosition().save());
    }
    if (hasBodyModelPart()
        && this.getModelBodyPosition() != null
        && this.getModelBodyPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_BODY_TAG, this.getModelBodyPosition().save());
    }
    if (hasArmsModelPart()
        && this.getModelArmsPosition() != null
        && this.getModelArmsPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_ARMS_TAG, this.getModelArmsPosition().save());
    }
    if (hasLeftArmModelPart()
        && this.getModelLeftArmPosition() != null
        && this.getModelLeftArmPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG, this.getModelLeftArmPosition().save());
    }
    if (hasRightArmModelPart()
        && this.getModelRightArmPosition() != null
        && this.getModelRightArmPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG, this.getModelRightArmPosition().save());
    }
    if (hasLeftLegModelPart()
        && this.getModelLeftLegPosition() != null
        && this.getModelLeftLegPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG, this.getModelLeftLegPosition().save());
    }
    if (hasRightLegModelPart()
        && this.getModelRightLegPosition() != null
        && this.getModelRightLegPosition().hasChanged()) {
      positionsTag.put(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG, this.getModelRightLegPosition().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_POSITION_TAG, positionsTag);
  }

  private void addAdditionalModelRotationData(CompoundTag compoundTag) {
    CompoundTag rotationsTag = new CompoundTag();
    if (this.getModelLockRotation()) {
      rotationsTag.putBoolean(EASY_NPC_DATA_MODEL_LOCK_TAG, this.getModelLockRotation());
    }
    if (this.hasHeadModelPart()
        && this.getModelHeadRotation() != null
        && this.getModelHeadRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_HEAD_TAG, this.getModelHeadRotation().save());
    }
    if (this.hasBodyModelPart()
        && this.getModelBodyRotation() != null
        && this.getModelBodyRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_BODY_TAG, this.getModelBodyRotation().save());
    }
    if (this.hasArmsModelPart()
        && this.getModelArmsRotation() != null
        && this.getModelArmsRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_ARMS_TAG, this.getModelArmsRotation().save());
    }
    if (this.hasLeftArmModelPart()
        && this.getModelLeftArmRotation() != null
        && this.getModelLeftArmRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG, this.getModelLeftArmRotation().save());
    }
    if (this.hasRightArmModelPart()
        && this.getModelRightArmRotation() != null
        && this.getModelRightArmRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG, this.getModelRightArmRotation().save());
    }
    if (this.hasLeftLegModelPart()
        && this.getModelLeftLegRotation() != null
        && this.getModelLeftLegRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG, this.getModelLeftLegRotation().save());
    }
    if (this.hasRightLegModelPart()
        && this.getModelRightLegRotation() != null
        && this.getModelRightLegRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG, this.getModelRightLegRotation().save());
    }
    if (this.getModelRootRotation() != null && this.getModelRootRotation().hasChanged()) {
      rotationsTag.put(EASY_NPC_DATA_MODEL_ROOT_TAG, this.getModelRootRotation().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_ROTATION_TAG, rotationsTag);
  }

  private void addAdditionalModelVisibilityData(CompoundTag compoundTag) {
    CompoundTag visibilityTag = new CompoundTag();
    if (this.isModelHeadVisible() != this.hasHeadModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_HEAD_TAG, this.isModelHeadVisible());
    }
    if (this.isModelBodyVisible() != this.hasBodyModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_BODY_TAG, this.isModelBodyVisible());
    }
    if (this.isModelArmsVisible() != this.hasArmsModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_ARMS_TAG, this.isModelArmsVisible());
    }
    if (this.isModelLeftArmVisible() != this.hasLeftArmModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG, this.isModelLeftArmVisible());
    }
    if (this.isModelRightArmVisible() != this.hasRightArmModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG, this.isModelRightArmVisible());
    }
    if (this.isModelLeftLegVisible() != this.hasLeftLegModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG, this.isModelLeftLegVisible());
    }
    if (this.isModelRightLegVisible() != this.hasRightLegModelPart()) {
      visibilityTag.putBoolean(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG, this.isModelRightLegVisible());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_VISIBLE_TAG, visibilityTag);
  }

  private void readAdditionalModelPositionData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_POSITION_TAG)) {
      return;
    }
    CompoundTag positionTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_POSITION_TAG);
    if (positionTag.contains(EASY_NPC_DATA_MODEL_HEAD_TAG)) {
      setModelHeadPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_HEAD_TAG, 5)));
    }
    if (positionTag.contains(EASY_NPC_DATA_MODEL_BODY_TAG)) {
      setModelBodyPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_BODY_TAG, 5)));
    }
    if (positionTag.contains(EASY_NPC_DATA_MODEL_ARMS_TAG)) {
      setModelArmsPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_ARMS_TAG, 5)));
    }
    if (positionTag.contains(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG)) {
      setModelLeftArmPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG, 5)));
    }
    if (positionTag.contains(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG)) {
      setModelRightArmPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG, 5)));
    }
    if (positionTag.contains(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG)) {
      setModelLeftLegPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG, 5)));
    }
    if (positionTag.contains(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG)) {
      setModelRightLegPosition(
          new CustomPosition(positionTag.getList(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG, 5)));
    }
  }

  private void readAdditionalModelRotationData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_ROTATION_TAG)) {
      return;
    }
    CompoundTag rotationsTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_ROTATION_TAG);
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_LOCK_TAG)) {
      setModelLockRotation(rotationsTag.getBoolean(EASY_NPC_DATA_MODEL_LOCK_TAG));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_HEAD_TAG)) {
      setModelHeadRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_HEAD_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_BODY_TAG)) {
      setModelBodyRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_BODY_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_ARMS_TAG)) {
      setModelArmsRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_ARMS_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG)) {
      setModelLeftArmRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG)) {
      setModelRightArmRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG)) {
      setModelLeftLegRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG)) {
      setModelRightLegRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG, 5)));
    }
    if (rotationsTag.contains(EASY_NPC_DATA_MODEL_ROOT_TAG)) {
      setModelRootRotation(
          new CustomRotation(rotationsTag.getList(EASY_NPC_DATA_MODEL_ROOT_TAG, 5)));
    }
  }

  private void readAdditionalModelVisibilityData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_VISIBLE_TAG)) {
      return;
    }
    CompoundTag visibilityTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_VISIBLE_TAG);
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_HEAD_TAG)) {
      setModelHeadVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_HEAD_TAG));
    }
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_BODY_TAG)) {
      setModelBodyVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_BODY_TAG));
    }
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_ARMS_TAG)) {
      setModelArmsVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_ARMS_TAG));
    }
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG)) {
      setModelLeftArmVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_LEFT_ARM_TAG));
    }
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG)) {
      setModelRightArmVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_RIGHT_ARM_TAG));
    }
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG)) {
      setModelLeftLegVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_LEFT_LEG_TAG));
    }
    if (visibilityTag.contains(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG)) {
      setModelRightLegVisible(visibilityTag.getBoolean(EASY_NPC_DATA_MODEL_RIGHT_LEG_TAG));
    }
  }

  default void addAdditionalModelData(CompoundTag compoundTag) {
    CompoundTag modelDataTag = new CompoundTag();

    // Check Model Pose
    if (this.getModelPose() != null) {
      if (this.getModelPose() != ModelPose.DEFAULT && this.hasChangedModel()) {
        modelDataTag.putString(EASY_NPC_DATA_MODEL_POSE_TAG, this.getModelPose().name());
      } else {
        modelDataTag.putString(EASY_NPC_DATA_MODEL_POSE_TAG, ModelPose.DEFAULT.name());
      }
    }

    // Model Position
    this.addAdditionalModelPositionData(modelDataTag);

    // Model Rotation
    this.addAdditionalModelRotationData(modelDataTag);

    // Model Visibility
    this.addAdditionalModelVisibilityData(modelDataTag);

    // Version
    modelDataTag.putInt(EASY_NPC_DATA_MODEL_VERSION_TAG, 3);

    compoundTag.put(EASY_NPC_DATA_MODEL_DATA_TAG, modelDataTag);
  }

  default void readAdditionalModelData(CompoundTag compoundTag) {

    // Legacy data support
    if (LegacyModelData.readAdditionalLegacyModelData(compoundTag, this.getEasyNPCModelData())) {
      return;
    }

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
        setModelPose(ModelPose.get(modelPose));
      }
    }

    // Model Position
    this.readAdditionalModelPositionData(modelDataTag);

    // Model Rotation
    this.readAdditionalModelRotationData(modelDataTag);

    // Model Visibility
    this.readAdditionalModelVisibilityData(modelDataTag);
  }
}
