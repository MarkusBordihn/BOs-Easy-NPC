package de.markusbordihn.easynpc.entity.data.legacy;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.core.Rotations;
import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LegacyEntityModelData {

  private static final String DATA_MODEL_VERSION_TAG = "Version";
  // CompoundTags (Legacy 1.0)
  private static final String LEGACY_1_DATA_MODEL_POSE_TAG = "ModelPose";
  private static final String LEGACY_1_DATA_MODEL_LOCK_ROTATION_TAG = "ModelLockRotation";
  private static final String LEGACY_1_DATA_MODEL_HEAD_ROTATION_TAG = "ModelHeadRotation";
  private static final String LEGACY_1_DATA_MODEL_BODY_ROTATION_TAG = "ModelBodyRotation";
  private static final String LEGACY_1_DATA_MODEL_LEFT_ARM_ROTATION_TAG = "ModelLeftArmRotation";
  private static final String LEGACY_1_DATA_MODEL_RIGHT_ARM_ROTATION_TAG = "ModelRightArmRotation";
  private static final String LEGACY_1_DATA_MODEL_LEFT_LEG_ROTATION_TAG = "ModelLeftLegRotation";
  private static final String LEGACY_1_DATA_MODEL_RIGHT_LEG_ROTATION_TAG = "ModelRightLegRotation";
  private static final String LEGACY_1_DATA_MODEL_ROOT_ROTATION_TAG = "ModelRootRotation";
  // CompoundTags (Legacy 2.0)
  private static final String LEGACY_2_DATA_MODEL_POSE_TAG = "Pose";
  private static final String LEGACY_2_DATA_MODEL_DATA_TAG = "ModelData";
  private static final String LEGACY_2_DATA_MODEL_POSITION_TAG = "Position";
  private static final String LEGACY_2_DATA_MODEL_HEAD_POSITION_TAG = "HeadPosition";
  private static final String LEGACY_2_DATA_MODEL_BODY_POSITION_TAG = "BodyPosition";
  private static final String LEGACY_2_DATA_MODEL_ARMS_POSITION_TAG = "ArmsPosition";
  private static final String LEGACY_2_DATA_MODEL_LEFT_ARM_POSITION_TAG = "LeftArmPosition";
  private static final String LEGACY_2_DATA_MODEL_RIGHT_ARM_POSITION_TAG = "RightArmPosition";
  private static final String LEGACY_2_DATA_MODEL_LEFT_LEG_POSITION_TAG = "LeftLegPosition";
  private static final String LEGACY_2_DATA_MODEL_RIGHT_LEG_POSITION_TAG = "RightLegPosition";
  private static final String LEGACY_2_DATA_MODEL_ROTATION_TAG = "Rotation";
  private static final String LEGACY_2_DATA_MODEL_LOCK_ROTATION_TAG = "LockRotation";
  private static final String LEGACY_2_DATA_MODEL_HEAD_ROTATION_TAG = "HeadRotation";
  private static final String LEGACY_2_DATA_MODEL_BODY_ROTATION_TAG = "BodyRotation";
  private static final String LEGACY_2_DATA_MODEL_ARMS_ROTATION_TAG = "ArmsRotation";
  private static final String LEGACY_2_DATA_MODEL_LEFT_ARM_ROTATION_TAG = "LeftArmRotation";
  private static final String LEGACY_2_DATA_MODEL_RIGHT_ARM_ROTATION_TAG = "RightArmRotation";
  private static final String LEGACY_2_DATA_MODEL_LEFT_LEG_ROTATION_TAG = "LeftLegRotation";
  private static final String LEGACY_2_DATA_MODEL_RIGHT_LEG_ROTATION_TAG = "RightLegRotation";
  private static final String LEGACY_2_DATA_MODEL_ROOT_ROTATION_TAG = "RootRotation";
  private static final String LEGACY_2_DATA_MODEL_VISIBLE_TAG = "Visible";
  private static final String LEGACY_2_DATA_MODEL_HEAD_VISIBLE_TAG = "HeadVisible";
  private static final String LEGACY_2_DATA_MODEL_BODY_VISIBLE_TAG = "BodyVisible";
  private static final String LEGACY_2_DATA_MODEL_ARMS_VISIBLE_TAG = "ArmsVisible";
  private static final String LEGACY_2_DATA_MODEL_LEFT_ARM_VISIBLE_TAG = "LeftArmVisible";
  private static final String LEGACY_2_DATA_MODEL_RIGHT_ARM_VISIBLE_TAG = "RightArmVisible";
  private static final String LEGACY_2_DATA_MODEL_LEFT_LEG_VISIBLE_TAG = "LeftLegVisible";
  private static final String LEGACY_2_DATA_MODEL_RIGHT_LEG_VISIBLE_TAG = "RightLegVisible";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected LegacyEntityModelData() {}

  public static boolean readAdditionalLegacyModelData(
      CompoundTag compoundTag, EasyNPCEntity entity) {

    // Version < 1.0 Conversions
    if (compoundTag.contains(LEGACY_1_DATA_MODEL_POSE_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_LOCK_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_HEAD_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_BODY_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_LEFT_ARM_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_LEFT_LEG_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)
        || compoundTag.contains(LEGACY_1_DATA_MODEL_ROOT_ROTATION_TAG)) {
      log.info("Converting legacy 1.x model data for {}", entity);
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_POSE_TAG)) {
        String modelPose = compoundTag.getString(LEGACY_1_DATA_MODEL_POSE_TAG);
        if (!modelPose.isEmpty()) {
          entity.setModelPose(ModelPose.get(modelPose));
        }
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_LOCK_ROTATION_TAG)) {
        entity.setModelLockRotation(compoundTag.getBoolean(LEGACY_1_DATA_MODEL_LOCK_ROTATION_TAG));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_HEAD_ROTATION_TAG)) {
        entity.setModelHeadRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_HEAD_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_BODY_ROTATION_TAG)) {
        entity.setModelBodyRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_BODY_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
        entity.setModelLeftArmRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
        entity.setModelRightArmRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
        entity.setModelLeftLegRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
        entity.setModelRightLegRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_ROOT_ROTATION_TAG)) {
        entity.setModelRootRotation(
            new Rotations(compoundTag.getList(LEGACY_1_DATA_MODEL_ROOT_ROTATION_TAG, 5)));
      }
      return true;
    }

    // Version < 2.0 Conversions
    if (compoundTag.contains(LEGACY_2_DATA_MODEL_DATA_TAG)) {
      CompoundTag modelDataTag = compoundTag.getCompound(LEGACY_2_DATA_MODEL_DATA_TAG);
      if (modelDataTag.contains(DATA_MODEL_VERSION_TAG)) {
        return false;
      }
      log.info("Converting legacy 2.x model data for {}", entity);

      // Model Pose
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_POSE_TAG)) {
        String modelPose = modelDataTag.getString(LEGACY_2_DATA_MODEL_POSE_TAG);
        if (!modelPose.isEmpty()) {
          entity.setModelPose(ModelPose.get(modelPose));
        }
      }

      // Model Position
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_POSITION_TAG)) {
        CompoundTag positionTag = modelDataTag.getCompound(LEGACY_2_DATA_MODEL_POSITION_TAG);
        if (positionTag.contains(LEGACY_2_DATA_MODEL_HEAD_POSITION_TAG)) {
          entity.setModelHeadPosition(
              new CustomPosition(positionTag.getList(LEGACY_2_DATA_MODEL_HEAD_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_BODY_POSITION_TAG)) {
          entity.setModelBodyPosition(
              new CustomPosition(positionTag.getList(LEGACY_2_DATA_MODEL_BODY_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_ARMS_POSITION_TAG)) {
          entity.setModelArmsPosition(
              new CustomPosition(positionTag.getList(LEGACY_2_DATA_MODEL_ARMS_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_LEFT_ARM_POSITION_TAG)) {
          entity.setModelLeftArmPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_LEFT_ARM_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_RIGHT_ARM_POSITION_TAG)) {
          entity.setModelRightArmPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_RIGHT_ARM_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_LEFT_LEG_POSITION_TAG)) {
          entity.setModelLeftLegPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_LEFT_LEG_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_RIGHT_LEG_POSITION_TAG)) {
          entity.setModelRightLegPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_RIGHT_LEG_POSITION_TAG, 5)));
        }
      }

      // Model Rotations
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_ROTATION_TAG)) {
        CompoundTag rotationsTag = modelDataTag.getCompound(LEGACY_2_DATA_MODEL_ROTATION_TAG);
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_LOCK_ROTATION_TAG)) {
          entity.setModelLockRotation(
              rotationsTag.getBoolean(LEGACY_2_DATA_MODEL_LOCK_ROTATION_TAG));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_HEAD_ROTATION_TAG)) {
          entity.setModelHeadRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_HEAD_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_BODY_ROTATION_TAG)) {
          entity.setModelBodyRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_BODY_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_ARMS_ROTATION_TAG)) {
          entity.setModelArmsRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_ARMS_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
          entity.setModelLeftArmRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
          entity.setModelRightArmRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
          entity.setModelLeftLegRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
          entity.setModelRightLegRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_ROOT_ROTATION_TAG)) {
          entity.setModelRootRotation(
              new Rotations(rotationsTag.getList(LEGACY_2_DATA_MODEL_ROOT_ROTATION_TAG, 5)));
        }
      }

      // Model Visibility
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_VISIBLE_TAG)) {
        CompoundTag visibilityTag = modelDataTag.getCompound(LEGACY_2_DATA_MODEL_VISIBLE_TAG);
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_HEAD_VISIBLE_TAG)) {
          entity.setModelHeadVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_HEAD_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_BODY_VISIBLE_TAG)) {
          entity.setModelBodyVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_BODY_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_ARMS_VISIBLE_TAG)) {
          entity.setModelArmsVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_ARMS_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_LEFT_ARM_VISIBLE_TAG)) {
          entity.setModelLeftArmVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_LEFT_ARM_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_RIGHT_ARM_VISIBLE_TAG)) {
          entity.setModelRightArmVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_RIGHT_ARM_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_LEFT_LEG_VISIBLE_TAG)) {
          entity.setModelLeftLegVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_LEFT_LEG_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_RIGHT_LEG_VISIBLE_TAG)) {
          entity.setModelRightLegVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_RIGHT_LEG_VISIBLE_TAG));
        }
      }
      return true;
    }
    return false;
  }
}
