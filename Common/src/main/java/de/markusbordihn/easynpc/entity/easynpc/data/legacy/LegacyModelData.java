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

package de.markusbordihn.easynpc.entity.easynpc.data.legacy;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LegacyModelData {

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

  protected LegacyModelData() {
  }

  public static boolean readAdditionalLegacyModelData(
      CompoundTag compoundTag, ModelData<?> modelData) {

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
      log.info("Converting legacy 1.x model data for {}", modelData);
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_POSE_TAG)) {
        String modelPose = compoundTag.getString(LEGACY_1_DATA_MODEL_POSE_TAG);
        if (!modelPose.isEmpty()) {
          modelData.setModelPose(ModelPose.get(modelPose));
        }
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_LOCK_ROTATION_TAG)) {
        modelData.setModelLockRotation(
            compoundTag.getBoolean(LEGACY_1_DATA_MODEL_LOCK_ROTATION_TAG));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_HEAD_ROTATION_TAG)) {
        modelData.setModelHeadRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_HEAD_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_BODY_ROTATION_TAG)) {
        modelData.setModelBodyRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_BODY_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
        modelData.setModelLeftArmRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
        modelData.setModelRightArmRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
        modelData.setModelLeftLegRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
        modelData.setModelRightLegRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
      }
      if (compoundTag.contains(LEGACY_1_DATA_MODEL_ROOT_ROTATION_TAG)) {
        modelData.setModelRootRotation(
            new CustomRotation(compoundTag.getList(LEGACY_1_DATA_MODEL_ROOT_ROTATION_TAG, 5)));
      }
      return true;
    }

    // Version < 2.0 Conversions
    if (compoundTag.contains(LEGACY_2_DATA_MODEL_DATA_TAG)) {
      CompoundTag modelDataTag = compoundTag.getCompound(LEGACY_2_DATA_MODEL_DATA_TAG);
      if (modelDataTag.contains(DATA_MODEL_VERSION_TAG)) {
        return false;
      }
      log.info("Converting legacy 2.x model data for {}", modelData);

      // Model Pose
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_POSE_TAG)) {
        String modelPose = modelDataTag.getString(LEGACY_2_DATA_MODEL_POSE_TAG);
        if (!modelPose.isEmpty()) {
          modelData.setModelPose(ModelPose.get(modelPose));
        }
      }

      // Model Position
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_POSITION_TAG)) {
        CompoundTag positionTag = modelDataTag.getCompound(LEGACY_2_DATA_MODEL_POSITION_TAG);
        if (positionTag.contains(LEGACY_2_DATA_MODEL_HEAD_POSITION_TAG)) {
          modelData.setModelHeadPosition(
              new CustomPosition(positionTag.getList(LEGACY_2_DATA_MODEL_HEAD_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_BODY_POSITION_TAG)) {
          modelData.setModelBodyPosition(
              new CustomPosition(positionTag.getList(LEGACY_2_DATA_MODEL_BODY_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_ARMS_POSITION_TAG)) {
          modelData.setModelArmsPosition(
              new CustomPosition(positionTag.getList(LEGACY_2_DATA_MODEL_ARMS_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_LEFT_ARM_POSITION_TAG)) {
          modelData.setModelLeftArmPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_LEFT_ARM_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_RIGHT_ARM_POSITION_TAG)) {
          modelData.setModelRightArmPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_RIGHT_ARM_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_LEFT_LEG_POSITION_TAG)) {
          modelData.setModelLeftLegPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_LEFT_LEG_POSITION_TAG, 5)));
        }
        if (positionTag.contains(LEGACY_2_DATA_MODEL_RIGHT_LEG_POSITION_TAG)) {
          modelData.setModelRightLegPosition(
              new CustomPosition(
                  positionTag.getList(LEGACY_2_DATA_MODEL_RIGHT_LEG_POSITION_TAG, 5)));
        }
      }

      // Model Rotations
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_ROTATION_TAG)) {
        CompoundTag rotationsTag = modelDataTag.getCompound(LEGACY_2_DATA_MODEL_ROTATION_TAG);
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_LOCK_ROTATION_TAG)) {
          modelData.setModelLockRotation(
              rotationsTag.getBoolean(LEGACY_2_DATA_MODEL_LOCK_ROTATION_TAG));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_HEAD_ROTATION_TAG)) {
          modelData.setModelHeadRotation(
              new CustomRotation(rotationsTag.getList(LEGACY_2_DATA_MODEL_HEAD_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_BODY_ROTATION_TAG)) {
          modelData.setModelBodyRotation(
              new CustomRotation(rotationsTag.getList(LEGACY_2_DATA_MODEL_BODY_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_ARMS_ROTATION_TAG)) {
          modelData.setModelArmsRotation(
              new CustomRotation(rotationsTag.getList(LEGACY_2_DATA_MODEL_ARMS_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_LEFT_ARM_ROTATION_TAG)) {
          modelData.setModelLeftArmRotation(
              new CustomRotation(
                  rotationsTag.getList(LEGACY_2_DATA_MODEL_LEFT_ARM_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_RIGHT_ARM_ROTATION_TAG)) {
          modelData.setModelRightArmRotation(
              new CustomRotation(
                  rotationsTag.getList(LEGACY_2_DATA_MODEL_RIGHT_ARM_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_LEFT_LEG_ROTATION_TAG)) {
          modelData.setModelLeftLegRotation(
              new CustomRotation(
                  rotationsTag.getList(LEGACY_2_DATA_MODEL_LEFT_LEG_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_RIGHT_LEG_ROTATION_TAG)) {
          modelData.setModelRightLegRotation(
              new CustomRotation(
                  rotationsTag.getList(LEGACY_2_DATA_MODEL_RIGHT_LEG_ROTATION_TAG, 5)));
        }
        if (rotationsTag.contains(LEGACY_2_DATA_MODEL_ROOT_ROTATION_TAG)) {
          modelData.setModelRootRotation(
              new CustomRotation(rotationsTag.getList(LEGACY_2_DATA_MODEL_ROOT_ROTATION_TAG, 5)));
        }
      }

      // Model Visibility
      if (modelDataTag.contains(LEGACY_2_DATA_MODEL_VISIBLE_TAG)) {
        CompoundTag visibilityTag = modelDataTag.getCompound(LEGACY_2_DATA_MODEL_VISIBLE_TAG);
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_HEAD_VISIBLE_TAG)) {
          modelData.setModelHeadVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_HEAD_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_BODY_VISIBLE_TAG)) {
          modelData.setModelBodyVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_BODY_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_ARMS_VISIBLE_TAG)) {
          modelData.setModelArmsVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_ARMS_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_LEFT_ARM_VISIBLE_TAG)) {
          modelData.setModelLeftArmVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_LEFT_ARM_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_RIGHT_ARM_VISIBLE_TAG)) {
          modelData.setModelRightArmVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_RIGHT_ARM_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_LEFT_LEG_VISIBLE_TAG)) {
          modelData.setModelLeftLegVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_LEFT_LEG_VISIBLE_TAG));
        }
        if (visibilityTag.contains(LEGACY_2_DATA_MODEL_RIGHT_LEG_VISIBLE_TAG)) {
          modelData.setModelRightLegVisible(
              visibilityTag.getBoolean(LEGACY_2_DATA_MODEL_RIGHT_LEG_VISIBLE_TAG));
        }
      }
      return true;
    }
    return false;
  }
}
