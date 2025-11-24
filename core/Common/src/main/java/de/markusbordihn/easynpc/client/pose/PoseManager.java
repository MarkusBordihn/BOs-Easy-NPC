/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.easynpc.client.pose;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.animation.AnimationData;
import de.markusbordihn.easynpc.data.animation.AnimationData.Animation;
import de.markusbordihn.easynpc.data.animation.AnimationData.Bone;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PoseManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String TEXTURE_PREFIX = "pose/";
  private static final String LOG_PREFIX = "[Pose Manager]";

  private static final Map<ResourceLocation, Animation> poseDataMap = new HashMap<>();

  private PoseManager() {}

  public static void registerPoseData(SkinModel skinModel, AnimationData animationData) {
    if (skinModel == null || animationData == null) {
      log.error("{} Pose data {} is invalid!", LOG_PREFIX, skinModel);
      return;
    }

    // Register valid pose data
    for (Animation animation : animationData.getAnimations().values()) {
      if (animation.getBones() == null || animation.getBones().isEmpty()) {
        log.warn(
            "{} pose data {} with name {} has no bones!",
            LOG_PREFIX,
            skinModel,
            animation.getName());
        continue;
      }
      registerPoseData(getResourceLocation(skinModel, animation), animation);
    }
  }

  public static ResourceLocation getResourceLocation(SkinModel skinModel, Animation animation) {
    try {
      String resourcePath =
          TEXTURE_PREFIX
              + skinModel.name().toLowerCase(Locale.ROOT)
              + "/"
              + animation.getName().replaceAll("[^a-zA-Z0-9_.-]", "").toLowerCase(Locale.ROOT);
      return ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, resourcePath);
    } catch (Exception exception) {
      log.error(
          "{} Could not create resource location for {} with {}",
          LOG_PREFIX,
          skinModel,
          animation,
          exception);
    }
    return null;
  }

  public static Animation getPoseData(ResourceLocation resourceLocation) {
    if (resourceLocation == null) {
      return null;
    }
    return poseDataMap.get(resourceLocation);
  }

  public static Set<ResourceLocation> getPoseDataKeys() {
    return poseDataMap.keySet();
  }

  private static void registerPoseData(ResourceLocation resourceLocation, Animation animation) {
    if (resourceLocation == null || animation == null) {
      log.error("{} Pose data {} is invalid!", LOG_PREFIX, resourceLocation);
      return;
    }

    if (poseDataMap.containsKey(resourceLocation)) {
      log.warn("{} Pose data {} already registered!", LOG_PREFIX, resourceLocation);
    }

    log.info("{} Registering pose data {} with {}", LOG_PREFIX, resourceLocation, animation);
    poseDataMap.put(resourceLocation, animation);
  }

  public static void resetModelPose(EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return;
    }

    // Validate Model data.
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("{} Model data is missing for Easy NPC {}!", LOG_PREFIX, easyNPC.getEntityUUID());
      return;
    }

    // Reset model pose
    easyNPC.getEntity().setPose(Pose.STANDING);
    modelData.setModelPose(ModelPose.DEFAULT);
  }

  public static boolean setModelPose(EasyNPC<?> easyNPC, Animation animation) {
    if (easyNPC == null || animation == null) {
      return false;
    }

    // Validate Animation data.
    if (animation.getBones() == null || animation.getBones().isEmpty()) {
      log.error("{} Animation data is missing for {}!", LOG_PREFIX, animation.getName());
      return false;
    }

    // Validate Model data.
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("{} Model data is missing for Easy NPC {}!", LOG_PREFIX, easyNPC.getEntityUUID());
      return false;
    }

    // Set default pose
    easyNPC.getEntity().setPose(Pose.STANDING);
    modelData.setModelPose(ModelPose.CUSTOM);

    // Iterate over all bones and set the pose
    for (String boneName : animation.getBones().keySet()) {
      Bone bone = animation.getBones().get(boneName);
      ModelPartType modelPartType = ModelPartType.get(boneName);
      if (modelPartType == ModelPartType.UNKNOWN) {
        log.error("{} Bone {} is not supported!", LOG_PREFIX, boneName);
        continue;
      }

      List<Float> position = bone.getPosition();
      CustomPosition customPosition =
          position == null || position.size() < 3
              ? new CustomPosition(0, 0, 0)
              : new CustomPosition(position.get(0), position.get(1) * -1, position.get(2));

      List<Float> rotation = bone.getRotation();
      CustomRotation customRotation =
          rotation == null || rotation.size() < 3
              ? new CustomRotation(0, 0, 0)
              : new CustomRotation(
                  rotation.get(0) * (float) Math.PI / 180.0f,
                  rotation.get(1) * (float) Math.PI / 180.0f,
                  rotation.get(2) * (float) Math.PI / 180.0f);

      modelData.setModelPartPosition(modelPartType, customPosition);
      modelData.setModelPartRotation(modelPartType, customRotation);

      log.debug("{} Set {} to {} / {}", LOG_PREFIX, modelPartType, customPosition, customRotation);
    }

    return true;
  }
}
