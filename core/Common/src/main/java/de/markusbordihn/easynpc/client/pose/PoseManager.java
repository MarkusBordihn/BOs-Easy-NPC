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
import java.util.EnumMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PoseManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String TEXTURE_PREFIX = "pose/";
  private static final String LOG_PREFIX = "[Pose Manager]";

  private static final Map<ResourceLocation, Animation> poseDataMap = new ConcurrentHashMap<>();
  private static final Map<ResourceLocation, EnumMap<ModelPartType, CustomRotation>>
      cachedRotations = new ConcurrentHashMap<>();
  private static final Map<ResourceLocation, EnumMap<ModelPartType, CustomPosition>>
      cachedPositions = new ConcurrentHashMap<>();

  private PoseManager() {}

  public static void clearPoseData() {
    poseDataMap.clear();
    cachedRotations.clear();
    cachedPositions.clear();
    log.info("{} Cleared all pose data.", LOG_PREFIX);
  }

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

  public static Set<ResourceLocation> getPoseDataKeysForModel(SkinModel skinModel) {
    if (skinModel == null) {
      return Set.of();
    }

    // Collect own poses
    String prefix = TEXTURE_PREFIX + skinModel.name().toLowerCase(Locale.ROOT) + "/";
    Map<String, ResourceLocation> posesByName = new java.util.LinkedHashMap<>();
    for (ResourceLocation rl : poseDataMap.keySet()) {
      if (rl.getPath().startsWith(prefix)) {
        String poseName = rl.getPath().substring(prefix.length());
        posesByName.put(poseName, rl);
      }
    }

    // Inherit poses from parent model (if available)
    SkinModel parentModel = skinModel.getParentSkinModel();
    if (parentModel != null) {
      String parentPrefix = TEXTURE_PREFIX + parentModel.name().toLowerCase(Locale.ROOT) + "/";
      Set<String> excluded = SkinModel.getExcludedFromInheritance();
      for (ResourceLocation rl : poseDataMap.keySet()) {
        if (rl.getPath().startsWith(parentPrefix)) {
          String poseName = rl.getPath().substring(parentPrefix.length());
          if (!posesByName.containsKey(poseName) && !excluded.contains(poseName)) {
            posesByName.put(poseName, rl);
          }
        }
      }
    }

    // Sort: "standing" always first, rest alphabetically
    return posesByName.values().stream()
        .sorted(
            (a, b) -> {
              boolean aStanding = a.getPath().endsWith("/standing");
              boolean bStanding = b.getPath().endsWith("/standing");
              if (aStanding && !bStanding) return -1;
              if (!aStanding && bStanding) return 1;
              return a.getPath().compareTo(b.getPath());
            })
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }

  public static String getPoseDisplayName(ResourceLocation resourceLocation) {
    if (resourceLocation == null) {
      return "";
    }
    String path = resourceLocation.getPath();
    int lastSlash = path.lastIndexOf('/');
    String name = lastSlash >= 0 ? path.substring(lastSlash + 1) : path;
    if (name.isEmpty()) {
      return "";
    }
    return name.substring(0, 1).toUpperCase(Locale.ROOT) + name.substring(1);
  }

  private static void registerPoseData(ResourceLocation resourceLocation, Animation animation) {
    if (resourceLocation == null || animation == null) {
      log.error("{} Pose data {} is invalid!", LOG_PREFIX, resourceLocation);
      return;
    }

    if (poseDataMap.containsKey(resourceLocation)) {
      log.warn("{} Pose data {} already registered!", LOG_PREFIX, resourceLocation);
    }

    log.info("{} Registering pose data {}", LOG_PREFIX, resourceLocation);
    poseDataMap.put(resourceLocation, animation);
    cachedRotations.remove(resourceLocation);
    cachedPositions.remove(resourceLocation);
  }

  public static void resetModelPose(EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("{} Model data is missing for Easy NPC {}!", LOG_PREFIX, easyNPC.getEntityUUID());
      return;
    }

    easyNPC.getEntity().setPose(Pose.STANDING);
    modelData.setModelPose(ModelPose.VANILLA);
    modelData.setModelPoseName("");

    // Clear all pose data to prevent leftover rotations/positions
    modelData.setModelPartRotation(new EnumMap<>(ModelPartType.class));
    modelData.setModelPartPosition(new EnumMap<>(ModelPartType.class));
  }

  public static boolean setModelPose(EasyNPC<?> easyNPC, ResourceLocation resourceLocation) {
    if (easyNPC == null || resourceLocation == null) {
      return false;
    }

    Animation animation = getPoseData(resourceLocation);
    if (animation == null) {
      log.error("{} Pose data {} was not found!", LOG_PREFIX, resourceLocation);
      return false;
    }

    // Try cached version first for better performance
    if (cachedRotations.containsKey(resourceLocation)) {
      if (setModelPoseFromCache(easyNPC, resourceLocation)) {
        ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
        if (modelData != null) {
          modelData.setModelPoseName(resourceLocation.toString());
        }
        return true;
      }
    }

    if (setModelPose(easyNPC, animation)) {
      ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData != null) {
        modelData.setModelPoseName(resourceLocation.toString());
      }
      // Build cache for subsequent uses
      buildCache(resourceLocation, animation);
      return true;
    }
    return false;
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
    modelData.setModelPose(ModelPose.DEFAULT);

    // Convert bones to rotation/position maps and apply atomically
    EnumMap<ModelPartType, CustomRotation> newRotations = new EnumMap<>(ModelPartType.class);
    EnumMap<ModelPartType, CustomPosition> newPositions = new EnumMap<>(ModelPartType.class);
    convertBonesToMaps(animation, newRotations, newPositions);

    // Apply fresh maps atomically, replacing any leftover data from previous poses
    modelData.setModelPartRotation(newRotations);
    modelData.setModelPartPosition(newPositions);

    return true;
  }

  public static boolean setModelPoseFromCache(
      EasyNPC<?> easyNPC, ResourceLocation resourceLocation) {
    if (easyNPC == null || resourceLocation == null) {
      return false;
    }

    EnumMap<ModelPartType, CustomRotation> rotations = cachedRotations.get(resourceLocation);
    EnumMap<ModelPartType, CustomPosition> positions = cachedPositions.get(resourceLocation);
    if (rotations == null || positions == null) {
      return false;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      return false;
    }

    easyNPC.getEntity().setPose(Pose.STANDING);
    modelData.setModelPose(ModelPose.DEFAULT);

    // Build fresh maps from cache to replace all previous pose data atomically
    EnumMap<ModelPartType, CustomRotation> newRotations = new EnumMap<>(rotations);
    EnumMap<ModelPartType, CustomPosition> newPositions = new EnumMap<>(positions);

    // Apply fresh maps, replacing any leftover data from previous poses
    modelData.setModelPartRotation(newRotations);
    modelData.setModelPartPosition(newPositions);

    return true;
  }

  private static void buildCache(ResourceLocation resourceLocation, Animation animation) {
    EnumMap<ModelPartType, CustomRotation> rotations = new EnumMap<>(ModelPartType.class);
    EnumMap<ModelPartType, CustomPosition> positions = new EnumMap<>(ModelPartType.class);
    convertBonesToMaps(animation, rotations, positions);
    cachedRotations.put(resourceLocation, rotations);
    cachedPositions.put(resourceLocation, positions);
    log.debug("{} Built cache for pose {}", LOG_PREFIX, resourceLocation);
  }

  private static void convertBonesToMaps(
      Animation animation,
      EnumMap<ModelPartType, CustomRotation> rotations,
      EnumMap<ModelPartType, CustomPosition> positions) {
    for (Map.Entry<String, Bone> entry : animation.getBones().entrySet()) {
      ModelPartType modelPartType = ModelPartType.get(entry.getKey());
      if (modelPartType == ModelPartType.UNKNOWN) {
        log.error("{} Bone {} is not supported!", LOG_PREFIX, entry.getKey());
        continue;
      }

      Bone bone = entry.getValue();
      List<Float> position = bone.getPosition();
      positions.put(
          modelPartType,
          position == null || position.size() < 3
              ? new CustomPosition(0, 0, 0)
              : new CustomPosition(position.get(0), position.get(1) * -1, position.get(2)));

      List<Float> rotation = bone.getRotation();
      rotations.put(
          modelPartType,
          rotation == null || rotation.size() < 3
              ? new CustomRotation(0, 0, 0)
              : new CustomRotation(
                  rotation.get(0) * (float) Math.PI / 180.0f,
                  rotation.get(1) * (float) Math.PI / 180.0f,
                  rotation.get(2) * (float) Math.PI / 180.0f));
    }
  }
}
