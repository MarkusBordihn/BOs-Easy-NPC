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

package de.markusbordihn.easynpc.api.pose;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.pose.PoseManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.utils.ResourceNameNormalizer;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

public class ModelPoseAPI {
  public static final ResourceLocation VANILLA_STANDING =
      new ResourceLocation("minecraft", "standing");
  public static final ResourceLocation VANILLA_CROUCHING =
      new ResourceLocation("minecraft", "crouching");
  public static final ResourceLocation VANILLA_SLEEPING =
      new ResourceLocation("minecraft", "sleeping");
  public static final ResourceLocation VANILLA_SWIMMING =
      new ResourceLocation("minecraft", "swimming");
  private static final Map<ResourceLocation, Pose> VANILLA_POSES = createVanillaPoses();

  private ModelPoseAPI() {}

  public static boolean setPose(EasyNPC<?> npc, ResourceLocation poseId) {
    Pose vanillaPose = VANILLA_POSES.get(poseId);
    if (vanillaPose != null) {
      return setVanillaPose(npc, vanillaPose);
    }
    return PoseManager.setModelPose(npc, poseId);
  }

  public static boolean setPose(EasyNPC<?> npc, String poseName) {
    if (npc == null || poseName == null || poseName.isEmpty()) {
      return false;
    }
    SkinDataCapable<?> skinData = npc.getEasyNPCSkinData();
    if (skinData == null) {
      return false;
    }
    SkinModel skinModel = skinData.getSkinModel();
    ResourceLocation poseId =
        new ResourceLocation(
            Constants.MOD_ID,
            "pose/"
                + skinModel.name().toLowerCase(Locale.ROOT)
                + "/"
                + ResourceNameNormalizer.toResourcePath(poseName));
    return PoseManager.setModelPose(npc, poseId);
  }

  public static boolean setVanillaPose(EasyNPC<?> npc, Pose pose) {
    if (npc == null || pose == null) {
      return false;
    }
    ModelDataCapable<?> modelData = npc.getEasyNPCModelData();
    if (modelData == null) {
      return false;
    }

    modelData.setModelPose(ModelPose.VANILLA);
    modelData.setModelPoseName("");
    npc.getEntity().setPose(pose);
    modelData.setModelPartRotation(new EnumMap<>(ModelPartType.class));
    modelData.setModelPartPosition(new EnumMap<>(ModelPartType.class));
    return true;
  }

  public static void resetPose(EasyNPC<?> npc) {
    PoseManager.resetModelPose(npc);
  }

  public static String getCurrentPoseName(EasyNPC<?> npc) {
    if (npc == null) {
      return "";
    }
    ModelDataCapable<?> modelData = npc.getEasyNPCModelData();
    return modelData != null ? modelData.getModelPoseName() : "";
  }

  public static ModelPose getCurrentPoseMode(EasyNPC<?> npc) {
    if (npc == null) {
      return ModelPose.VANILLA;
    }
    ModelDataCapable<?> modelData = npc.getEasyNPCModelData();
    return modelData != null ? modelData.getModelPose() : ModelPose.VANILLA;
  }

  public static Set<ResourceLocation> getAvailablePoses(SkinModel skinModel) {
    return PoseManager.getPoseDataKeysForModel(skinModel);
  }

  public static Set<ResourceLocation> getAvailablePoses(EasyNPC<?> npc) {
    if (npc == null) {
      return Set.of();
    }
    SkinDataCapable<?> skinData = npc.getEasyNPCSkinData();
    if (skinData == null) {
      return Set.of();
    }
    return PoseManager.getPoseDataKeysForModel(skinData.getSkinModel());
  }

  public static Set<ResourceLocation> getVanillaPoseIds() {
    return VANILLA_POSES.keySet();
  }

  public static boolean isVanillaPose(ResourceLocation poseId) {
    return VANILLA_POSES.containsKey(poseId);
  }

  public static Optional<Pose> getVanillaPose(ResourceLocation poseId) {
    return Optional.ofNullable(VANILLA_POSES.get(poseId));
  }

  private static Map<ResourceLocation, Pose> createVanillaPoses() {
    Map<ResourceLocation, Pose> poses = new LinkedHashMap<>();
    poses.put(VANILLA_STANDING, Pose.STANDING);
    poses.put(VANILLA_CROUCHING, Pose.CROUCHING);
    poses.put(VANILLA_SLEEPING, Pose.SLEEPING);
    poses.put(VANILLA_SWIMMING, Pose.SWIMMING);
    return Map.copyOf(poses);
  }
}
