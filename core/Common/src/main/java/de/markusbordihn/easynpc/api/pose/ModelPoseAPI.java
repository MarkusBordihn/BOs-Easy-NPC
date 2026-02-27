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
import java.util.EnumMap;
import java.util.Locale;
import java.util.Set;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Pose;

/**
 * Public API for setting and querying NPC poses. Use this class from external mods and dialog
 * actions to control NPC poses programmatically.
 */
public class ModelPoseAPI {

  private ModelPoseAPI() {}

  /**
   * Sets a named pose on the NPC by its full ResourceLocation (e.g.
   * "easy_npc:pose/humanoid_slim/sitting").
   *
   * @return true if the pose was successfully applied
   */
  public static boolean setPose(EasyNPC<?> npc, ResourceLocation poseId) {
    return PoseManager.setModelPose(npc, poseId);
  }

  /**
   * Sets a named pose on the NPC by short name (e.g. "sitting"). Resolves the full ResourceLocation
   * against the NPC's SkinModel automatically.
   *
   * @return true if the pose was successfully applied
   */
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
        ResourceLocation.fromNamespaceAndPath(
            Constants.MOD_ID,
            "pose/"
                + skinModel.name().toLowerCase(Locale.ROOT)
                + "/"
                + poseName.replaceAll("[^a-zA-Z0-9_.-]", "").toLowerCase(Locale.ROOT));
    return PoseManager.setModelPose(npc, poseId);
  }

  /** Sets a vanilla Pose on the NPC (STANDING, CROUCHING, etc.). */
  public static void setVanillaPose(EasyNPC<?> npc, Pose pose) {
    if (npc == null || pose == null) {
      return;
    }
    ModelDataCapable<?> modelData = npc.getEasyNPCModelData();
    if (modelData != null) {
      modelData.setModelPose(ModelPose.VANILLA);
      modelData.setModelPoseName("");
      npc.getEntity().setPose(pose);

      // Clear leftover rotation/position data from previous custom poses
      modelData.setModelPartRotation(new EnumMap<>(ModelPartType.class));
      modelData.setModelPartPosition(new EnumMap<>(ModelPartType.class));
    }
  }

  /** Resets the NPC to the default standing pose with no custom model data. */
  public static void resetPose(EasyNPC<?> npc) {
    PoseManager.resetModelPose(npc);
  }

  /** Returns the current named pose ResourceLocation string, or empty if no named pose is set. */
  public static String getCurrentPoseName(EasyNPC<?> npc) {
    if (npc == null) {
      return "";
    }
    ModelDataCapable<?> modelData = npc.getEasyNPCModelData();
    return modelData != null ? modelData.getModelPoseName() : "";
  }

  /** Returns the current ModelPose mode (VANILLA, DEFAULT, or CUSTOM). */
  public static ModelPose getCurrentPoseMode(EasyNPC<?> npc) {
    if (npc == null) {
      return ModelPose.VANILLA;
    }
    ModelDataCapable<?> modelData = npc.getEasyNPCModelData();
    return modelData != null ? modelData.getModelPose() : ModelPose.VANILLA;
  }

  /** Returns all available named poses for a given skin model. */
  public static Set<ResourceLocation> getAvailablePoses(SkinModel skinModel) {
    return PoseManager.getPoseDataKeysForModel(skinModel);
  }

  /** Returns all available named poses for the NPC's skin model. */
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
}
