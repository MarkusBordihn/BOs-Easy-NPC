/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.gametest.ServerMessageAssertions.SurvivalOwnerAccess;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelAnimationDataMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelEquipmentVisibilityMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelPoseMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelPositionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelRotationMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelScaleMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelVisibilityMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeNamedPoseMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangePoseMessage;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelAnimationData;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Pose;

public final class ModelMessageTestHelper {

  private static final CustomPosition CHANGED_HEAD_POSITION = new CustomPosition(1.0f, 2.0f, 3.0f);
  private static final CustomRotation CHANGED_ROOT_ROTATION =
      new CustomRotation(0.0f, 90.0f, 0.0f, true);
  private static final CustomRotation CHANGED_HEAD_ROTATION = new CustomRotation(0.5f, 0.25f, 0.0f);
  private static final CustomScale CHANGED_ROOT_SCALE = new CustomScale(1.5f);
  private static final CustomScale CHANGED_HEAD_SCALE = new CustomScale(1.5f, 0.5f, 1.5f);
  private static final String SITTING_POSE_ID = "easy_npc:pose/humanoid/sitting";

  private ModelMessageTestHelper() {}

  public static void assertModelPoseChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelPoseMessage(uuid, ModelPose.CUSTOM),
        ChangeModelPoseMessage::create,
        easyNPC -> modelData(easyNPC).getModelPose() == ModelPose.CUSTOM,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelPartPositionChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelPositionMessage(uuid, ModelPartType.HEAD, CHANGED_HEAD_POSITION),
        ChangeModelPositionMessage::create,
        easyNPC ->
            CHANGED_HEAD_POSITION.equals(
                    modelData(easyNPC).getModelPartPosition(ModelPartType.HEAD))
                && modelData(easyNPC).getModelPose() == ModelPose.CUSTOM,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelRootRotationChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelRotationMessage(uuid, ModelPartType.ROOT, CHANGED_ROOT_ROTATION),
        ChangeModelRotationMessage::create,
        easyNPC -> CHANGED_ROOT_ROTATION.equals(modelData(easyNPC).getModelRootData().rotation()),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelPartRotationChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelRotationMessage(uuid, ModelPartType.HEAD, CHANGED_HEAD_ROTATION),
        ChangeModelRotationMessage::create,
        easyNPC ->
            CHANGED_HEAD_ROTATION.equals(
                    modelData(easyNPC).getModelPartRotation(ModelPartType.HEAD))
                && modelData(easyNPC).getModelPose() == ModelPose.CUSTOM,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelRootScaleChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelScaleMessage(uuid, ModelPartType.ROOT, CHANGED_ROOT_SCALE),
        ChangeModelScaleMessage::create,
        easyNPC -> CHANGED_ROOT_SCALE.equals(modelData(easyNPC).getModelRootData().scale()),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelPartScaleChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelScaleMessage(uuid, ModelPartType.HEAD, CHANGED_HEAD_SCALE),
        ChangeModelScaleMessage::create,
        easyNPC ->
            CHANGED_HEAD_SCALE.equals(modelData(easyNPC).getModelPartScale(ModelPartType.HEAD))
                && modelData(easyNPC).getModelPose() == ModelPose.CUSTOM,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelPartVisibilityChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelVisibilityMessage(uuid, ModelPartType.HEAD, false),
        ChangeModelVisibilityMessage::create,
        easyNPC ->
            !modelData(easyNPC).getModelPartVisibility(ModelPartType.HEAD)
                && modelData(easyNPC).getModelPose() == ModelPose.CUSTOM,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertModelAnimationBehaviorChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeModelAnimationDataMessage(
                uuid, new ModelAnimationData(ModelAnimationBehavior.NONE)),
        ChangeModelAnimationDataMessage::create,
        easyNPC -> modelData(easyNPC).getModelAnimationBehavior() == ModelAnimationBehavior.NONE,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertEquipmentVisibilityChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeModelEquipmentVisibilityMessage(uuid, EquipmentSlot.HEAD, false),
        ChangeModelEquipmentVisibilityMessage::create,
        easyNPC -> !modelData(easyNPC).getModelPartVisibility(EquipmentSlot.HEAD),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertPoseChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangePoseMessage(uuid, Pose.CROUCHING),
        ChangePoseMessage::create,
        easyNPC ->
            easyNPC.getEntity().getPose() == Pose.CROUCHING
                && modelData(easyNPC).getModelPose() == ModelPose.VANILLA,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertNamedPoseChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeNamedPoseMessage(uuid, SITTING_POSE_ID),
        ChangeNamedPoseMessage::create,
        easyNPC ->
            SITTING_POSE_ID.equals(modelData(easyNPC).getModelPoseName())
                && modelData(easyNPC).getModelPose() == ModelPose.DEFAULT,
        SurvivalOwnerAccess.GRANTED);
  }

  private static ModelDataCapable<?> modelData(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCModelData();
  }
}
