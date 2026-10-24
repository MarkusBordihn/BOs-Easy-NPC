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

import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelPositionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelRotationMessage;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelPositionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelRotationDataCapable;
import de.markusbordihn.easynpc.gametest.GameTestHelpers;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public final class MassPosePersistenceTestHelper {

  private static final int KNOWN_POSE_COUNT = 10;
  private static final int HEAD_YAW_VARIANTS = 5;
  private static final int[] HEAD_YAW_DEGREES = {-60, -30, 0, 30, 60};
  private static final int LEFT_ARM_ROLL_VARIANTS = 10;
  private static final int LEFT_ARM_ROLL_STEP_DEGREES = 15;
  private static final int NPC_COUNT =
      KNOWN_POSE_COUNT * HEAD_YAW_VARIANTS * LEFT_ARM_ROLL_VARIANTS;
  private static final int CHANGED_POSE_INDEX_SHIFT = NPC_COUNT / 2 + 3;
  public static final int TIMEOUT_TICKS = NPC_COUNT + 400;

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 0);

  private static final List<ModelPartType> POSED_PARTS =
      List.of(
          ModelPartType.HEAD,
          ModelPartType.BODY,
          ModelPartType.RIGHT_ARM,
          ModelPartType.LEFT_ARM,
          ModelPartType.RIGHT_LEG,
          ModelPartType.LEFT_LEG);

  private MassPosePersistenceTestHelper() {}

  public static void assertDistinctPosesSurviveKillAndRevive(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer poseEditor =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "test-mass-pose-editor");
    Map<UUID, ExpectedPose> initialPoses = new LinkedHashMap<>();
    Map<UUID, ExpectedPose> changedPoses = new LinkedHashMap<>();
    List<EasyNPC<?>> killedNPCs = new ArrayList<>(NPC_COUNT);
    for (int npcIndex = 0; npcIndex < NPC_COUNT; npcIndex++) {
      ExpectedPose initialPose = createExpectedPose(npcIndex);
      ExpectedPose changedPose =
          createExpectedPose((npcIndex + CHANGED_POSE_INDEX_SHIFT) % NPC_COUNT);
      helper.runAfterDelay(
          npcIndex + 1L,
          () -> {
            EasyNPC<?> easyNPC = spawnPoseAndKill(helper, entityType, initialPose, poseEditor);
            initialPoses.put(easyNPC.getEntityUUID(), initialPose);
            changedPoses.put(easyNPC.getEntityUUID(), changedPose);
            killedNPCs.add(easyNPC);
          });
    }

    List<EasyNPC<?>> revivedNPCs = new ArrayList<>(NPC_COUNT);
    helper
        .startSequence()
        .thenWaitUntil(() -> assertAllStoredAsKilled(helper, killedNPCs))
        .thenExecute(
            () -> {
              revivedNPCs.addAll(reviveAndAssertPoses(helper, initialPoses, "first revive"));
              for (EasyNPC<?> revivedNPC : revivedNPCs) {
                ExpectedPose changedPose = changedPoses.get(revivedNPC.getEntityUUID());
                applyPose(revivedNPC, changedPose, poseEditor);
                assertPose(helper, revivedNPC, changedPose, "after changing the pose");
              }
              killAll(helper, revivedNPCs);
            })
        .thenWaitUntil(() -> assertAllStoredAsKilled(helper, revivedNPCs))
        .thenExecute(() -> reviveAndAssertPoses(helper, changedPoses, "revive after pose change"))
        .thenSucceed();
  }

  private static ExpectedPose createExpectedPose(int poseIndex) {
    KnownPose knownPose = KnownPose.values()[poseIndex % KnownPose.values().length];
    int variant = poseIndex / KnownPose.values().length;
    int headPitchDegrees = (poseIndex % 3 - 1) * 20;
    int headYawDegrees = HEAD_YAW_DEGREES[variant % HEAD_YAW_DEGREES.length];
    int leftArmRollStep = variant / HEAD_YAW_DEGREES.length - LEFT_ARM_ROLL_VARIANTS / 2;
    int leftArmRollOffsetDegrees = leftArmRollStep * LEFT_ARM_ROLL_STEP_DEGREES;

    Map<ModelPartType, CustomRotation> rotationsInDegrees =
        new EnumMap<>(knownPose.rotationsInDegrees);
    rotationsInDegrees.put(
        ModelPartType.HEAD, new CustomRotation(headPitchDegrees, headYawDegrees, 0));
    CustomRotation leftArmInDegrees =
        rotationsInDegrees.getOrDefault(
            ModelPartType.LEFT_ARM, ModelRotationDataCapable.DEFAULT_MODEL_PART_ROTATION);
    rotationsInDegrees.put(
        ModelPartType.LEFT_ARM,
        new CustomRotation(
            leftArmInDegrees.x(),
            leftArmInDegrees.y(),
            leftArmInDegrees.z() + leftArmRollOffsetDegrees));

    Map<ModelPartType, CustomRotation> rotations = new EnumMap<>(ModelPartType.class);
    rotationsInDegrees.forEach(
        (modelPartType, rotationInDegrees) ->
            rotations.put(
                modelPartType,
                new CustomRotation(
                    toWrappedRadians(rotationInDegrees.x()),
                    toWrappedRadians(rotationInDegrees.y()),
                    toWrappedRadians(rotationInDegrees.z()))));

    Map<ModelPartType, CustomPosition> positions = new EnumMap<>(ModelPartType.class);
    if (poseIndex % 4 == 0) {
      positions.put(ModelPartType.HEAD, new CustomPosition(0.0f, poseIndex % 3 + 1.0f, 0.0f));
    }
    return new ExpectedPose(rotations, positions);
  }

  private static float toWrappedRadians(float degrees) {
    return (float) Math.toRadians(Math.floorMod(Math.round(degrees) + 180, 360) - 180);
  }

  private static EasyNPC<?> spawnPoseAndKill(
      GameTestHelper helper,
      EntityType<?> entityType,
      ExpectedPose expectedPose,
      ServerPlayer poseEditor) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertTrue(
        helper,
        "Freshly spawned NPC " + easyNPC.getEntityUUID() + " already has a changed pose",
        modelData.getModelPose() == ModelPose.VANILLA && !modelData.hasChangedModel());

    applyPose(easyNPC, expectedPose, poseEditor);
    assertPose(helper, easyNPC, expectedPose, "after editing");
    assertPose(
        helper,
        NPCPersistenceAssertions.retrack(helper, easyNPC),
        expectedPose,
        "after re-tracking");

    easyNPC.getLivingEntity().kill(helper.getLevel());
    return easyNPC;
  }

  private static void applyPose(
      EasyNPC<?> easyNPC, ExpectedPose expectedPose, ServerPlayer poseEditor) {
    for (ModelPartType modelPartType : POSED_PARTS) {
      CustomRotation rotation =
          expectedPose
              .rotations()
              .getOrDefault(modelPartType, ModelRotationDataCapable.DEFAULT_MODEL_PART_ROTATION);
      ServerMessageAssertions.receive(
          uuid -> new ChangeModelRotationMessage(uuid, modelPartType, rotation),
          ChangeModelRotationMessage::create,
          easyNPC,
          poseEditor);
    }
    for (ModelPartType modelPartType : POSED_PARTS) {
      CustomPosition position =
          expectedPose
              .positions()
              .getOrDefault(modelPartType, ModelPositionDataCapable.DEFAULT_MODEL_PART_POSITION);
      ServerMessageAssertions.receive(
          uuid -> new ChangeModelPositionMessage(uuid, modelPartType, position),
          ChangeModelPositionMessage::create,
          easyNPC,
          poseEditor);
    }
  }

  private static void killAll(GameTestHelper helper, List<EasyNPC<?>> easyNPCs) {
    for (EasyNPC<?> easyNPC : easyNPCs) {
      easyNPC.getLivingEntity().kill(helper.getLevel());
    }
  }

  private static void assertAllStoredAsKilled(GameTestHelper helper, List<EasyNPC<?>> easyNPCs) {
    GameTestHelpers.assertEquals(helper, "Number of killed NPCs", NPC_COUNT, easyNPCs.size());
    for (EasyNPC<?> easyNPC : easyNPCs) {
      UUID entityUUID = easyNPC.getEntityUUID();
      GameTestHelpers.assertTrue(
          helper,
          "Killed NPC " + entityUUID + " is still in the world",
          easyNPC.getEntity().isRemoved());
      GameTestHelpers.assertEquals(
          helper,
          "Stored removal reason of NPC " + entityUUID,
          NPCRemovalReason.KILLED,
          NPCEntityManager.getMetadata(entityUUID)
              .map(NPCEntityMetadata::removalReason)
              .orElse(NPCRemovalReason.NONE));
    }
  }

  private static List<EasyNPC<?>> reviveAndAssertPoses(
      GameTestHelper helper, Map<UUID, ExpectedPose> expectedPoses, String stage) {
    List<EasyNPC<?>> revivedNPCs = new ArrayList<>(expectedPoses.size());
    for (UUID entityUUID : expectedPoses.keySet()) {
      GameTestHelpers.assertTrue(
          helper,
          "NPC " + entityUUID + " was not revived on " + stage,
          EasyNPCEntityHandler.revive(entityUUID, helper.getLevel()));
      EasyNPC<?> revivedNPC =
          LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
      GameTestHelpers.assertNotNull(
          helper, "Revived NPC " + entityUUID + " is missing after " + stage, revivedNPC);
      revivedNPCs.add(revivedNPC);
    }

    long distinctPoseCount =
        revivedNPCs.stream()
            .map(revivedNPC -> revivedNPC.getEasyNPCModelData().getModelPartRotation())
            .distinct()
            .count();
    GameTestHelpers.assertEquals(
        helper,
        "Distinct poses of " + revivedNPCs.size() + " NPCs after " + stage,
        (long) revivedNPCs.size(),
        distinctPoseCount);

    for (EasyNPC<?> revivedNPC : revivedNPCs) {
      ExpectedPose expectedPose = expectedPoses.get(revivedNPC.getEntityUUID());
      assertPose(helper, revivedNPC, expectedPose, "after " + stage);
      assertPose(
          helper,
          NPCPersistenceAssertions.retrack(helper, revivedNPC),
          expectedPose,
          "after " + stage + " and re-tracking");
    }
    return revivedNPCs;
  }

  private static void assertPose(
      GameTestHelper helper, EasyNPC<?> easyNPC, ExpectedPose expectedPose, String stage) {
    String npcName = "NPC " + easyNPC.getEntityUUID() + " " + stage;
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertEquals(
        helper, "Model pose of " + npcName, ModelPose.CUSTOM, modelData.getModelPose());
    for (ModelPartType modelPartType : POSED_PARTS) {
      GameTestHelpers.assertEquals(
          helper,
          modelPartType + " rotation of " + npcName,
          expectedPose
              .rotations()
              .getOrDefault(modelPartType, ModelRotationDataCapable.DEFAULT_MODEL_PART_ROTATION),
          modelData.getModelPartRotation(modelPartType));
      GameTestHelpers.assertEquals(
          helper,
          modelPartType + " position of " + npcName,
          expectedPose
              .positions()
              .getOrDefault(modelPartType, ModelPositionDataCapable.DEFAULT_MODEL_PART_POSITION),
          modelData.getModelPartPosition(modelPartType));
    }
  }

  private enum KnownPose {
    T_POSE(
        Map.of(
            ModelPartType.RIGHT_ARM, new CustomRotation(0, 0, 90),
            ModelPartType.LEFT_ARM, new CustomRotation(0, 0, -90))),
    ZOMBIE(
        Map.of(
            ModelPartType.RIGHT_ARM, new CustomRotation(-90, 0, 0),
            ModelPartType.LEFT_ARM, new CustomRotation(-90, 0, 0))),
    WAVE(Map.of(ModelPartType.RIGHT_ARM, new CustomRotation(0, 0, 150))),
    SALUTE(Map.of(ModelPartType.RIGHT_ARM, new CustomRotation(-120, -30, 0))),
    CHEER(
        Map.of(
            ModelPartType.RIGHT_ARM, new CustomRotation(0, 0, 160),
            ModelPartType.LEFT_ARM, new CustomRotation(0, 0, -160))),
    CROSSED_ARMS(
        Map.of(
            ModelPartType.RIGHT_ARM, new CustomRotation(-70, 30, 0),
            ModelPartType.LEFT_ARM, new CustomRotation(-70, -30, 0))),
    BOW(
        Map.of(
            ModelPartType.BODY, new CustomRotation(30, 0, 0),
            ModelPartType.RIGHT_ARM, new CustomRotation(-30, 0, 0),
            ModelPartType.LEFT_ARM, new CustomRotation(-30, 0, 0))),
    RUNNING(
        Map.of(
            ModelPartType.RIGHT_ARM, new CustomRotation(-45, 0, 0),
            ModelPartType.LEFT_ARM, new CustomRotation(45, 0, 0),
            ModelPartType.RIGHT_LEG, new CustomRotation(45, 0, 0),
            ModelPartType.LEFT_LEG, new CustomRotation(-45, 0, 0))),
    SITTING(
        Map.of(
            ModelPartType.RIGHT_LEG, new CustomRotation(-90, 10, 0),
            ModelPartType.LEFT_LEG, new CustomRotation(-90, -10, 0))),
    KNEELING(
        Map.of(
            ModelPartType.BODY, new CustomRotation(10, 0, 0),
            ModelPartType.RIGHT_LEG, new CustomRotation(-90, 0, 0),
            ModelPartType.LEFT_LEG, new CustomRotation(40, 0, 0)));

    private final Map<ModelPartType, CustomRotation> rotationsInDegrees;

    KnownPose(Map<ModelPartType, CustomRotation> rotationsInDegrees) {
      this.rotationsInDegrees = rotationsInDegrees;
    }
  }

  private record ExpectedPose(
      Map<ModelPartType, CustomRotation> rotations, Map<ModelPartType, CustomPosition> positions) {}
}
