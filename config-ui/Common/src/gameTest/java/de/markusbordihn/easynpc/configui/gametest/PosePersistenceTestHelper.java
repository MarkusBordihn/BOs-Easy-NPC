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

import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelRotationMessage;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.gametest.GameTestHelpers;
import de.markusbordihn.easynpc.handler.PresetHandler;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.phys.Vec3;

public final class PosePersistenceTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 0);

  private static final int WAIT_TICKS = 20 * 70;
  public static final int TIMEOUT_TICKS = WAIT_TICKS + 200;

  private static final float QUARTER_TURN = (float) (Math.PI / 2);
  private static final Map<ModelPartType, CustomRotation> T_POSE = createTPose();

  private PosePersistenceTestHelper() {}

  private static Map<ModelPartType, CustomRotation> createTPose() {
    Map<ModelPartType, CustomRotation> tPose = new EnumMap<>(ModelPartType.class);
    tPose.put(ModelPartType.HEAD, new CustomRotation(-0.2f, 0.4f, 0.0f));
    tPose.put(ModelPartType.BODY, new CustomRotation(0.1f, 0.0f, 0.0f));
    tPose.put(ModelPartType.RIGHT_ARM, new CustomRotation(0.0f, 0.0f, QUARTER_TURN));
    tPose.put(ModelPartType.LEFT_ARM, new CustomRotation(0.0f, 0.0f, -QUARTER_TURN));
    tPose.put(ModelPartType.RIGHT_LEG, new CustomRotation(0.0f, 0.0f, 0.15f));
    tPose.put(ModelPartType.LEFT_LEG, new CustomRotation(0.0f, 0.0f, -0.15f));
    return tPose;
  }

  public static void assertTPoseSurvivesWaitRespawnAndPreset(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer poseEditor =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "test-pose-editor");
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    for (Map.Entry<ModelPartType, CustomRotation> partRotation : T_POSE.entrySet()) {
      ServerMessageAssertions.receive(
          uuid ->
              new ChangeModelRotationMessage(uuid, partRotation.getKey(), partRotation.getValue()),
          ChangeModelRotationMessage::create,
          easyNPC,
          poseEditor);
    }
    assertTPose(helper, easyNPC, "after editing");
    assertTPose(helper, NPCPersistenceAssertions.retrack(helper, easyNPC), "after re-tracking");

    helper.runAfterDelay(
        WAIT_TICKS,
        () -> {
          assertTPose(helper, easyNPC, "after waiting " + WAIT_TICKS + " ticks");
          assertTPose(
              helper,
              NPCPersistenceAssertions.retrack(helper, easyNPC),
              "after waiting and re-tracking");

          EasyNPC<?> respawnedNPC = NPCPersistenceAssertions.respawn(helper, easyNPC);
          assertTPose(helper, respawnedNPC, "after respawn");

          EasyNPC<?> presetNPC = importAsPreset(helper, respawnedNPC);
          assertTPose(helper, presetNPC, "after preset export and import");
          helper.succeed();
        });
  }

  private static EasyNPC<?> importAsPreset(GameTestHelper helper, EasyNPC<?> easyNPC) {
    CompoundTag presetTag = PresetHandler.prepareExportData(easyNPC);
    GameTestHelpers.assertNotNull(helper, "Preset export is null", presetTag);
    presetTag.remove(PresetHandler.UUID_TAG);

    EasyNPC<?> presetNPC =
        GameTestHelpers.mockEasyNPC(helper, easyNPC.getEntity().getType(), NPC_POSITION);
    presetNPC.registerEasyNPCDefaultData();
    ((PresetDataCapable<?>) presetNPC).importPresetData(presetTag);
    return presetNPC;
  }

  private static void assertTPose(GameTestHelper helper, EasyNPC<?> easyNPC, String stage) {
    String npcName = easyNPC.getEntity().getType() + " " + stage;
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertEquals(
        helper, "Model pose of " + npcName, ModelPose.CUSTOM, modelData.getModelPose());
    GameTestHelpers.assertEquals(
        helper, "Pose of " + npcName, Pose.STANDING, easyNPC.getEntity().getPose());
    for (Map.Entry<ModelPartType, CustomRotation> partRotation : T_POSE.entrySet()) {
      GameTestHelpers.assertEquals(
          helper,
          partRotation.getKey() + " rotation of " + npcName,
          partRotation.getValue(),
          modelData.getModelPartRotation(partRotation.getKey()));
    }
  }
}
