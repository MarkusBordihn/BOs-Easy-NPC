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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class ModelRootCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);

  private ModelRootCommandTestHelper() {}

  private static EasyNPC<?> mockSpawnedNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    return easyNPC;
  }

  private static int executeModelCommand(
      GameTestHelper helper, String commandName, EasyNPC<?> easyNPC, String partAndValues) {
    return GameTestHelpers.executeCommand(
        helper,
        GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
        "easy_npc " + commandName + " " + easyNPC.getEntityUUID() + " " + partAndValues);
  }

  private static void assertPoseIsNotCustom(GameTestHelper helper, ModelDataCapable<?> modelData) {
    GameTestHelpers.assertTrue(
        helper,
        "Root transform must not force the custom pose, but was " + modelData.getModelPose(),
        modelData.getModelPose() != ModelPose.CUSTOM);
  }

  public static void assertScaleCommandAcceptsRoot(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result = executeModelCommand(helper, "scale", easyNPC, "root 2");

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertEquals(helper, "Root scale command result", 1, result);
    GameTestHelpers.assertEquals(
        helper, "Root scale", new CustomScale(2, 2, 2), modelData.getModelRootData().scale());
    assertPoseIsNotCustom(helper, modelData);
  }

  public static void assertRotateCommandAcceptsRoot(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result = executeModelCommand(helper, "rotate", easyNPC, "root 0 90 0");

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertEquals(helper, "Root rotate command result", 1, result);
    GameTestHelpers.assertEquals(
        helper, "Root rotation y", 90.0F, modelData.getModelRootData().rotation().y());
    GameTestHelpers.assertEquals(helper, "Entity y rotation", 90.0F, easyNPC.getEntity().getYRot());
    assertPoseIsNotCustom(helper, modelData);
  }

  public static void assertPositionCommandAcceptsRoot(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result = executeModelCommand(helper, "position", easyNPC, "root 0 0.5 0");

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertEquals(helper, "Root position command result", 1, result);
    GameTestHelpers.assertEquals(
        helper,
        "Root position",
        new CustomPosition(0, 0.5F, 0),
        modelData.getModelPartPosition(ModelPartType.ROOT));
    assertPoseIsNotCustom(helper, modelData);
  }

  public static void assertScaleCommandOnModelPartSetsCustomPose(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result = executeModelCommand(helper, "scale", easyNPC, "head 2");

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    GameTestHelpers.assertEquals(helper, "Head scale command result", 1, result);
    GameTestHelpers.assertEquals(
        helper,
        "Head scale",
        new CustomScale(2, 2, 2),
        modelData.getModelPartScale(ModelPartType.HEAD));
    GameTestHelpers.assertEquals(helper, "Model pose", ModelPose.CUSTOM, modelData.getModelPose());
  }
}
