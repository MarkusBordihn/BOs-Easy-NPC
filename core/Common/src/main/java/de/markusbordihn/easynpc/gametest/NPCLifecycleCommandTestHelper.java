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

import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Locale;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class NPCLifecycleCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final double POSITION_TOLERANCE = 1.0E-6D;

  private NPCLifecycleCommandTestHelper() {}

  private static EasyNPC<?> mockStoredNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NPCEntityManager.saveNPC(easyNPC);
    return easyNPC;
  }

  private static UUID despawnAsKilled(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    GameTestHelpers.assertTrue(
        helper,
        "Failed to remove the NPC as killed",
        EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.KILLED));
    return easyNPC.getEntityUUID();
  }

  private static int executeConsoleCommand(GameTestHelper helper, String command) {
    return GameTestHelpers.executeCommand(
        helper, GameTestHelpers.consoleCommandSource(helper, NPC_POSITION), command);
  }

  private static EasyNPC<?> getLoadedNPC(GameTestHelper helper, UUID entityUUID) {
    return LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
  }

  public static void assertRespawnCommandReplacesNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();

    int result = executeConsoleCommand(helper, "easy_npc respawn " + entityUUID);

    GameTestHelpers.assertEquals(helper, "Respawn command result", 1, result);
    GameTestHelpers.assertTrue(
        helper, "Respawned NPC was not removed", easyNPC.getEntity().isRemoved());
    EasyNPC<?> respawnedNPC = getLoadedNPC(helper, entityUUID);
    GameTestHelpers.assertNotNull(
        helper, "Respawned NPC " + entityUUID + " was not found in the world", respawnedNPC);
    GameTestHelpers.assertTrue(
        helper, "Respawn command kept the old NPC instance", respawnedNPC != easyNPC);
  }

  public static void assertReviveCommandRevivesKilledNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    UUID entityUUID = despawnAsKilled(helper, entityType);

    int spawnResult = executeConsoleCommand(helper, "easy_npc spawn " + entityUUID);

    GameTestHelpers.assertEquals(helper, "Spawn command result for a killed NPC", 0, spawnResult);

    int reviveResult = executeConsoleCommand(helper, "easy_npc revive " + entityUUID);

    GameTestHelpers.assertEquals(helper, "Revive command result", 1, reviveResult);
    GameTestHelpers.assertNotNull(
        helper,
        "Revived NPC " + entityUUID + " was not found in the world",
        getLoadedNPC(helper, entityUUID));
  }

  public static void assertReviveCommandUsesGivenPosition(
      GameTestHelper helper, EntityType<?> entityType) {
    UUID entityUUID = despawnAsKilled(helper, entityType);
    Vec3 revivePosition = helper.absoluteVec(new Vec3(2.5, 2, 2.5));

    int result =
        executeConsoleCommand(
            helper,
            String.format(
                Locale.ROOT,
                "easy_npc revive %s %.3f %.3f %.3f",
                entityUUID,
                revivePosition.x,
                revivePosition.y,
                revivePosition.z));

    GameTestHelpers.assertEquals(helper, "Revive command result", 1, result);
    EasyNPC<?> revivedNPC = getLoadedNPC(helper, entityUUID);
    GameTestHelpers.assertNotNull(
        helper, "Revived NPC " + entityUUID + " was not found in the world", revivedNPC);
    Vec3 revivedPosition = revivedNPC.getEntity().position();
    GameTestHelpers.assertTrue(
        helper,
        "Revived NPC is at " + revivedPosition + " instead of " + revivePosition,
        revivedPosition.distanceToSqr(revivePosition) <= POSITION_TOLERANCE);
  }

  public static void assertReviveCommandRequiresAccess(
      GameTestHelper helper, EntityType<?> entityType) {
    UUID entityUUID = despawnAsKilled(helper, entityType);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, "revive_stranger");

    int result =
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.playerCommandSource(serverPlayer),
            "easy_npc revive " + entityUUID);

    GameTestHelpers.assertEquals(helper, "Revive command result of a stranger", 0, result);
    GameTestHelpers.assertTrue(
        helper,
        "A stranger revived NPC " + entityUUID,
        getLoadedNPC(helper, entityUUID) == null);
  }

  public static void assertReviveCommandRejectsNPCOnShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "revive_shoulder");
    serverPlayer.setOnGround(true);
    UUID entityUUID = easyNPC.getEntityUUID();
    GameTestHelpers.assertTrue(
        helper,
        "NPC " + entityUUID + " did not sit on the shoulder",
        EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer));

    int result = executeConsoleCommand(helper, "easy_npc revive " + entityUUID);

    GameTestHelpers.assertEquals(helper, "Revive command result for an NPC on a shoulder", 0, result);
    Optional<UUID> shoulderUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.LEFT);
    GameTestHelpers.assertTrue(
        helper,
        "Revive command took NPC " + entityUUID + " from the shoulder",
        shoulderUUID.isPresent() && shoulderUUID.get().equals(entityUUID));
  }
}
