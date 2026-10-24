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

import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class ShoulderCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final int REMOUNT_COOLDOWN_TICKS = 20;

  private ShoulderCommandTestHelper() {}

  private static ServerPlayer mockGroundedPlayer(GameTestHelper helper, String playerName) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, playerName);
    serverPlayer.setOnGround(true);
    return serverPlayer;
  }

  private static EasyNPC<?> mockStoredNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NPCEntityManager.saveNPC(easyNPC);
    return easyNPC;
  }

  private static int executeConsoleCommand(GameTestHelper helper, String command) {
    return GameTestHelpers.executeCommand(
        helper, GameTestHelpers.consoleCommandSource(helper, NPC_POSITION), command);
  }

  private static boolean isOnLeftShoulder(ServerPlayer serverPlayer, UUID entityUUID) {
    Optional<UUID> shoulderUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.LEFT);
    return shoulderUUID.isPresent() && shoulderUUID.get().equals(entityUUID);
  }

  public static void assertSitCommandPutsNPCOnShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder_sit");
    UUID entityUUID = easyNPC.getEntityUUID();

    int result =
        executeConsoleCommand(
            helper,
            "easy_npc shoulder sit " + entityUUID + " " + serverPlayer.getScoreboardName());

    GameTestHelpers.assertEquals(helper, "Shoulder sit command result", 1, result);
    GameTestHelpers.assertTrue(
        helper,
        "Left shoulder does not hold NPC " + entityUUID + " after the sit command",
        isOnLeftShoulder(serverPlayer, entityUUID));
    GameTestHelpers.assertTrue(
        helper,
        "NPC " + entityUUID + " is not stored as sitting on a shoulder",
        EasyNPCShoulderHandler.isOnShoulder(entityUUID));
  }

  public static void assertDismountCommandUsesSideAndCooldown(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder_down");
    UUID entityUUID = easyNPC.getEntityUUID();
    GameTestHelpers.assertTrue(
        helper,
        "NPC " + entityUUID + " did not sit on the shoulder",
        EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer));

    long dismountGameTime = helper.getLevel().getGameTime();
    int result =
        executeConsoleCommand(
            helper,
            "easy_npc shoulder dismount "
                + serverPlayer.getScoreboardName()
                + " left "
                + REMOUNT_COOLDOWN_TICKS);

    GameTestHelpers.assertEquals(helper, "Shoulder dismount command result", 1, result);
    GameTestHelpers.assertTrue(
        helper,
        "Left shoulder still holds the NPC after the dismount command",
        serverPlayer.getShoulderEntityLeft().isEmpty());
    EasyNPC<?> dismountedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(
        helper, "Dismounted NPC " + entityUUID + " was not found in the world", dismountedNPC);
    GameTestHelpers.assertEquals(
        helper,
        "Dismount command ignores the remount cooldown",
        dismountGameTime + REMOUNT_COOLDOWN_TICKS,
        EasyNPCShoulderHandler.getRemountAllowedGameTime(dismountedNPC));
  }

  public static void assertDismountCommandWithoutSideFreesBothShoulders(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> leftNPC = mockStoredNPC(helper, entityType);
    EasyNPC<?> rightNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder_both");
    GameTestHelpers.assertTrue(
        helper,
        "Two NPCs did not sit on both shoulders",
        EasyNPCShoulderHandler.sitOnShoulder(leftNPC, serverPlayer)
            && EasyNPCShoulderHandler.sitOnShoulder(rightNPC, serverPlayer));

    int result =
        executeConsoleCommand(
            helper, "easy_npc shoulder dismount " + serverPlayer.getScoreboardName());

    GameTestHelpers.assertEquals(helper, "Shoulder dismount command result", 1, result);
    GameTestHelpers.assertTrue(
        helper,
        "A shoulder still holds an NPC after the dismount command",
        serverPlayer.getShoulderEntityLeft().isEmpty()
            && serverPlayer.getShoulderEntityRight().isEmpty());
  }

  public static void assertShoulderCommandOnlyControlsOwnPlayer(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer owner = mockGroundedPlayer(helper, "shoulder_owner");
    ServerPlayer otherPlayer =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, "shoulder_other");
    UUID entityUUID = easyNPC.getEntityUUID();
    GameTestHelpers.assertTrue(
        helper,
        "NPC " + entityUUID + " did not sit on the shoulder",
        EasyNPCShoulderHandler.sitOnShoulder(easyNPC, owner));

    int otherPlayerResult =
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.playerCommandSource(otherPlayer),
            "easy_npc shoulder dismount " + owner.getScoreboardName() + " left");

    GameTestHelpers.assertEquals(
        helper, "Dismount command of another player result", 0, otherPlayerResult);
    GameTestHelpers.assertTrue(
        helper,
        "Another player put NPC " + entityUUID + " down from the owner's shoulder",
        isOnLeftShoulder(owner, entityUUID));

    int ownerResult =
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.playerCommandSource(owner),
            "easy_npc shoulder info " + owner.getScoreboardName());

    GameTestHelpers.assertEquals(helper, "Shoulder info command of the owner result", 1, ownerResult);
  }

  public static void assertEnableCommandTogglesObjective(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);

    int enableResult =
        executeConsoleCommand(helper, "easy_npc shoulder enable " + easyNPC.getEntityUUID());

    GameTestHelpers.assertEquals(helper, "Shoulder enable command result", 1, enableResult);
    GameTestHelpers.assertTrue(
        helper,
        "Shoulder enable command did not add the shoulder objective",
        EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER));

    int disableResult =
        executeConsoleCommand(helper, "easy_npc shoulder disable " + easyNPC.getEntityUUID());

    GameTestHelpers.assertEquals(helper, "Shoulder disable command result", 1, disableResult);
    GameTestHelpers.assertTrue(
        helper,
        "Shoulder disable command did not remove the shoulder objective",
        !EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER));
  }

  private static Vec3 shoulderOffset(EasyNPC<?> easyNPC) {
    return easyNPC
        .getEasyNPCAttributeData()
        .getEntityAttributes()
        .getInteractionAttributes()
        .shoulderOffset();
  }

  public static void assertOffsetCommandTravelsOverShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder_offset");
    UUID entityUUID = easyNPC.getEntityUUID();

    int offsetResult =
        executeConsoleCommand(helper, "easy_npc shoulder offset " + entityUUID + " 0.1 -0.25 0.5");

    GameTestHelpers.assertEquals(helper, "Shoulder offset command result", 1, offsetResult);
    GameTestHelpers.assertTrue(
        helper,
        "NPC " + entityUUID + " did not sit on the shoulder",
        EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "NPC " + entityUUID + " was not put down from the shoulder",
        EasyNPCShoulderHandler.dismount(serverPlayer));
    EasyNPC<?> dismountedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    GameTestHelpers.assertNotNull(
        helper, "Dismounted NPC " + entityUUID + " was not found in the world", dismountedNPC);
    GameTestHelpers.assertEquals(
        helper,
        "Shoulder offset after a ride on the shoulder",
        new Vec3(0.1D, -0.25D, 0.5D),
        shoulderOffset(dismountedNPC));

    int resetResult =
        executeConsoleCommand(helper, "easy_npc shoulder offset " + entityUUID + " reset");

    GameTestHelpers.assertEquals(helper, "Shoulder offset reset command result", 1, resetResult);
    GameTestHelpers.assertEquals(
        helper, "Shoulder offset after a reset", Vec3.ZERO, shoulderOffset(dismountedNPC));
  }
}
