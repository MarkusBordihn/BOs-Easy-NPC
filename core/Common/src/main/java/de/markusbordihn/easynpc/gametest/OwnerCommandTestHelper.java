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

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.api.event.OwnerEventListener;
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class OwnerCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 1, 1);

  private OwnerCommandTestHelper() {}

  public static void assertOwnerSetChangesOwner(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer newOwner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "owner_target");

    int result =
        executeCommand(
            helper,
            consoleSource(helper),
            "easy_npc owner set " + easyNPC.getEntityUUID() + " " + newOwner.getScoreboardName());

    GameTestHelpers.assertEquals(helper, "Owner set command result", 1, result);
    GameTestHelpers.assertTrue(
        helper,
        "NPC is not owned by the player after owner set",
        easyNPC.getEasyNPCOwnerData().isNPCOwner(newOwner.getUUID()));
  }

  public static void assertOwnerRemoveClearsOwner(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "owner_owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());

    int result =
        executeCommand(
            helper, consoleSource(helper), "easy_npc owner remove " + easyNPC.getEntityUUID());

    GameTestHelpers.assertEquals(helper, "Owner remove command result", 1, result);
    GameTestHelpers.assertTrue(
        helper,
        "NPC still has an owner after owner remove",
        !easyNPC.getEasyNPCOwnerData().hasNPCOwner());
  }

  public static void assertOwnerSetRequiresGamemaster(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "owner_owner");
    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    ownerData.setNPCOwnerUUID(owner.getUUID());
    ServerPlayer player =
        GameTestHelpers.mockSurvivalServerPlayer(helper, NPC_POSITION, "owner_player");
    CommandSourceStack playerSource =
        player
            .createCommandSourceStack()
            .withPermission(CommandPermissionLevel.ALL.minecraftLevel());

    int result =
        executeCommand(
            helper,
            playerSource,
            "easy_npc owner set " + easyNPC.getEntityUUID() + " " + player.getScoreboardName());

    GameTestHelpers.assertEquals(helper, "Owner set command result without permission", 0, result);
    GameTestHelpers.assertTrue(
        helper,
        "Owner was changed by a player without permission",
        ownerData.isNPCOwner(owner.getUUID()));
  }

  public static void assertOwnerChangeUpdatesOwnerIndex(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NPCEntityManager.saveNPC(easyNPC);
    UUID firstOwnerUUID = UUID.randomUUID();
    UUID secondOwnerUUID = UUID.randomUUID();

    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(firstOwnerUUID);
    GameTestHelpers.assertTrue(
        helper,
        "A new owner must list the NPC in the owner index",
        isListedForOwner(easyNPC, firstOwnerUUID));

    CompoundTag presetTag = new CompoundTag();
    presetTag.putUUID(OwnerDataCapable.DATA_OWNER_TAG, secondOwnerUUID);
    easyNPC.getEasyNPCPresetData().importPresetData(presetTag);
    GameTestHelpers.assertTrue(
        helper,
        "A preset import must remove the NPC from the previous owner",
        !isListedForOwner(easyNPC, firstOwnerUUID));
    GameTestHelpers.assertTrue(
        helper,
        "A preset import must list the NPC for the imported owner",
        isListedForOwner(easyNPC, secondOwnerUUID));

    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(null);
    GameTestHelpers.assertTrue(
        helper,
        "Removing the owner must remove the NPC from the owner index",
        !isListedForOwner(easyNPC, secondOwnerUUID));
  }

  public static void assertSameOwnerFiresOneEvent(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NPCEntityManager.saveNPC(easyNPC);
    UUID ownerUUID = UUID.randomUUID();
    AtomicInteger ownerEvents = new AtomicInteger();
    OwnerEventListener ownerEventListener =
        (changedNPC, previousOwnerUUID, changedOwnerUUID) -> {
          if (changedNPC == easyNPC && ownerUUID.equals(changedOwnerUUID)) {
            ownerEvents.incrementAndGet();
          }
        };
    EasyNPCEventRegistry.registerOwnerEventListener(ownerEventListener);

    try {
      easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(ownerUUID);
      easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(ownerUUID);
    } finally {
      EasyNPCEventRegistry.unregisterOwnerEventListener(ownerEventListener);
    }

    GameTestHelpers.assertEquals(
        helper, "Setting the same owner twice must fire one event", 1, ownerEvents.get());
  }

  public static void assertOwnerSetBeforeSpawnFiresOneEvent(
      GameTestHelper helper, EntityType<?> entityType) {
    UUID ownerUUID = UUID.randomUUID();
    AtomicInteger ownerEvents = new AtomicInteger();
    OwnerEventListener ownerEventListener =
        (changedNPC, previousOwnerUUID, changedOwnerUUID) -> {
          if (ownerUUID.equals(changedOwnerUUID)) {
            ownerEvents.incrementAndGet();
          }
        };
    Entity entity = entityType.create(helper.getLevel());
    GameTestHelpers.assertTrue(
        helper, "The entity type must create an Easy NPC", entity instanceof EasyNPC<?>);
    EasyNPC<?> easyNPC = (EasyNPC<?>) entity;
    entity.setPos(helper.absoluteVec(NPC_POSITION));
    GameTestHelpers.removeNPCDataAfterTest(helper, easyNPC.getEntityUUID());
    EasyNPCEventRegistry.registerOwnerEventListener(ownerEventListener);

    try {
      easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(ownerUUID);
      helper.getLevel().addFreshEntity(entity);
    } finally {
      EasyNPCEventRegistry.unregisterOwnerEventListener(ownerEventListener);
    }

    GameTestHelpers.assertEquals(
        helper, "An owner set before the spawn must fire one event", 1, ownerEvents.get());
    GameTestHelpers.assertTrue(
        helper,
        "An owner set before the spawn must list the NPC in the owner index",
        isListedForOwner(easyNPC, ownerUUID));
  }

  private static boolean isListedForOwner(EasyNPC<?> easyNPC, UUID ownerUUID) {
    return EasyNPCEntityHandler.getByOwner(ownerUUID).stream()
        .anyMatch(
            savedNPCEntityEntry ->
                easyNPC.getEntityUUID().equals(savedNPCEntityEntry.entityUUID()));
  }

  private static CommandSourceStack consoleSource(GameTestHelper helper) {
    return helper
        .getLevel()
        .getServer()
        .createCommandSourceStack()
        .withLevel(helper.getLevel())
        .withPosition(helper.absoluteVec(NPC_POSITION));
  }

  private static int executeCommand(
      GameTestHelper helper, CommandSourceStack commandSourceStack, String command) {
    CommandDispatcher<CommandSourceStack> commandDispatcher =
        helper.getLevel().getServer().getCommands().getDispatcher();
    try {
      return commandDispatcher.execute(commandDispatcher.parse(command, commandSourceStack));
    } catch (CommandSyntaxException e) {
      return 0;
    }
  }
}
