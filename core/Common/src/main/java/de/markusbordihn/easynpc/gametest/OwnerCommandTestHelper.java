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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
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
