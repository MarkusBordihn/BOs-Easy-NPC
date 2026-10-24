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
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.CommandBlockEntity;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;

public class PresetCommandSecurityTestHelper {

  private static final String DEFAULT_PRESET =
      "easy_npc:default_preset/orc/trainings_dummy.npc.snbt";

  private PresetCommandSecurityTestHelper() {}

  public static void assertCommandBlockPresetImport(GameTestHelper helper) {
    BlockPos commandBlockPosition = new BlockPos(1, 1, 1);
    helper.setBlock(commandBlockPosition, Blocks.COMMAND_BLOCK);

    CommandBlockEntity commandBlockEntity =
        (CommandBlockEntity) helper.getBlockEntity(commandBlockPosition);
    commandBlockEntity
        .getCommandBlock()
        .setCommand("easy_npc preset import_new default " + DEFAULT_PRESET + " ~ ~1 ~");
    commandBlockEntity.getCommandBlock().performCommand(helper.getLevel());

    assertImportedNPC(
        helper, findImportedNPC(helper), CommandPermissionLevel.GAMEMASTERS, null, "command block");
  }

  public static void assertPlayerPresetImport(GameTestHelper helper) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 1, 1));
    CommandSourceStack commandSourceStack =
        serverPlayer
            .createCommandSourceStack()
            .withPermission(CommandPermissionLevel.GAMEMASTERS.minecraftLevel())
            .withPosition(helper.absoluteVec(new Vec3(1, 1, 1)));
    executePresetImport(helper, commandSourceStack, "import_new", "");

    assertImportedNPC(
        helper,
        findImportedNPC(helper),
        CommandPermissionLevel.GAMEMASTERS,
        serverPlayer,
        "player");
  }

  public static void assertConsolePresetImport(GameTestHelper helper) {
    CommandSourceStack commandSourceStack =
        helper
            .getLevel()
            .getServer()
            .createCommandSourceStack()
            .withLevel(helper.getLevel())
            .withPosition(helper.absoluteVec(new Vec3(1, 1, 1)));
    executePresetImport(helper, commandSourceStack, "import_new", "");

    assertImportedNPC(
        helper, findImportedNPC(helper), CommandPermissionLevel.ADMINS, null, "console");
  }

  public static void assertConsolePresetImportWithOwner(GameTestHelper helper) {
    ServerPlayer owner = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 1, 1));
    CommandSourceStack commandSourceStack =
        helper
            .getLevel()
            .getServer()
            .createCommandSourceStack()
            .withLevel(helper.getLevel())
            .withPosition(helper.absoluteVec(new Vec3(1, 1, 1)));
    executePresetImport(helper, commandSourceStack, "import_with_owner", "@p ");

    assertImportedNPC(
        helper,
        findImportedNPC(helper),
        CommandPermissionLevel.ADMINS,
        owner,
        "console with owner");
  }

  private static void executePresetImport(
      GameTestHelper helper,
      CommandSourceStack commandSourceStack,
      String importMode,
      String ownerArgument) {
    String command =
        "easy_npc preset "
            + importMode
            + " default "
            + DEFAULT_PRESET
            + " "
            + ownerArgument
            + "~ ~1 ~";
    CommandDispatcher<CommandSourceStack> commandDispatcher =
        helper.getLevel().getServer().getCommands().getDispatcher();
    try {
      int result = commandDispatcher.execute(commandDispatcher.parse(command, commandSourceStack));
      if (result == 0) {
        helper.fail("Preset import command failed for " + commandSourceStack.getTextName());
      }
    } catch (CommandSyntaxException exception) {
      helper.fail(
          "Preset import command failed for "
              + commandSourceStack.getTextName()
              + ": "
              + exception.getMessage());
    }
  }

  private static void assertImportedNPC(
      GameTestHelper helper,
      EasyNPC<?> importedNPC,
      CommandPermissionLevel expectedPermissionLevel,
      ServerPlayer expectedOwner,
      String sourceName) {
    if (importedNPC == null) {
      helper.fail(sourceName + " did not import the default NPC preset");
      return;
    }

    if (!importedNPC
        .getEasyNPCActionEventData()
        .hasActionEvent(ActionEventType.ON_DISTANCE_CLOSE)) {
      helper.fail("Imported NPC is missing its distance action");
      return;
    }

    CommandPermissionLevel actualPermissionLevel =
        importedNPC.getEasyNPCActionEventData().getActionCommandPermissionLevel();
    if (actualPermissionLevel != expectedPermissionLevel) {
      helper.fail(
          "Expected "
              + sourceName
              + " permission level "
              + expectedPermissionLevel
              + ", got "
              + actualPermissionLevel);
    }

    if (expectedOwner == null && importedNPC.getEasyNPCOwnerData().hasNPCOwner()) {
      helper.fail("Expected imported NPC without owner for " + sourceName);
    }

    if (expectedOwner != null
        && !importedNPC.getEasyNPCOwnerData().isNPCOwner(expectedOwner.getUUID())) {
      helper.fail("Expected imported NPC to be owned by " + expectedOwner.getName().getString());
    }
  }

  private static EasyNPC<?> findImportedNPC(GameTestHelper helper) {
    AABB testBounds = AABB.ofSize(helper.absoluteVec(new Vec3(1.5, 2, 1.5)), 4, 4, 4);
    for (Mob entity : helper.getLevel().getEntitiesOfClass(Mob.class, testBounds)) {
      if (entity instanceof EasyNPC<?> easyNPC
          && "Grukk, the Sparring Post".equals(entity.getName().getString())) {
        return easyNPC;
      }
    }

    return null;
  }
}
