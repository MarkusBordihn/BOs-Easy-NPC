/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.ParseResults;
import de.markusbordihn.easynpc.security.CommandAuthority;
import de.markusbordihn.easynpc.security.CommandExecutionSubject;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.CommandSecurity;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CommandExecutor {

  protected static final Logger log = LogManager.getLogger(CommandExecutor.class);

  private CommandExecutor() {}

  public static boolean isBlockedUnsafeNPCCommand(String command) {
    return CommandSecurity.isBlockedUnsafeNpcCommand(command);
  }

  public static void executeEntityCommand(
      String command, Entity entity, int permissionLevel, boolean debug) {
    executeEntityCommand(
        command, entity, CommandPermissionLevel.fromMinecraftLevel(permissionLevel), debug);
  }

  public static void executeEntityCommand(
      String command, Entity entity, CommandPermissionLevel permissionLevel, boolean debug) {
    executeEntityCommand(
        command,
        entity,
        new CommandAuthority(CommandExecutionSubject.NPC_ENTITY, permissionLevel, permissionLevel),
        debug);
  }

  public static void executeEntityCommand(
      String command, Entity entity, CommandAuthority commandAuthority, boolean debug) {
    MinecraftServer minecraftServer = entity.getServer();
    if (minecraftServer == null) {
      log.error("No Minecraft server found for entity {}", entity);
      return;
    }

    CommandPermissionLevel permissionLevel =
        commandAuthority != null ? commandAuthority.effective() : CommandPermissionLevel.ALL;
    if (isBlockedUnsafeNPCCommand(command)) {
      log.warn(
          "Blocked unsafe entity command {} for {} with permission level {}!",
          command,
          entity,
          permissionLevel);
      return;
    }

    if (command.startsWith("/")) {
      command = command.substring(1);
    }
    log.debug(
        "Execute Entity {} Command: \"{}\" with permission level {}",
        entity,
        command,
        permissionLevel);
    Commands commands = minecraftServer.getCommands();
    CommandSourceStack commandSourceStack =
        minecraftServer
            .createCommandSourceStack()
            .withEntity(entity)
            .withPosition(entity.position())
            .withRotation(entity.getRotationVector())
            .withPermission(permissionLevel.minecraftLevel());
    CommandDispatcher<CommandSourceStack> commandDispatcher = commands.getDispatcher();
    ParseResults<CommandSourceStack> parseResults =
        commandDispatcher.parse(
            command, debug ? commandSourceStack : commandSourceStack.withSuppressedOutput());
    commands.performCommand(parseResults, command);
  }

  public static void executePlayerCommand(
      String command, ServerPlayer serverPlayer, int permissionLevel, boolean debug) {
    executePlayerCommand(
        command, serverPlayer, CommandPermissionLevel.fromMinecraftLevel(permissionLevel), debug);
  }

  public static void executePlayerCommand(
      String command,
      ServerPlayer serverPlayer,
      CommandPermissionLevel permissionLevel,
      boolean debug) {
    executePlayerCommand(
        command,
        serverPlayer,
        new CommandAuthority(CommandExecutionSubject.USER, permissionLevel, permissionLevel),
        debug);
  }

  public static void executePlayerCommand(
      String command, ServerPlayer serverPlayer, CommandAuthority commandAuthority, boolean debug) {
    MinecraftServer minecraftServer = serverPlayer.getServer();
    if (minecraftServer == null) {
      log.error("No Minecraft server found for player {}", serverPlayer);
      return;
    }

    CommandPermissionLevel permissionLevel =
        commandAuthority != null ? commandAuthority.effective() : CommandPermissionLevel.ALL;
    if (isBlockedUnsafeNPCCommand(command)) {
      log.warn(
          "Blocked unsafe player command {} for {} with permission level {}!",
          command,
          serverPlayer,
          permissionLevel);
      return;
    }

    if (command.startsWith("/")) {
      command = command.substring(1);
    }
    log.debug(
        "Execute Player {} Command: \"{}\" with permission level {}",
        serverPlayer,
        command,
        permissionLevel);
    Commands commands = minecraftServer.getCommands();
    CommandSourceStack commandSourceStack =
        minecraftServer
            .createCommandSourceStack()
            .withEntity(serverPlayer)
            .withPosition(serverPlayer.position())
            .withRotation(serverPlayer.getRotationVector())
            .withPermission(permissionLevel.minecraftLevel())
            .withLevel(serverPlayer.serverLevel());
    CommandDispatcher<CommandSourceStack> commandDispatcher = commands.getDispatcher();
    ParseResults<CommandSourceStack> parseResults =
        commandDispatcher.parse(
            command, debug ? commandSourceStack : commandSourceStack.withSuppressedOutput());
    commands.performCommand(parseResults, command);
  }
}
