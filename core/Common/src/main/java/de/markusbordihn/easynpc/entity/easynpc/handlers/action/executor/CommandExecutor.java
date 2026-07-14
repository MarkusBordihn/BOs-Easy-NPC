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
import com.mojang.brigadier.context.CommandContextBuilder;
import de.markusbordihn.easynpc.config.SecurityConfig;
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
  private static final String UNSAFE_COMMAND_CONFIG_KEYS =
      "blockUnsafeNpcCommands and unsafeNpcCommands";

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
    if (!CommandSecurity.isExecuteAsNpcCommandAllowed(command)) {
      log.warn(
          "Blocked unsafe entity command '{}' for {} with permission level {}. Adjust {} keys {} to change this.",
          CommandSecurity.getRootCommandName(command),
          entity,
          permissionLevel,
          SecurityConfig.CONFIG_FILE_NAME,
          UNSAFE_COMMAND_CONFIG_KEYS);
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
          "Blocked unsafe player command '{}' for {} with permission level {}. Adjust {} keys {} to change this.",
          CommandSecurity.getRootCommandName(command),
          serverPlayer,
          permissionLevel,
          SecurityConfig.CONFIG_FILE_NAME,
          UNSAFE_COMMAND_CONFIG_KEYS);
      return;
    }

    command = normalizeCommand(command);
    if (command.isEmpty()) {
      log.warn("Blocked empty player command for {}", serverPlayer);
      return;
    }

    String rootCommandName = CommandSecurity.getRootCommandName(command);
    CommandPermissionLevel npcPermissionLevel =
        commandAuthority != null ? commandAuthority.ceiling() : CommandPermissionLevel.ALL;
    CommandPermissionLevel requestedPermissionLevel =
        commandAuthority != null ? commandAuthority.requested() : CommandPermissionLevel.ALL;
    CommandPermissionLevel maxPermissionLevel =
        CommandPermissionLevel.min(requestedPermissionLevel, npcPermissionLevel);
    CommandPermissionLevel playerPermissionLevel =
        CommandSecurity.getPlayerPermissionLevel(serverPlayer);
    if (!CommandSecurity.isExecuteAsUserCommandAllowed(command, maxPermissionLevel)) {
      log.warn(
          "Blocked execute-as-player command '{}' for {}: not allowlisted up to {}. Add it to {} in {}.",
          rootCommandName,
          serverPlayer,
          maxPermissionLevel,
          getExecuteAsUserAllowListKey(maxPermissionLevel),
          SecurityConfig.getConfigFilePath());
      log.debug(
          "Execute-as-player permissions: player {}, requested {}, NPC cap {}, active allowlist up to {}: {}",
          playerPermissionLevel,
          requestedPermissionLevel,
          npcPermissionLevel,
          maxPermissionLevel,
          SecurityConfig.getExecuteAsUserAllowedRoots(maxPermissionLevel));
      return;
    }

    Commands commands = minecraftServer.getCommands();
    CommandDispatcher<CommandSourceStack> commandDispatcher = commands.getDispatcher();
    CommandPermissionLevel basePermissionLevel =
        CommandPermissionLevel.min(
            requestedPermissionLevel,
            CommandPermissionLevel.min(playerPermissionLevel, npcPermissionLevel));
    CommandSourceStack baseCommandSourceStack =
        createPlayerCommandSourceStack(serverPlayer, basePermissionLevel, debug);
    ParseResults<CommandSourceStack> parseResults =
        commandDispatcher.parse(command, baseCommandSourceStack);
    if (isParseSuccessful(parseResults)) {
      log.debug(
          "Execute Player {} Command: \"{}\" with player permission level {}, capped by NPC permission level {}",
          serverPlayer,
          command,
          basePermissionLevel,
          npcPermissionLevel);
      commands.performCommand(parseResults, command);
      return;
    }

    if (maxPermissionLevel == basePermissionLevel) {
      log.warn(
          "Execute-as-player command '{}' is unavailable for {} at permission level {}.",
          rootCommandName,
          serverPlayer,
          basePermissionLevel);
      log.debug(
          "Execute-as-player permissions: player {}, requested {}, NPC cap {}",
          playerPermissionLevel,
          requestedPermissionLevel,
          npcPermissionLevel);
      return;
    }

    CommandSourceStack elevatedCommandSourceStack =
        createPlayerCommandSourceStack(serverPlayer, maxPermissionLevel, debug);
    parseResults = commandDispatcher.parse(command, elevatedCommandSourceStack);
    if (!isParseSuccessful(parseResults)) {
      log.warn(
          "Execute-as-player command '{}' is unavailable for {} at elevated permission level {}.",
          rootCommandName,
          serverPlayer,
          maxPermissionLevel);
      log.debug(
          "Execute-as-player permissions: base {}, NPC cap {}",
          basePermissionLevel,
          npcPermissionLevel);
      return;
    }

    log.info(
        "Execute allowlisted player command {} for {} with permission level {} (player {}, NPC cap {})",
        rootCommandName,
        serverPlayer,
        maxPermissionLevel,
        playerPermissionLevel,
        npcPermissionLevel);
    commands.performCommand(parseResults, command);
  }

  private static String normalizeCommand(String command) {
    if (command == null) {
      return "";
    }

    String normalizedCommand = command.trim();
    return normalizedCommand.startsWith("/") ? normalizedCommand.substring(1) : normalizedCommand;
  }

  private static String getExecuteAsUserAllowListKey(CommandPermissionLevel permissionLevel) {
    return "executeAsUserCommandAllowList." + permissionLevel.name();
  }

  private static CommandSourceStack createPlayerCommandSourceStack(
      ServerPlayer serverPlayer, CommandPermissionLevel permissionLevel, boolean debug) {
    CommandSourceStack commandSourceStack =
        serverPlayer.createCommandSourceStack().withPermission(permissionLevel.minecraftLevel());
    return debug ? commandSourceStack : commandSourceStack.withSuppressedOutput();
  }

  private static boolean isParseSuccessful(ParseResults<CommandSourceStack> parseResults) {
    if (parseResults == null || parseResults.getReader().canRead()) {
      return false;
    }

    CommandContextBuilder<CommandSourceStack> context = parseResults.getContext();
    while (context != null) {
      if (context.getCommand() != null) {
        return true;
      }
      context = context.getChild();
    }
    return false;
  }
}
