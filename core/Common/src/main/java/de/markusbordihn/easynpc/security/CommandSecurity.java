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

package de.markusbordihn.easynpc.security;

import de.markusbordihn.easynpc.config.SecurityConfig;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.permissions.Permission;
import net.minecraft.server.permissions.PermissionSet;
import net.minecraft.server.permissions.Permissions;

public class CommandSecurity {

  private CommandSecurity() {}

  public static ActorSecurityContext getActorContext(ServerPlayer serverPlayer) {
    if (serverPlayer == null) {
      return null;
    }

    CommandPermissionLevel permissionLevel = getPlayerPermissionLevel(serverPlayer);
    return new ActorSecurityContext(
        serverPlayer,
        serverPlayer.isCreative(),
        permissionLevel,
        permissionLevel.allows(CommandPermissionLevel.GAMEMASTERS));
  }

  public static ActorSecurityContext getActorContext(CommandSourceStack commandSourceStack) {
    if (commandSourceStack == null) {
      return null;
    }

    ServerPlayer serverPlayer =
        commandSourceStack.getEntity() instanceof ServerPlayer player ? player : null;
    CommandPermissionLevel permissionLevel = getCommandSourcePermissionLevel(commandSourceStack);
    return new ActorSecurityContext(
        serverPlayer,
        serverPlayer != null && serverPlayer.isCreative(),
        permissionLevel,
        permissionLevel.allows(CommandPermissionLevel.GAMEMASTERS));
  }

  public static ActorSecurityContext getServerActorContext() {
    CommandPermissionLevel permissionLevel = SecurityConfig.SERVER_TRUSTED_COMMAND_LEVEL;
    return new ActorSecurityContext(null, false, permissionLevel, true);
  }

  public static CommandPermissionLevel getCommandSourcePermissionLevel(
      CommandSourceStack commandSourceStack) {
    if (commandSourceStack == null) {
      return CommandPermissionLevel.ALL;
    }

    PermissionSet permissions = commandSourceStack.permissions();
    CommandPermissionLevel permissionLevel = CommandPermissionLevel.ALL;
    for (CommandPermissionLevel candidate : CommandPermissionLevel.values()) {
      Permission permission = getMinecraftPermission(candidate);
      if (permission == null || permissions.hasPermission(permission)) {
        permissionLevel = candidate;
      }
    }
    return permissionLevel;
  }

  private static Permission getMinecraftPermission(CommandPermissionLevel permissionLevel) {
    return switch (permissionLevel) {
      case ALL -> null;
      case MODERATORS -> Permissions.COMMANDS_MODERATOR;
      case GAMEMASTERS -> Permissions.COMMANDS_GAMEMASTER;
      case ADMINS -> Permissions.COMMANDS_ADMIN;
      case OWNERS -> Permissions.COMMANDS_OWNER;
    };
  }

  public static CommandPermissionLevel getPlayerPermissionLevel(ServerPlayer serverPlayer) {
    if (serverPlayer == null) {
      return CommandPermissionLevel.ALL;
    }

    if (serverPlayer
        .createCommandSourceStack()
        .permissions()
        .hasPermission(Permissions.COMMANDS_GAMEMASTER)) {
      return CommandPermissionLevel.GAMEMASTERS;
    }
    return CommandPermissionLevel.ALL;
  }

  public static CommandPermissionLevel getPresetImportCommandLevel(
      ActorSecurityContext actorSecurityContext, PresetTrustLevel trustLevel) {
    if (actorSecurityContext == null) {
      return SecurityConfig.SERVER_TRUSTED_COMMAND_LEVEL;
    }

    if (actorSecurityContext.player() == null) {
      return CommandPermissionLevel.min(
          actorSecurityContext.permissionLevel(), SecurityConfig.SERVER_TRUSTED_COMMAND_LEVEL);
    }

    if (actorSecurityContext.admin()) {
      return CommandPermissionLevel.min(
          actorSecurityContext.permissionLevel(), SecurityConfig.MAX_ADMIN_IMPORTED_COMMAND_LEVEL);
    }

    if (trustLevel == PresetTrustLevel.CREATIVE_PLAYER || actorSecurityContext.creative()) {
      return CommandPermissionLevel.min(
          actorSecurityContext.permissionLevel(), SecurityConfig.CREATIVE_PLAYER_COMMAND_LEVEL);
    }

    return SecurityConfig.NORMAL_PLAYER_COMMAND_LEVEL;
  }

  public static CommandAuthority getUserCommandAuthority(
      CommandPermissionLevel requestedLevel, ActorSecurityContext actorSecurityContext) {
    return getUserCommandAuthority(
        requestedLevel,
        actorSecurityContext,
        actorSecurityContext != null
            ? actorSecurityContext.permissionLevel()
            : CommandPermissionLevel.ALL);
  }

  public static CommandAuthority getUserCommandAuthority(
      CommandPermissionLevel requestedLevel,
      ActorSecurityContext actorSecurityContext,
      CommandPermissionLevel npcLevel) {
    return new CommandAuthority(
        CommandExecutionSubject.USER,
        requestedLevel,
        npcLevel != null ? npcLevel : CommandPermissionLevel.ALL);
  }

  public static CommandAuthority getNpcCommandAuthority(
      CommandPermissionLevel requestedLevel, CommandPermissionLevel npcLevel) {
    return new CommandAuthority(
        CommandExecutionSubject.NPC_ENTITY,
        requestedLevel,
        npcLevel != null ? npcLevel : CommandPermissionLevel.ALL);
  }

  public static boolean isBlockedUnsafeNpcCommand(String command) {
    return SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS
        && UnsafeNpcCommand.matches(command, SecurityConfig.UNSAFE_NPC_COMMANDS);
  }

  public static boolean isExecuteAsNpcCommandAllowed(String command) {
    return !isBlockedUnsafeNpcCommand(command);
  }

  public static String getRootCommandName(String command) {
    return UnsafeNpcCommand.extractRootCommandName(command);
  }

  public static boolean isExecuteAsUserCommandAllowed(
      String command, CommandPermissionLevel permissionLevel) {
    return SecurityConfig.isExecuteAsUserCommandAllowed(
        getRootCommandName(command), permissionLevel);
  }
}
