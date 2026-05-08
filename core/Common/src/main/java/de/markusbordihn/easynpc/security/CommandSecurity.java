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
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;

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

  public static CommandPermissionLevel getPlayerPermissionLevel(ServerPlayer serverPlayer) {
    if (serverPlayer == null || serverPlayer.getServer() == null) {
      return CommandPermissionLevel.ALL;
    }

    MinecraftServer minecraftServer = serverPlayer.getServer();
    return CommandPermissionLevel.fromMinecraftLevel(
        minecraftServer.getProfilePermissions(serverPlayer.getGameProfile()));
  }

  public static CommandPermissionLevel getPresetImportCommandLevel(
      ActorSecurityContext actorSecurityContext, PresetTrustLevel trustLevel) {
    if (actorSecurityContext == null) {
      return SecurityConfig.SERVER_TRUSTED_COMMAND_LEVEL;
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
    return new CommandAuthority(
        CommandExecutionSubject.USER,
        requestedLevel,
        actorSecurityContext != null
            ? actorSecurityContext.permissionLevel()
            : CommandPermissionLevel.ALL);
  }

  public static CommandAuthority getNpcCommandAuthority(
      CommandPermissionLevel requestedLevel, CommandPermissionLevel npcLevel) {
    return new CommandAuthority(
        CommandExecutionSubject.NPC_ENTITY,
        requestedLevel,
        npcLevel != null ? npcLevel : CommandPermissionLevel.ALL);
  }

  public static boolean isBlockedUnsafeNpcCommand(String command) {
    return SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS && UnsafeNpcCommand.matches(command);
  }
}
