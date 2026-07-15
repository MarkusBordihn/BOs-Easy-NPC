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

import de.markusbordihn.easynpc.config.SecurityConfig;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.CommandExecutor;
import de.markusbordihn.easynpc.security.CommandAuthority;
import de.markusbordihn.easynpc.security.CommandExecutionSubject;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.CommandSecurity;
import java.lang.reflect.Field;
import java.util.EnumMap;
import java.util.Set;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class ExecuteAsUserCommandTestHelper {

  private ExecuteAsUserCommandTestHelper() {}

  public static void assertNormalPlayerTeleportWithGamemasterAllowList(GameTestHelper helper) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 1, 1));
    if (CommandSecurity.getPlayerPermissionLevel(serverPlayer) != CommandPermissionLevel.ALL) {
      helper.fail("Expected mock player to have command permission level ALL");
      return;
    }

    EnumMap<CommandPermissionLevel, Set<String>> allowList = getExecuteAsUserAllowList(helper);
    EnumMap<CommandPermissionLevel, Set<String>> originalAllowList = new EnumMap<>(allowList);
    CommandAuthority commandAuthority =
        new CommandAuthority(
            CommandExecutionSubject.USER,
            CommandPermissionLevel.GAMEMASTERS,
            CommandPermissionLevel.ADMINS);
    Vec3 originalPosition = serverPlayer.position();

    try {
      clearAllowList(allowList);
      CommandExecutor.executePlayerCommand("tp @s ~1 ~ ~", serverPlayer, commandAuthority, true);
      assertPosition(helper, serverPlayer, originalPosition, "without an allowlist entry");

      allowList.put(CommandPermissionLevel.GAMEMASTERS, Set.of("tp"));
      CommandExecutor.executePlayerCommand("tp @s ~1 ~ ~", serverPlayer, commandAuthority, true);
      assertPosition(
          helper,
          serverPlayer,
          originalPosition.add(1, 0, 0),
          "with a GAMEMASTERS allowlist entry");
    } finally {
      allowList.clear();
      allowList.putAll(originalAllowList);
    }
  }

  @SuppressWarnings("unchecked")
  private static EnumMap<CommandPermissionLevel, Set<String>> getExecuteAsUserAllowList(
      GameTestHelper helper) {
    try {
      Field field = SecurityConfig.class.getDeclaredField("EXECUTE_AS_USER_COMMAND_ALLOW_LIST");
      field.setAccessible(true);
      return (EnumMap<CommandPermissionLevel, Set<String>>) field.get(null);
    } catch (ReflectiveOperationException exception) {
      helper.fail("Unable to access execute-as-user command allowlist: " + exception.getMessage());
      return new EnumMap<>(CommandPermissionLevel.class);
    }
  }

  private static void clearAllowList(EnumMap<CommandPermissionLevel, Set<String>> allowList) {
    allowList.clear();
    for (CommandPermissionLevel permissionLevel : CommandPermissionLevel.values()) {
      allowList.put(permissionLevel, Set.of());
    }
  }

  private static void assertPosition(
      GameTestHelper helper, ServerPlayer serverPlayer, Vec3 expected, String scenario) {
    if (serverPlayer.position().distanceToSqr(expected) > 0.0001D) {
      helper.fail(
          "Expected player position "
              + expected
              + " "
              + scenario
              + ", got "
              + serverPlayer.position());
    }
  }
}
