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

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.config.SecurityConfig;
import java.lang.reflect.Field;
import java.util.EnumMap;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ExecuteAsUserCommandSecurityTest {

  @SuppressWarnings("unchecked")
  private static EnumMap<CommandPermissionLevel, Set<String>> getExecuteAsUserAllowList()
      throws Exception {
    Field field = SecurityConfig.class.getDeclaredField("EXECUTE_AS_USER_COMMAND_ALLOW_LIST");
    field.setAccessible(true);
    return (EnumMap<CommandPermissionLevel, Set<String>>) field.get(null);
  }

  @Test
  @DisplayName("Should require allowlist entries up to the NPC capped level")
  void testExecuteAsUserAllowListLevels() throws Exception {
    EnumMap<CommandPermissionLevel, Set<String>> allowList = getExecuteAsUserAllowList();
    EnumMap<CommandPermissionLevel, Set<String>> backup = new EnumMap<>(allowList);
    try {
      allowList.clear();
      for (CommandPermissionLevel permissionLevel : CommandPermissionLevel.values()) {
        allowList.put(permissionLevel, Set.of());
      }
      allowList.put(CommandPermissionLevel.GAMEMASTERS, Set.of("shop"));

      assertFalse(
          CommandSecurity.isExecuteAsUserCommandAllowed(
              "/shop My Shop", CommandPermissionLevel.ALL));
      assertTrue(
          CommandSecurity.isExecuteAsUserCommandAllowed(
              "/shop My Shop", CommandPermissionLevel.GAMEMASTERS));
      assertTrue(
          CommandSecurity.isExecuteAsUserCommandAllowed(
              "/shop My Shop", CommandPermissionLevel.ADMINS));
      assertFalse(
          CommandSecurity.isExecuteAsUserCommandAllowed(
              "/say hello", CommandPermissionLevel.GAMEMASTERS));
    } finally {
      allowList.clear();
      allowList.putAll(backup);
    }
  }

  @Test
  @DisplayName("Should cap user command authority by NPC command level")
  void testUserCommandAuthorityNpcCap() {
    CommandAuthority normalNpcAuthority =
        CommandSecurity.getUserCommandAuthority(
            CommandPermissionLevel.ADMINS, null, CommandPermissionLevel.ALL);
    assertEquals(CommandPermissionLevel.ALL, normalNpcAuthority.effective());

    CommandAuthority adminNpcAuthority =
        CommandSecurity.getUserCommandAuthority(
            CommandPermissionLevel.GAMEMASTERS, null, CommandPermissionLevel.ADMINS);
    assertEquals(CommandPermissionLevel.GAMEMASTERS, adminNpcAuthority.effective());
  }

  @Test
  @DisplayName("Should keep execute as NPC independent from execute as user allowlist")
  void testExecuteAsNpcPolicy() throws Exception {
    boolean blockUnsafeNpcCommands = SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS;
    Set<String> unsafeNpcCommands = SecurityConfig.UNSAFE_NPC_COMMANDS;
    EnumMap<CommandPermissionLevel, Set<String>> allowList = getExecuteAsUserAllowList();
    EnumMap<CommandPermissionLevel, Set<String>> allowListBackup = new EnumMap<>(allowList);
    try {
      SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS = true;
      SecurityConfig.UNSAFE_NPC_COMMANDS = Set.of("op");
      allowList.clear();
      for (CommandPermissionLevel permissionLevel : CommandPermissionLevel.values()) {
        allowList.put(permissionLevel, Set.of());
      }

      assertTrue(CommandSecurity.isExecuteAsNpcCommandAllowed("/say hello"));
      assertFalse(CommandSecurity.isExecuteAsNpcCommandAllowed("/op Steve"));
      assertFalse(CommandSecurity.isExecuteAsNpcCommandAllowed("/execute as @a run op Steve"));

      SecurityConfig.UNSAFE_NPC_COMMANDS = Set.of();
      assertTrue(CommandSecurity.isExecuteAsNpcCommandAllowed("/op Steve"));
      assertFalse(
          CommandSecurity.isExecuteAsUserCommandAllowed(
              "/op Steve", CommandPermissionLevel.ADMINS));
    } finally {
      SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS = blockUnsafeNpcCommands;
      SecurityConfig.UNSAFE_NPC_COMMANDS = unsafeNpcCommands;
      allowList.clear();
      allowList.putAll(allowListBackup);
    }
  }
}
