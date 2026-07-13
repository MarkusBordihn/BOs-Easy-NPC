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

package de.markusbordihn.easynpc.config;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.EnumMap;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SecurityConfigTest {

  @TempDir Path tempDirectory;

  @SuppressWarnings("unchecked")
  private static EnumMap<CommandPermissionLevel, Set<String>> getExecuteAsUserAllowList()
      throws Exception {
    Field field = SecurityConfig.class.getDeclaredField("EXECUTE_AS_USER_COMMAND_ALLOW_LIST");
    field.setAccessible(true);
    return (EnumMap<CommandPermissionLevel, Set<String>>) field.get(null);
  }

  @Test
  void shouldLoadExecuteAsUserAllowListFromConfigFile() throws Exception {
    Field configPathField = Config.class.getDeclaredField("configPath");
    configPathField.setAccessible(true);
    Path originalConfigPath = (Path) configPathField.get(null);

    EnumMap<CommandPermissionLevel, Set<String>> allowList = getExecuteAsUserAllowList();
    EnumMap<CommandPermissionLevel, Set<String>> originalAllowList = new EnumMap<>(allowList);
    boolean originalBlockUnsafeNpcCommands = SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS;

    try {
      configPathField.set(null, this.tempDirectory);
      Files.writeString(
          this.tempDirectory.resolve(SecurityConfig.CONFIG_FILE_NAME),
          """
          blockUnsafeNpcCommands=false
          executeAsUserCommandAllowList.ALL=
          executeAsUserCommandAllowList.MODERATORS=
          executeAsUserCommandAllowList.GAMEMASTERS=*,daycare,tp
          executeAsUserCommandAllowList.ADMINS=
          executeAsUserCommandAllowList.OWNERS=
          """);

      SecurityConfig.parseConfigFile();

      assertFalse(SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS);
      assertTrue(
          SecurityConfig.isExecuteAsUserCommandAllowed(
              "/daycare", CommandPermissionLevel.GAMEMASTERS));
      assertTrue(
          SecurityConfig.isExecuteAsUserCommandAllowed(
              "/tp @s ~1 ~ ~", CommandPermissionLevel.GAMEMASTERS));
      assertFalse(
          SecurityConfig.isExecuteAsUserCommandAllowed(
              "/tp @s ~1 ~ ~", CommandPermissionLevel.MODERATORS));
      assertFalse(
          SecurityConfig.isExecuteAsUserCommandAllowed(
              "/say allowed by wildcard", CommandPermissionLevel.GAMEMASTERS));
    } finally {
      configPathField.set(null, originalConfigPath);
      SecurityConfig.BLOCK_UNSAFE_NPC_COMMANDS = originalBlockUnsafeNpcCommands;
      allowList.clear();
      allowList.putAll(originalAllowList);
    }
  }
}
