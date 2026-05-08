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

import net.minecraft.commands.Commands;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("CommandPermissionLevel Tests")
class CommandPermissionLevelTest {

  @Test
  void testMinecraftLevelMapping() {
    assertEquals(CommandPermissionLevel.ALL, CommandPermissionLevel.fromMinecraftLevel(-1));
    assertEquals(
        CommandPermissionLevel.ALL, CommandPermissionLevel.fromMinecraftLevel(Commands.LEVEL_ALL));
    assertEquals(
        CommandPermissionLevel.GAMEMASTERS,
        CommandPermissionLevel.fromMinecraftLevel(Commands.LEVEL_GAMEMASTERS));
    assertEquals(
        CommandPermissionLevel.OWNERS,
        CommandPermissionLevel.fromMinecraftLevel(Commands.LEVEL_OWNERS));
  }

  @Test
  @DisplayName("Should parse enum names and legacy numeric values")
  void testParse() {
    assertEquals(
        CommandPermissionLevel.ADMINS,
        CommandPermissionLevel.parse("ADMINS", CommandPermissionLevel.ALL));
    assertEquals(
        CommandPermissionLevel.GAMEMASTERS,
        CommandPermissionLevel.parse(
            Integer.toString(Commands.LEVEL_GAMEMASTERS), CommandPermissionLevel.ALL));
    assertEquals(
        CommandPermissionLevel.ALL,
        CommandPermissionLevel.parse("unknown", CommandPermissionLevel.ALL));
  }

  @Test
  void testMinMaxAndAllows() {
    assertTrue(CommandPermissionLevel.ADMINS.allows(CommandPermissionLevel.GAMEMASTERS));
    assertFalse(CommandPermissionLevel.ALL.allows(CommandPermissionLevel.GAMEMASTERS));
    assertEquals(
        CommandPermissionLevel.ALL,
        CommandPermissionLevel.min(CommandPermissionLevel.ALL, CommandPermissionLevel.OWNERS));
    assertEquals(
        CommandPermissionLevel.OWNERS,
        CommandPermissionLevel.max(CommandPermissionLevel.ALL, CommandPermissionLevel.OWNERS));
  }
}
