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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("UnsafeNpcCommand Tests")
class UnsafeNpcCommandTest {

  @Test
  @DisplayName("Should block core server-management commands")
  void testCoreBlocklist() {
    assertTrue(UnsafeNpcCommand.matches("stop"));
    assertTrue(UnsafeNpcCommand.matches("op Steve"));
    assertTrue(UnsafeNpcCommand.matches("deop Steve"));
    assertTrue(UnsafeNpcCommand.matches("ban Steve"));
    assertTrue(UnsafeNpcCommand.matches("ban-ip 1.2.3.4"));
    assertTrue(UnsafeNpcCommand.matches("kick Steve"));
    assertTrue(UnsafeNpcCommand.matches("whitelist add Steve"));
    assertTrue(UnsafeNpcCommand.matches("reload"));
  }

  @Test
  @DisplayName("Should block newly added commands: function, schedule, spawnpoint, spreadplayers")
  void testNewBlocklist() {
    assertTrue(UnsafeNpcCommand.matches("function mypack:some_function"));
    assertTrue(UnsafeNpcCommand.matches("schedule function mypack:foo 5t"));
    assertTrue(UnsafeNpcCommand.matches("spawnpoint @a ~ ~ ~"));
    assertTrue(UnsafeNpcCommand.matches("spreadplayers ~ ~ 10 20 false @a"));
  }

  @Test
  void testLeadingSlash() {
    assertTrue(UnsafeNpcCommand.matches("/stop"));
    assertTrue(UnsafeNpcCommand.matches("/function mypack:foo"));
    assertTrue(UnsafeNpcCommand.matches("/schedule function mypack:foo 1t"));
    assertTrue(UnsafeNpcCommand.matches("/spawnpoint"));
  }

  @Test
  @DisplayName("Should block unsafe command hidden after execute run")
  void testExecuteRunBypass() {
    assertTrue(UnsafeNpcCommand.matches("execute as @a run stop"));
    assertTrue(UnsafeNpcCommand.matches("execute at @s run op Steve"));
    assertTrue(UnsafeNpcCommand.matches("execute as @a run function mypack:evil"));
    assertTrue(UnsafeNpcCommand.matches("execute as @a run schedule function mypack:evil 1t"));
    assertTrue(UnsafeNpcCommand.matches("execute as @a at @s run spawnpoint @a ~ ~ ~"));
  }

  @Test
  @DisplayName("Should block unsafe command in chained execute run run")
  void testChainedExecuteRun() {
    assertTrue(UnsafeNpcCommand.matches("execute as @a run execute as @e run stop"));
    assertTrue(UnsafeNpcCommand.matches("execute as @a run execute as @e run function mypack:x"));
  }

  @Test
  void testSafeCommands() {
    assertFalse(UnsafeNpcCommand.matches("say Hello World"));
    assertFalse(UnsafeNpcCommand.matches("give @p diamond 1"));
    assertFalse(UnsafeNpcCommand.matches("tp @s ~ ~1 ~"));
    assertFalse(UnsafeNpcCommand.matches("effect give @s speed 10 1"));
    assertFalse(UnsafeNpcCommand.matches("execute as @a run say Hi"));
    assertFalse(UnsafeNpcCommand.matches("scoreboard players set @s kills 0"));
  }

  @Test
  @DisplayName("Should not block on null, blank, or empty input")
  void testNullAndEmpty() {
    assertFalse(UnsafeNpcCommand.matches(null));
    assertFalse(UnsafeNpcCommand.matches(""));
    assertFalse(UnsafeNpcCommand.matches("   "));
    assertFalse(UnsafeNpcCommand.matches("/"));
  }

  @Test
  void testCaseInsensitive() {
    assertTrue(UnsafeNpcCommand.matches("STOP"));
    assertTrue(UnsafeNpcCommand.matches("Stop"));
    assertTrue(UnsafeNpcCommand.matches("FUNCTION mypack:foo"));
    assertTrue(UnsafeNpcCommand.matches("execute as @a run STOP"));
  }
}
