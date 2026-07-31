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

package de.markusbordihn.easynpc.data.state;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class StateActionCommandTest {

  @Test
  void testParseWithValueType() {
    StateActionCommand command = StateActionCommand.parse("set quest text 5");

    assertEquals(StateOperation.SET, command.operation());
    assertEquals("quest", command.stateName());
    assertEquals(StateValueType.TEXT, command.valueType());
    assertEquals("5", command.value());
    assertTrue(command.apply(null).isText());
  }

  @Test
  @DisplayName("A flag value is stored as a number, so it stays comparable")
  void testParseFlag() {
    assertEquals(1, StateActionCommand.parse("set forge_lit flag true").apply(null).numberValue());
    assertEquals(0, StateActionCommand.parse("set forge_lit flag false").apply(null).numberValue());
  }

  @Test
  @DisplayName("A command without a value type keeps guessing it, so older presets still work")
  void testParseWithoutValueType() {
    assertEquals(StateValueType.NUMBER, StateActionCommand.parse("set quest 5").valueType());
    assertEquals(StateValueType.TEXT, StateActionCommand.parse("set quest intro").valueType());
    assertEquals("a longsword", StateActionCommand.parse("set quest a longsword").value());
  }

  @Test
  @DisplayName("A value type is only read for set, where the value would be ambiguous")
  void testValueTypeIsSetOnly() {
    StateActionCommand command = StateActionCommand.parse("increase orders_taken 2");

    assertEquals(StateOperation.INCREASE, command.operation());
    assertEquals("2", command.value());
    assertEquals(2, command.apply(null).numberValue());
  }

  @Test
  void testParseUnknownOperation() {
    assertNull(StateActionCommand.parse("does_not_exist quest 5").operation());
  }

  @Test
  void testToCommand() {
    assertEquals(
        "set quest text intro",
        new StateActionCommand(StateOperation.SET, "quest", StateValueType.TEXT, "intro")
            .toCommand());
    assertEquals(
        "set quest flag true",
        new StateActionCommand(StateOperation.SET, "quest", StateValueType.FLAG, "true")
            .toCommand());
    assertEquals(
        "increase quest 2",
        new StateActionCommand(StateOperation.INCREASE, "quest", StateValueType.NUMBER, "2")
            .toCommand());
    assertEquals(
        "remove quest",
        new StateActionCommand(StateOperation.REMOVE, "quest", StateValueType.NUMBER, "")
            .toCommand());
  }

  @Test
  @DisplayName("A written command is read back as the same command")
  void testCommandRoundTrip() {
    for (String command :
        new String[] {
          "set quest text 5", "set quest number 5", "set quest flag true", "toggle quest"
        }) {
      assertEquals(command, StateActionCommand.parse(command).toCommand());
    }
  }
}
