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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ActionDataTypeTest {

  @Test
  void testGet_validNames() {
    assertEquals(ActionDataType.NONE, ActionDataType.get("NONE"));
    assertEquals(ActionDataType.COMMAND, ActionDataType.get("COMMAND"));
    assertEquals(ActionDataType.CLOSE_DIALOG, ActionDataType.get("CLOSE_DIALOG"));
    assertEquals(ActionDataType.INTERACT_BLOCK, ActionDataType.get("INTERACT_BLOCK"));
    assertEquals(ActionDataType.OPEN_TRADING_SCREEN, ActionDataType.get("OPEN_TRADING_SCREEN"));
    assertEquals(ActionDataType.OPEN_DEFAULT_DIALOG, ActionDataType.get("OPEN_DEFAULT_DIALOG"));
    assertEquals(ActionDataType.OPEN_NAMED_DIALOG, ActionDataType.get("OPEN_NAMED_DIALOG"));
    assertEquals(
        ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL,
        ActionDataType.get("OPEN_NAMED_DIALOG_CONDITIONAL"));
    assertEquals(ActionDataType.SCOREBOARD, ActionDataType.get("SCOREBOARD"));
  }

  @Test
  void testGet_null_returnsNone() {
    assertEquals(ActionDataType.NONE, ActionDataType.get(null));
  }

  @Test
  void testGet_empty_returnsNone() {
    assertEquals(ActionDataType.NONE, ActionDataType.get(""));
  }

  @ParameterizedTest
  @ValueSource(strings = {"command", "Command", "INVALID", "open_dialog"})
  void testGet_invalidOrLowercase_returnsNone(String input) {
    assertEquals(ActionDataType.NONE, ActionDataType.get(input));
  }

  @Test
  void testRequiresArgument_typesRequiringArgument() {
    assertTrue(ActionDataType.COMMAND.requiresArgument());
    assertTrue(ActionDataType.INTERACT_BLOCK.requiresArgument());
    assertTrue(ActionDataType.OPEN_NAMED_DIALOG.requiresArgument());
    assertTrue(ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL.requiresArgument());
    assertTrue(ActionDataType.SCOREBOARD.requiresArgument());
    assertTrue(ActionDataType.NONE.requiresArgument());
  }

  @Test
  void testRequiresArgument_typesNotRequiringArgument() {
    assertFalse(ActionDataType.CLOSE_DIALOG.requiresArgument());
    assertFalse(ActionDataType.OPEN_TRADING_SCREEN.requiresArgument());
    assertFalse(ActionDataType.OPEN_DEFAULT_DIALOG.requiresArgument());
  }

  @Test
  void testGetId_namingConvention() {
    assertEquals("actionDataType.none", ActionDataType.NONE.getId());
    assertEquals("actionDataType.command", ActionDataType.COMMAND.getId());
    assertEquals("actionDataType.close_dialog", ActionDataType.CLOSE_DIALOG.getId());
    assertEquals("actionDataType.open_trading_screen", ActionDataType.OPEN_TRADING_SCREEN.getId());
    assertEquals(
        "actionDataType.open_named_dialog_conditional",
        ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL.getId());
    assertEquals("actionDataType.scoreboard", ActionDataType.SCOREBOARD.getId());
  }

  @Test
  void testGetId_allEnumValues_havePrefix() {
    for (ActionDataType type : ActionDataType.values()) {
      assertTrue(type.getId().startsWith("actionDataType."), "Missing prefix for: " + type);
    }
  }
}
