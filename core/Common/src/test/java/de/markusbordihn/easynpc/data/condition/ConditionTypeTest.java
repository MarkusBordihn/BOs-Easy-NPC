/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.data.condition;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ConditionType Tests")
class ConditionTypeTest {

  @Test
  @DisplayName("Should get ConditionType from string")
  void testGet() {
    assertEquals(ConditionType.SCOREBOARD, ConditionType.get("SCOREBOARD"));
    assertEquals(ConditionType.EXECUTION_LIMIT, ConditionType.get("EXECUTION_LIMIT"));
    assertEquals(ConditionType.NONE, ConditionType.get("NONE"));
  }

  @Test
  @DisplayName("Should return NONE for invalid string")
  void testGetInvalid() {
    assertEquals(ConditionType.NONE, ConditionType.get("INVALID"));
    assertEquals(ConditionType.NONE, ConditionType.get(""));
    assertEquals(ConditionType.NONE, ConditionType.get(null));
  }

  @Test
  @DisplayName("Should check if name is required")
  void testRequiresName() {
    assertTrue(ConditionType.SCOREBOARD.requiresName());
    assertFalse(ConditionType.EXECUTION_LIMIT.requiresName());
    assertFalse(ConditionType.NONE.requiresName());
  }

  @Test
  @DisplayName("Should check if value is required")
  void testRequiresValue() {
    assertTrue(ConditionType.SCOREBOARD.requiresValue());
    assertTrue(ConditionType.EXECUTION_LIMIT.requiresValue());
    assertFalse(ConditionType.NONE.requiresValue());
  }

  @Test
  @DisplayName("Should check if operation is required")
  void testRequiresOperation() {
    assertTrue(ConditionType.SCOREBOARD.requiresOperation());
    assertFalse(ConditionType.EXECUTION_LIMIT.requiresOperation());
    assertFalse(ConditionType.NONE.requiresOperation());
  }
}
