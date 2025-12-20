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

@DisplayName("ConditionOperationType Tests")
class ConditionOperationTypeTest {

  @Test
  @DisplayName("Should evaluate EQUALS correctly")
  void testEquals() {
    assertTrue(ConditionOperationType.EQUALS.evaluate(5, 5));
    assertTrue(ConditionOperationType.EQUALS.evaluate(0, 0));
    assertTrue(ConditionOperationType.EQUALS.evaluate(-1, -1));
    assertFalse(ConditionOperationType.EQUALS.evaluate(5, 3));
    assertFalse(ConditionOperationType.EQUALS.evaluate(0, 1));
  }

  @Test
  @DisplayName("Should evaluate NOT_EQUALS correctly")
  void testNotEquals() {
    assertTrue(ConditionOperationType.NOT_EQUALS.evaluate(5, 3));
    assertTrue(ConditionOperationType.NOT_EQUALS.evaluate(0, 1));
    assertTrue(ConditionOperationType.NOT_EQUALS.evaluate(-1, 0));
    assertFalse(ConditionOperationType.NOT_EQUALS.evaluate(5, 5));
    assertFalse(ConditionOperationType.NOT_EQUALS.evaluate(0, 0));
  }

  @Test
  @DisplayName("Should evaluate GREATER_THAN correctly")
  void testGreaterThan() {
    assertTrue(ConditionOperationType.GREATER_THAN.evaluate(10, 5));
    assertTrue(ConditionOperationType.GREATER_THAN.evaluate(1, 0));
    assertTrue(ConditionOperationType.GREATER_THAN.evaluate(0, -1));
    assertFalse(ConditionOperationType.GREATER_THAN.evaluate(5, 5));
    assertFalse(ConditionOperationType.GREATER_THAN.evaluate(5, 10));
  }

  @Test
  @DisplayName("Should evaluate GREATER_THAN_OR_EQUALS correctly")
  void testGreaterThanOrEquals() {
    assertTrue(ConditionOperationType.GREATER_THAN_OR_EQUALS.evaluate(10, 5));
    assertTrue(ConditionOperationType.GREATER_THAN_OR_EQUALS.evaluate(5, 5));
    assertTrue(ConditionOperationType.GREATER_THAN_OR_EQUALS.evaluate(0, -1));
    assertFalse(ConditionOperationType.GREATER_THAN_OR_EQUALS.evaluate(5, 10));
    assertFalse(ConditionOperationType.GREATER_THAN_OR_EQUALS.evaluate(-1, 0));
  }

  @Test
  @DisplayName("Should evaluate LESS_THAN correctly")
  void testLessThan() {
    assertTrue(ConditionOperationType.LESS_THAN.evaluate(5, 10));
    assertTrue(ConditionOperationType.LESS_THAN.evaluate(0, 1));
    assertTrue(ConditionOperationType.LESS_THAN.evaluate(-1, 0));
    assertFalse(ConditionOperationType.LESS_THAN.evaluate(5, 5));
    assertFalse(ConditionOperationType.LESS_THAN.evaluate(10, 5));
  }

  @Test
  @DisplayName("Should evaluate LESS_THAN_OR_EQUALS correctly")
  void testLessThanOrEquals() {
    assertTrue(ConditionOperationType.LESS_THAN_OR_EQUALS.evaluate(5, 10));
    assertTrue(ConditionOperationType.LESS_THAN_OR_EQUALS.evaluate(5, 5));
    assertTrue(ConditionOperationType.LESS_THAN_OR_EQUALS.evaluate(-1, 0));
    assertFalse(ConditionOperationType.LESS_THAN_OR_EQUALS.evaluate(10, 5));
    assertFalse(ConditionOperationType.LESS_THAN_OR_EQUALS.evaluate(0, -1));
  }

  @Test
  @DisplayName("Should evaluate NONE as false")
  void testNone() {
    assertFalse(ConditionOperationType.NONE.evaluate(5, 5));
    assertFalse(ConditionOperationType.NONE.evaluate(0, 0));
    assertFalse(ConditionOperationType.NONE.evaluate(10, 5));
  }

  @Test
  @DisplayName("Should handle non-existent scoreboard value (-1)")
  void testNonExistentScoreboardValue() {
    assertTrue(ConditionOperationType.EQUALS.evaluate(-1, -1));
    assertTrue(ConditionOperationType.GREATER_THAN.evaluate(0, -1));
    assertTrue(ConditionOperationType.GREATER_THAN_OR_EQUALS.evaluate(0, -1));
    assertFalse(ConditionOperationType.GREATER_THAN.evaluate(-1, 0));
  }

  @Test
  @DisplayName("Should return correct symbols")
  void testGetSymbol() {
    assertEquals("==", ConditionOperationType.EQUALS.getSymbol());
    assertEquals("!=", ConditionOperationType.NOT_EQUALS.getSymbol());
    assertEquals(">", ConditionOperationType.GREATER_THAN.getSymbol());
    assertEquals(">=", ConditionOperationType.GREATER_THAN_OR_EQUALS.getSymbol());
    assertEquals("<", ConditionOperationType.LESS_THAN.getSymbol());
    assertEquals("<=", ConditionOperationType.LESS_THAN_OR_EQUALS.getSymbol());
    assertEquals("", ConditionOperationType.NONE.getSymbol());
  }

  @Test
  @DisplayName("Should get ConditionOperationType from string")
  void testGet() {
    assertEquals(ConditionOperationType.EQUALS, ConditionOperationType.get("EQUALS"));
    assertEquals(ConditionOperationType.NOT_EQUALS, ConditionOperationType.get("NOT_EQUALS"));
    assertEquals(ConditionOperationType.GREATER_THAN, ConditionOperationType.get("GREATER_THAN"));
    assertEquals(ConditionOperationType.NONE, ConditionOperationType.get("NONE"));
  }

  @Test
  @DisplayName("Should return NONE for invalid string")
  void testGetInvalid() {
    assertEquals(ConditionOperationType.NONE, ConditionOperationType.get("INVALID"));
    assertEquals(ConditionOperationType.NONE, ConditionOperationType.get(""));
    assertEquals(ConditionOperationType.NONE, ConditionOperationType.get(null));
  }
}
