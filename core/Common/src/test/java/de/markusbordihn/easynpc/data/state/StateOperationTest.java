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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class StateOperationTest {

  @Test
  void testSetNumberAndText() {
    assertEquals(5, StateOperation.SET.apply(null, "5").numberValue());
    assertTrue(StateOperation.SET.apply(null, "5").isNumber());
    assertEquals("intro", StateOperation.SET.apply(null, "intro").textValue());
    assertTrue(StateOperation.SET.apply(null, "intro").isText());
  }

  @Test
  void testIncreaseAndDecrease() {
    assertEquals(1, StateOperation.INCREASE.apply(null, "1").numberValue());
    assertEquals(4, StateOperation.INCREASE.apply(StateEntry.of(3), "1").numberValue());
    assertEquals(2, StateOperation.DECREASE.apply(StateEntry.of(3), "1").numberValue());
    assertEquals(-3, StateOperation.DECREASE.apply(null, "3").numberValue());
  }

  @Test
  @DisplayName("An amount that is not a number counts by one instead of failing")
  void testUnreadableAmount() {
    assertEquals(1, StateOperation.INCREASE.apply(null, "many").numberValue());
  }

  @Test
  @DisplayName("Counting stays inside the number range instead of wrapping around")
  void testCountingIsSaturated() {
    assertEquals(
        Integer.MAX_VALUE,
        StateOperation.INCREASE.apply(StateEntry.of(Integer.MAX_VALUE), "5").numberValue());
    assertEquals(
        Integer.MIN_VALUE,
        StateOperation.DECREASE.apply(StateEntry.of(Integer.MIN_VALUE), "5").numberValue());
  }

  @Test
  @DisplayName("Counting a text state starts at zero")
  void testCountingATextState() {
    StateEntry stateEntry = StateOperation.INCREASE.apply(StateEntry.of("intro"), "2");

    assertTrue(stateEntry.isNumber());
    assertEquals(2, stateEntry.numberValue());
  }

  @Test
  void testToggle() {
    assertTrue(StateOperation.TOGGLE.apply(null, "").asFlag());
    assertFalse(StateOperation.TOGGLE.apply(StateEntry.of(true), "").asFlag());
    assertTrue(StateOperation.TOGGLE.apply(StateEntry.of(false), "").asFlag());
    assertFalse(StateOperation.TOGGLE.apply(StateEntry.of("intro"), "").asFlag());
  }

  @Test
  @DisplayName("Remove reports no value, so the state is dropped")
  void testRemove() {
    assertNull(StateOperation.REMOVE.apply(StateEntry.of(3), ""));
  }

  @Test
  void testRequiresValue() {
    assertTrue(StateOperation.SET.requiresValue());
    assertTrue(StateOperation.INCREASE.requiresValue());
    assertTrue(StateOperation.DECREASE.requiresValue());
    assertFalse(StateOperation.TOGGLE.requiresValue());
    assertFalse(StateOperation.REMOVE.requiresValue());
  }

  @Test
  void testUnknownOperationFallsBackToSet() {
    assertEquals(StateOperation.SET, StateOperation.get("does_not_exist"));
    assertEquals(StateOperation.INCREASE, StateOperation.get("increase"));
    assertEquals("increase", StateOperation.INCREASE.getCommandName());
  }

  @Test
  @DisplayName("A misspelled operation is reported instead of silently setting the state")
  void testUnknownOperationIsNotGuessed() {
    assertNull(StateOperation.find("does_not_exist"));
    assertNull(StateOperation.find(""));
    assertNull(StateOperation.find(null));
    assertEquals(StateOperation.INCREASE, StateOperation.find("Increase"));
  }
}
