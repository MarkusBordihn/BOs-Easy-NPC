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

package de.markusbordihn.easynpc.condition;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.data.state.StateValueType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class NpcStateConditionTest {

  private static ConditionDataEntry numberCondition(
      ConditionOperationType operationType, int value) {
    return new ConditionDataEntry(ConditionType.NPC_STATE, operationType, "easy_npc:quest", value);
  }

  private static ConditionDataEntry textCondition(
      ConditionOperationType operationType, String textValue) {
    return numberCondition(operationType, 0).withCustomData(textValue);
  }

  private static ConditionDataEntry flagCondition(
      ConditionOperationType operationType, boolean flagValue) {
    return numberCondition(operationType, flagValue ? 1 : 0).withSubType(StateValueType.FLAG);
  }

  @Test
  void testNumberOperations() {
    assertTrue(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EQUALS, 3), StateEntry.of(3)));
    assertFalse(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EQUALS, 3), StateEntry.of(2)));
    assertTrue(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.NOT_EQUALS, 3), StateEntry.of(2)));
    assertFalse(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.NOT_EQUALS, 3), StateEntry.of(3)));
  }

  @Test
  @DisplayName("A state that was never set counts as zero")
  void testMissingStateIsZero() {
    assertTrue(NpcStateCondition.matches(numberCondition(ConditionOperationType.EQUALS, 0), null));
    assertFalse(NpcStateCondition.matches(numberCondition(ConditionOperationType.EQUALS, 1), null));
  }

  @Test
  @DisplayName("A text state is not compared as a number")
  void testTextStateAgainstNumberCondition() {
    assertTrue(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EQUALS, 0), StateEntry.of("intro")));
    assertFalse(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EQUALS, 1), StateEntry.of("intro")));
  }

  @Test
  void testTextOperations() {
    assertTrue(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.EQUALS, "intro"), StateEntry.of("intro")));
    assertFalse(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.EQUALS, "intro"), StateEntry.of("outro")));
    assertTrue(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.NOT_EQUALS, "intro"), StateEntry.of("outro")));
    assertFalse(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.NOT_EQUALS, "intro"), StateEntry.of("intro")));
  }

  @Test
  @DisplayName("A text condition ignores surrounding whitespace of the expected value")
  void testTextConditionIsTrimmed() {
    assertTrue(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.EQUALS, "  intro  "), StateEntry.of("intro")));
  }

  @Test
  @DisplayName("A missing or numeric state never matches an expected text")
  void testTextConditionAgainstOtherStates() {
    assertFalse(
        NpcStateCondition.matches(textCondition(ConditionOperationType.EQUALS, "intro"), null));
    assertFalse(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.EQUALS, "intro"), StateEntry.of(3)));
    assertTrue(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.NOT_EQUALS, "intro"), StateEntry.of(3)));
  }

  @Test
  @DisplayName("A flag condition reads any set state as true, not only a number")
  void testFlagOperations() {
    assertTrue(
        NpcStateCondition.matches(
            flagCondition(ConditionOperationType.EQUALS, true), StateEntry.of(1)));
    assertTrue(
        NpcStateCondition.matches(
            flagCondition(ConditionOperationType.EQUALS, true), StateEntry.of("intro")));
    assertTrue(
        NpcStateCondition.matches(flagCondition(ConditionOperationType.EQUALS, false), null));
    assertFalse(
        NpcStateCondition.matches(
            flagCondition(ConditionOperationType.EQUALS, true), StateEntry.of(0)));
    assertTrue(
        NpcStateCondition.matches(
            flagCondition(ConditionOperationType.NOT_EQUALS, true), StateEntry.of(0)));
  }

  @Test
  @DisplayName("A stored value type wins over guessing it from the compared value")
  void testStoredValueType() {
    assertTrue(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EQUALS, 0)
                .withSubType(StateValueType.TEXT)
                .withCustomData(""),
            null));
    assertFalse(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EQUALS, 0)
                .withSubType(StateValueType.TEXT)
                .withCustomData(""),
            StateEntry.of("intro")));
  }

  @Test
  @DisplayName("A state set to zero exists, a state that was never set does not")
  void testExistenceOperations() {
    assertTrue(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.EXISTS, 0), StateEntry.of(0)));
    assertFalse(NpcStateCondition.matches(numberCondition(ConditionOperationType.EXISTS, 0), null));
    assertTrue(
        NpcStateCondition.matches(numberCondition(ConditionOperationType.NOT_EXISTS, 0), null));
    assertFalse(
        NpcStateCondition.matches(
            numberCondition(ConditionOperationType.NOT_EXISTS, 0), StateEntry.of(0)));
  }

  @Test
  @DisplayName("The existence operations ignore the value type and the compared value")
  void testExistenceOperationsIgnoreValue() {
    assertTrue(
        NpcStateCondition.matches(
            textCondition(ConditionOperationType.EXISTS, "intro"), StateEntry.of("outro")));
    assertTrue(
        NpcStateCondition.matches(
            flagCondition(ConditionOperationType.EXISTS, true), StateEntry.of(0)));
    assertTrue(
        NpcStateCondition.matches(flagCondition(ConditionOperationType.NOT_EXISTS, true), null));
  }

  @Test
  void testEvaluateWithoutNpcContext() {
    assertFalse(
        NpcStateCondition.evaluate(numberCondition(ConditionOperationType.EQUALS, 0), null));
  }
}
