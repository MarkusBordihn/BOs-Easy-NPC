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
import org.junit.jupiter.api.Test;

class TimeOfDayConditionTest {

  @Test
  void evaluateReturnsFalseForNullEntry() {
    assertFalse(TimeOfDayCondition.evaluate(null, null));
  }

  @Test
  void evaluateReturnsFalseForNullLevel() {
    ConditionDataEntry entry =
        new ConditionDataEntry(
            ConditionType.TIME_OF_DAY, ConditionOperationType.GREATER_THAN_OR_EQUALS, "", 13000);
    assertFalse(TimeOfDayCondition.evaluate(entry, null));
  }

  @Test
  void evaluateReturnsFalseForNullOperationType() {
    ConditionDataEntry entry = new ConditionDataEntry(ConditionType.TIME_OF_DAY, null, "", 13000);
    assertFalse(TimeOfDayCondition.evaluate(entry, null));
  }

  @Test
  void matchesComparesAgainstThreshold() {
    assertTrue(
        TimeOfDayCondition.matches(ConditionOperationType.GREATER_THAN_OR_EQUALS, 13000, 13000));
    assertFalse(
        TimeOfDayCondition.matches(ConditionOperationType.GREATER_THAN_OR_EQUALS, 13000, 6000));
    assertTrue(TimeOfDayCondition.matches(ConditionOperationType.LESS_THAN, 6000, 1000));
    assertTrue(TimeOfDayCondition.matches(ConditionOperationType.EQUALS, 6000, 6000));
  }

  @Test
  void matchesWrapsWorldTimeIntoSingleDay() {
    assertTrue(
        TimeOfDayCondition.matches(ConditionOperationType.GREATER_THAN_OR_EQUALS, 1000, 25000));
    assertTrue(TimeOfDayCondition.matches(ConditionOperationType.EQUALS, 0, 48000));
  }

  @Test
  void matchesReturnsFalseForNoneOperation() {
    assertFalse(TimeOfDayCondition.matches(ConditionOperationType.NONE, 6000, 6000));
  }
}
