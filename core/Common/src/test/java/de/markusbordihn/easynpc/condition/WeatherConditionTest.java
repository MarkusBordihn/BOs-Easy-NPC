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
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.WeatherType;
import org.junit.jupiter.api.Test;

class WeatherConditionTest {

  @Test
  void evaluateReturnsFalseForNullEntry() {
    assertFalse(WeatherCondition.evaluate(null, null));
  }

  @Test
  void evaluateReturnsFalseForNullLevel() {
    ConditionDataEntry entry =
        new ConditionDataEntry(ConditionType.WEATHER).withSubType(WeatherType.RAIN);
    assertFalse(WeatherCondition.evaluate(entry, null));
  }

  @Test
  void matchesClearOnlyWhenNotRaining() {
    assertTrue(WeatherCondition.matches(WeatherType.CLEAR, false, false));
    assertFalse(WeatherCondition.matches(WeatherType.CLEAR, true, false));
    assertFalse(WeatherCondition.matches(WeatherType.CLEAR, true, true));
  }

  @Test
  void matchesRainOnlyWhenRainingWithoutThunder() {
    assertTrue(WeatherCondition.matches(WeatherType.RAIN, true, false));
    assertFalse(WeatherCondition.matches(WeatherType.RAIN, false, false));
    assertFalse(WeatherCondition.matches(WeatherType.RAIN, true, true));
  }

  @Test
  void matchesThunderWheneverThundering() {
    assertTrue(WeatherCondition.matches(WeatherType.THUNDER, true, true));
    assertTrue(WeatherCondition.matches(WeatherType.THUNDER, false, true));
    assertFalse(WeatherCondition.matches(WeatherType.THUNDER, true, false));
    assertFalse(WeatherCondition.matches(WeatherType.THUNDER, false, false));
  }
}
