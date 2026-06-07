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

import org.junit.jupiter.api.Test;

class DurationTypeTest {

  @Test
  void testGet() {
    assertEquals(DurationType.PER_MINUTE, DurationType.get("PER_MINUTE"));
    assertEquals(DurationType.PER_HOUR, DurationType.get("PER_HOUR"));
    assertEquals(DurationType.PER_DAY, DurationType.get("PER_DAY"));
    assertEquals(DurationType.PER_WEEK, DurationType.get("PER_WEEK"));
    assertEquals(DurationType.PER_MONTH, DurationType.get("PER_MONTH"));
    assertEquals(DurationType.LIFETIME, DurationType.get("LIFETIME"));
  }

  @Test
  void testGetFallback() {
    assertEquals(DurationType.PER_DAY, DurationType.get("INVALID"));
    assertEquals(DurationType.PER_DAY, DurationType.get(""));
    assertEquals(DurationType.PER_DAY, DurationType.get(null));
  }

  @Test
  void testImplementsConditionSubTypeEntry() {
    assertTrue(DurationType.PER_DAY instanceof ConditionSubTypeEntry);
    assertTrue(DurationType.LIFETIME instanceof ConditionSubTypeEntry);
  }
}
