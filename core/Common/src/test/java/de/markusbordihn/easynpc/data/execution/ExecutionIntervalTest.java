/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.data.execution;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class ExecutionIntervalTest {

  @Test
  void testGetWithValidName() {
    assertEquals(ExecutionInterval.PER_DAY, ExecutionInterval.get("PER_DAY"));
    assertEquals(ExecutionInterval.PER_HOUR, ExecutionInterval.get("PER_HOUR"));
    assertEquals(ExecutionInterval.LIFETIME, ExecutionInterval.get("LIFETIME"));
  }

  @Test
  void testHasIntervalPassedForLifetime() {
    assertFalse(ExecutionInterval.LIFETIME.hasIntervalPassed(0));
    assertFalse(ExecutionInterval.LIFETIME.hasIntervalPassed(System.currentTimeMillis()));
  }

  @Test
  void testHasIntervalPassedForPerMinute() {
    long now = System.currentTimeMillis();
    long oneMinuteAgo = now - 60_001L;
    long thirtySecondsAgo = now - 30_000L;

    assertTrue(ExecutionInterval.PER_MINUTE.hasIntervalPassed(oneMinuteAgo));
    assertFalse(ExecutionInterval.PER_MINUTE.hasIntervalPassed(thirtySecondsAgo));
  }

  @Test
  void testHasIntervalPassedForPerDay() {
    long now = System.currentTimeMillis();
    long oneDayAgo = now - 86_400_001L;
    long oneHourAgo = now - 3_600_000L;

    assertTrue(ExecutionInterval.PER_DAY.hasIntervalPassed(oneDayAgo));
    assertFalse(ExecutionInterval.PER_DAY.hasIntervalPassed(oneHourAgo));
  }

  @Test
  void testGetMilliseconds() {
    assertEquals(60_000L, ExecutionInterval.PER_MINUTE.getMilliseconds());
    assertEquals(3_600_000L, ExecutionInterval.PER_HOUR.getMilliseconds());
    assertEquals(86_400_000L, ExecutionInterval.PER_DAY.getMilliseconds());
    assertEquals(604_800_000L, ExecutionInterval.PER_WEEK.getMilliseconds());
    assertEquals(2_592_000_000L, ExecutionInterval.PER_MONTH.getMilliseconds());
    assertEquals(Long.MAX_VALUE, ExecutionInterval.LIFETIME.getMilliseconds());
  }
}
