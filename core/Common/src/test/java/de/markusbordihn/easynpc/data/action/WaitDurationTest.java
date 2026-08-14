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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class WaitDurationTest {

  @Test
  @DisplayName("A bare number, seconds and ticks all resolve to the same duration")
  void testSecondsAndTicks() {
    assertEquals(400, WaitDuration.parse("20").ticks());
    assertEquals(400, WaitDuration.parse("20s").ticks());
    assertEquals(400, WaitDuration.parse("400t").ticks());
    assertEquals(6000, WaitDuration.parse("5m").ticks());
    assertEquals(30, WaitDuration.parse("1.5s").ticks());
  }

  @Test
  @DisplayName("Spaces, upper case and a unit without a number are handled")
  void testLenientParsing() {
    assertEquals(400, WaitDuration.parse(" 20S ").ticks());
    assertEquals(6000, WaitDuration.parse("5 M").ticks());
    assertFalse(WaitDuration.parse("s").isValid());
  }

  @Test
  @DisplayName("An empty, missing or malformed duration is invalid")
  void testInvalidDuration() {
    assertFalse(WaitDuration.parse(null).isValid());
    assertFalse(WaitDuration.parse("").isValid());
    assertFalse(WaitDuration.parse("abc").isValid());
    assertFalse(WaitDuration.parse("-5s").isValid());
    assertFalse(WaitDuration.parse("0s").isValid());
    assertFalse(WaitDuration.parse("20h").isValid());
    assertEquals(WaitDuration.INVALID_TICKS, WaitDuration.parseUnclampedTicks("abc"));
  }

  @Test
  @DisplayName("A duration above the limit is shortened instead of dropped")
  void testDurationLimit() {
    WaitDuration waitDuration = WaitDuration.parse("999m");
    assertTrue(waitDuration.isValid());
    assertEquals(WaitDuration.MAX_TICKS, waitDuration.ticks());
    assertTrue(WaitDuration.parseUnclampedTicks("999m") > WaitDuration.MAX_TICKS);
  }
}
