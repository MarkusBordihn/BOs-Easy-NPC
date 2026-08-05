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

package de.markusbordihn.easynpc.handler;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class EnvironmentChangeTrackerTest {

  @Test
  @DisplayName("An environment change is visible only during the current base tick window")
  void testChangeWindow() {
    assertTrue(EnvironmentChangeTracker.isWithinChangeWindow(100L, 100L));
    assertTrue(EnvironmentChangeTracker.isWithinChangeWindow(116L, 100L));
    assertFalse(EnvironmentChangeTracker.isWithinChangeWindow(117L, 100L));
  }

  @Test
  @DisplayName("A game clock behind the recorded change never reports a stale transition")
  void testBackwardsGameClock() {
    assertFalse(EnvironmentChangeTracker.isWithinChangeWindow(5L, 100L));
    assertFalse(EnvironmentChangeTracker.isWithinChangeWindow(5L, -1L));
  }
}
