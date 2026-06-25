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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogButtonConditionModeTest {

  @Test
  @DisplayName("Should parse exact enum names")
  void testParseExactNames() {
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get("LOCK"));
    assertEquals(DialogButtonConditionMode.HIDE, DialogButtonConditionMode.get("HIDE"));
  }

  @Test
  @DisplayName("Should parse case-insensitively and trim surrounding whitespace")
  void testParseCaseInsensitiveAndTrimmed() {
    assertEquals(DialogButtonConditionMode.HIDE, DialogButtonConditionMode.get("hide"));
    assertEquals(DialogButtonConditionMode.HIDE, DialogButtonConditionMode.get("Hide"));
    assertEquals(DialogButtonConditionMode.HIDE, DialogButtonConditionMode.get("  HIDE  "));
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get(" lock "));
  }

  @Test
  @DisplayName("Should fall back to LOCK for null, blank or unknown values")
  void testFallbackToLock() {
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get(null));
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get(""));
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get("   "));
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get("unknown"));
    assertEquals(DialogButtonConditionMode.LOCK, DialogButtonConditionMode.get("HIDDEN"));
  }

  @Test
  @DisplayName("Should round-trip every value through name()")
  void testNameRoundTrip() {
    for (DialogButtonConditionMode mode : DialogButtonConditionMode.values()) {
      assertEquals(mode, DialogButtonConditionMode.get(mode.name()));
    }
  }
}
