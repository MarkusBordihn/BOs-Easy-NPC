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

package de.markusbordihn.easynpc.configui.client.screen.components;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ColorUtilsTest {

  @Test
  @DisplayName("Should format RGB values as zero-padded six digit hex strings")
  void testFormatRgbColor() {
    assertEquals("#FF0000", ColorUtils.formatRgbColor(0xFF0000));
    assertEquals("#00FF00", ColorUtils.formatRgbColor(0x00FF00));
    assertEquals("#000000", ColorUtils.formatRgbColor(0x000000));
    assertEquals("#FFFFFF", ColorUtils.formatRgbColor(0xFFFFFF));
  }

  @Test
  @DisplayName("Should mask alpha or out-of-range bits when formatting")
  void testFormatMasksHigherBits() {
    assertEquals("#123456", ColorUtils.formatRgbColor(0xFF123456));
    assertEquals("#FFFFFF", ColorUtils.formatRgbColor(-1));
  }

  @Test
  @DisplayName("Should parse hex strings with and without a leading hash")
  void testParseRgbColor() {
    assertEquals(0xFF0000, ColorUtils.parseRgbColor("#FF0000").intValue());
    assertEquals(0xFF0000, ColorUtils.parseRgbColor("FF0000").intValue());
    assertEquals(0x000000, ColorUtils.parseRgbColor("#000000").intValue());
    assertEquals(0xFFFFFF, ColorUtils.parseRgbColor("#FFFFFF").intValue());
  }

  @Test
  @DisplayName("Should trim surrounding whitespace and accept lower case")
  void testParseTrimsAndIsCaseInsensitive() {
    assertEquals(0x00FF00, ColorUtils.parseRgbColor("  #00ff00  ").intValue());
    assertEquals(0xABCDEF, ColorUtils.parseRgbColor("abcdef").intValue());
  }

  @Test
  @DisplayName("Should return null for null, malformed or wrong-length input")
  void testParseRejectsInvalidInput() {
    assertNull(ColorUtils.parseRgbColor(null));
    assertNull(ColorUtils.parseRgbColor(""));
    assertNull(ColorUtils.parseRgbColor("#FFF"));
    assertNull(ColorUtils.parseRgbColor("#1234567"));
    assertNull(ColorUtils.parseRgbColor("#GGGGGG"));
    assertNull(ColorUtils.parseRgbColor("not-a-color"));
  }

  @Test
  @DisplayName("Should round-trip between format and parse")
  void testFormatParseRoundTrip() {
    int[] colors = {0x000000, 0x123456, 0xABCDEF, 0xFFFFFF};
    for (int color : colors) {
      assertEquals(color, ColorUtils.parseRgbColor(ColorUtils.formatRgbColor(color)).intValue());
    }
  }
}
