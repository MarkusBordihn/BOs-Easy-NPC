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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import net.minecraft.network.chat.Component;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

class TextFormattingCodesTest {

  @Test
  void testParseTextCodes_keepsComponentWithoutCodes() {
    Component component = Component.literal("plain text");
    assertSame(component, TextFormattingCodes.parseTextCodes(component));
  }

  @Test
  void testParseTextCodes_parsesColorTagsAndLineBreaks() {
    Component component = Component.literal("<gold>Gold</gold><br>Next");
    assertEquals("§6Gold§0\nNext", TextFormattingCodes.parseTextCodes(component).getString());
  }

  @Test
  void testParseTextCodes_nullComponent() {
    assertNull(TextFormattingCodes.parseTextCodes(null));
  }

  @Test
  void testHasTextFormattingCodes_withColorTag() {
    assertTrue(TextFormattingCodes.hasTextFormattingCodes("<red>hello</red>"));
    assertTrue(TextFormattingCodes.hasTextFormattingCodes("<bold>text</bold>"));
  }

  @Test
  void testHasTextFormattingCodes_withoutTags() {
    assertFalse(TextFormattingCodes.hasTextFormattingCodes("plain text"));
    assertFalse(TextFormattingCodes.hasTextFormattingCodes("no tags here"));
  }

  @ParameterizedTest
  @NullAndEmptySource
  void testHasTextFormattingCodes_nullOrEmpty(String input) {
    assertFalse(TextFormattingCodes.hasTextFormattingCodes(input));
  }

  @Test
  void testParseTextFormattingCodes_colorTags() {
    assertEquals("§cHello§0", TextFormattingCodes.parseTextFormattingCodes("<red>Hello</red>"));
    assertEquals("§aGreen§0", TextFormattingCodes.parseTextFormattingCodes("<green>Green</green>"));
    assertEquals("§6Gold§0", TextFormattingCodes.parseTextFormattingCodes("<gold>Gold</gold>"));
  }

  @Test
  void testParseTextFormattingCodes_formattingTags() {
    assertEquals("§lBold§r", TextFormattingCodes.parseTextFormattingCodes("<bold>Bold</bold>"));
    assertEquals("§oBold§r", TextFormattingCodes.parseTextFormattingCodes("<italic>Bold</italic>"));
    assertEquals(
        "§nUnder§r", TextFormattingCodes.parseTextFormattingCodes("<underline>Under</underline>"));
  }

  @Test
  void testParseTextFormattingCodes_shortCodes() {
    assertEquals("§lBold§r", TextFormattingCodes.parseTextFormattingCodes("<b>Bold</b>"));
    assertEquals("§oItalic§r", TextFormattingCodes.parseTextFormattingCodes("<i>Italic</i>"));
  }

  @Test
  void testParseTextFormattingCodes_noTags_returnsUnchanged() {
    assertEquals("plain text", TextFormattingCodes.parseTextFormattingCodes("plain text"));
  }

  @Test
  void testParseTextFormattingCodes_null_returnsNull() {
    assertNull(TextFormattingCodes.parseTextFormattingCodes(null));
  }

  @Test
  void testHasTextLinebreakCodes_withBrTag() {
    assertTrue(TextFormattingCodes.hasTextLinebreakCodes("Line one<br>Line two"));
  }

  @Test
  void testHasTextLinebreakCodes_withBackslashN() {
    assertTrue(TextFormattingCodes.hasTextLinebreakCodes("Line one\\nLine two"));
  }

  @Test
  void testHasTextLinebreakCodes_withoutBreaks() {
    assertFalse(TextFormattingCodes.hasTextLinebreakCodes("no breaks here"));
  }

  @ParameterizedTest
  @NullAndEmptySource
  void testHasTextLinebreakCodes_nullOrEmpty(String input) {
    assertFalse(TextFormattingCodes.hasTextLinebreakCodes(input));
  }

  @Test
  void testParseTextLineBreaks_brTag() {
    assertEquals(
        "Line one\nLine two", TextFormattingCodes.parseTextLineBreaks("Line one<br>Line two"));
  }

  @Test
  void testParseTextLineBreaks_backslashN() {
    assertEquals(
        "Line one\nLine two", TextFormattingCodes.parseTextLineBreaks("Line one\\nLine two"));
  }

  @Test
  void testParseTextLineBreaks_multiple() {
    assertEquals("A\nB\nC", TextFormattingCodes.parseTextLineBreaks("A<br>B<br>C"));
  }

  @Test
  void testParseTextLineBreaks_noBreaks_returnsUnchanged() {
    assertEquals("no breaks", TextFormattingCodes.parseTextLineBreaks("no breaks"));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "plain"})
  void testParseTextLineBreaks_noBreakCodes_returnsUnchanged(String input) {
    assertEquals(input, TextFormattingCodes.parseTextLineBreaks(input));
  }
}
