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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class DialogUtilsTest {

  @Test
  void testHasDialogMacros_withMacros() {
    assertTrue(DialogUtils.hasDialogMacros("Hello @npc"));
    assertTrue(DialogUtils.hasDialogMacros("Hello @initiator"));
    assertTrue(DialogUtils.hasDialogMacros("@npc says hello to @initiator"));
  }

  @Test
  void testHasDialogMacros_withoutMacros() {
    assertFalse(DialogUtils.hasDialogMacros("Hello world"));
    assertFalse(DialogUtils.hasDialogMacros("Simple text"));
    assertFalse(DialogUtils.hasDialogMacros("No macros here"));
  }

  @Test
  void testHasDialogMacros_null() {
    assertFalse(DialogUtils.hasDialogMacros((String) null));
  }

  @Test
  void testHasDialogMacros_empty() {
    assertFalse(DialogUtils.hasDialogMacros(""));
  }

  @Test
  void testParseDialogText_noMacros() {
    String input = "Hello world";
    String result = DialogUtils.parseDialogText(input, null, null);
    assertEquals(input, result);
  }

  @Test
  void testParseDialogText_nullEntities() {
    String input = "Hello @npc and @initiator";
    String result = DialogUtils.parseDialogText(input, null, null);
    assertEquals(input, result);
  }

  @Test
  void testGenerateButtonLabel_withName() {
    assertEquals("abc", DialogUtils.generateButtonLabel("abc"));
    assertEquals("test123", DialogUtils.generateButtonLabel("test123"));
    assertEquals("my_button", DialogUtils.generateButtonLabel("My Button"));
    assertEquals("action", DialogUtils.generateButtonLabel("action"));
  }

  @Test
  void testGenerateButtonLabel_nullName() {
    String result = DialogUtils.generateButtonLabel(null);
    assertTrue(result.startsWith("button_"));
    assertTrue(result.length() > 7);
  }

  @Test
  void testGenerateButtonLabel_emptyName() {
    String result = DialogUtils.generateButtonLabel("");
    assertTrue(result.startsWith("button_"));
    assertTrue(result.length() > 7);
  }

  @Test
  void testGenerateDialogLabel_nullName() {
    String result = DialogUtils.generateDialogLabel(null);
    assertTrue(result.startsWith("dialog_"));
    assertTrue(result.length() > 7);
  }

  @Test
  void testGenerateDialogLabel_emptyName() {
    String result = DialogUtils.generateDialogLabel("");
    assertTrue(result.startsWith("dialog_"));
    assertTrue(result.length() > 7);
  }

  @ParameterizedTest
  @ValueSource(strings = {"main", "quest", "trade"})
  void testGenerateDialogLabel_withName(String name) {
    String result = DialogUtils.generateDialogLabel(name);
    assertEquals(name.toLowerCase(), result);
  }

  @Test
  void testGenerateLabel_randomness() {
    String label1 = DialogUtils.generateButtonLabel(null);
    String label2 = DialogUtils.generateButtonLabel(null);
    String label3 = DialogUtils.generateButtonLabel(null);
    assertFalse(label1.equals(label2) && label2.equals(label3));
  }

  @Test
  void testGenerateLabel_lowercase() {
    String result = DialogUtils.generateButtonLabel(null);
    assertEquals(result, result.toLowerCase());
  }

  @Test
  @DisplayName("Should not contain hyphens in random labels")
  void testGenerateLabel_noHyphens() {
    String result = DialogUtils.generateButtonLabel(null);
    String randomPart = result.substring(result.indexOf('_') + 1);
    assertFalse(randomPart.contains("-"));
  }

  @Test
  @DisplayName("Should verify max button label length constant")
  void testMaxButtonLabelLength() {
    assertEquals(32, DialogButtonEntry.MAX_BUTTON_LABEL_LENGTH);
  }

  @Test
  @DisplayName("Should verify max dialog label length constant")
  void testMaxDialogLabelLength() {
    assertEquals(32, DialogDataEntry.MAX_DIALOG_LABEL_LENGTH);
  }
}
