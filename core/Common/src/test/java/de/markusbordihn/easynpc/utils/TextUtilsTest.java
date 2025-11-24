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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("TextUtils Tests")
class TextUtilsTest {

  @Test
  @DisplayName("Should identify valid translation keys")
  void testIsTranslationKey_validKeys() {
    assertTrue(TextUtils.isTranslationKey("item.minecraft.diamond_sword"));
    assertTrue(TextUtils.isTranslationKey("block.easynpc.spawner"));
    assertTrue(TextUtils.isTranslationKey("text.config.advanced-trading"));
    assertTrue(TextUtils.isTranslationKey("gui.easynpc.dialog.title"));
  }

  @ParameterizedTest
  @DisplayName("Should reject invalid translation keys")
  @ValueSource(
      strings = {
        "invalid",
        "no_dots",
        ".startsWithDot",
        "endsWithDot.",
        "double..dot",
        "has space.key",
        "special@char.key"
      })
  void testIsTranslationKey_invalidKeys(String key) {
    assertFalse(TextUtils.isTranslationKey(key));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should reject null or empty translation keys")
  void testIsTranslationKey_nullOrEmpty(String key) {
    assertFalse(TextUtils.isTranslationKey(key));
  }

  @ParameterizedTest
  @CsvSource({
    "hello_world, Hello world",
    "some-text, Some text",
    "UPPERCASE, Uppercase",
    "mixed_CASE-test, Mixed case test",
    "single, Single"
  })
  @DisplayName("Should normalize strings correctly")
  void testNormalizeString(String input, String expected) {
    assertEquals(expected, TextUtils.normalizeString(input));
  }

  @Test
  @DisplayName("Should limit string to max size with ellipsis")
  void testLimitString() {
    assertEquals("Hello…", TextUtils.limitString("Hello World", 5));
    assertEquals("Test", TextUtils.limitString("Test", 10));
    assertEquals("", TextUtils.limitString("", 5));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should handle null and empty strings in limitString")
  void testLimitString_nullOrEmpty(String input) {
    assertEquals(input, TextUtils.limitString(input, 10));
  }

  @Test
  @DisplayName("Should trim whitespace before limiting")
  void testLimitString_withWhitespace() {
    assertEquals("Test…", TextUtils.limitString("  Test String  ", 4));
  }

  @ParameterizedTest
  @CsvSource({
    "hello_world, helloWorld",
    "test-case, testCase",
    "SIMPLE, simple",
    "UPPER_CASE, upperCase",
    "Mixed_Test, mixedTest",
    "test with spaces, testWithSpaces"
  })
  @DisplayName("Should convert to camelCase correctly")
  void testConvertToCamelCase(String input, String expected) {
    assertEquals(expected, TextUtils.convertToCamelCase(input));
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should handle null and empty strings in case conversion")
  void testConvertToCamelCase_nullOrEmpty(String input) {
    assertEquals(input, TextUtils.convertToCamelCase(input));
    assertEquals(input, TextUtils.convertToPascalCase(input));
  }

  @Test
  @DisplayName("Should normalize string with max size")
  void testNormalizeString_withMaxSize() {
    assertEquals("Hello…", TextUtils.normalizeString("hello_world_test", 5));
    assertEquals("Test", TextUtils.normalizeString("test", 10));
  }
}
