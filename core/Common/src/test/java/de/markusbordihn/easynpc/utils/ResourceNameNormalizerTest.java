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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ResourceNameNormalizerTest {

  @Test
  @DisplayName("Umlauts and accents are transliterated instead of dropped")
  void transliteratesInsteadOfDropping() {
    assertEquals("baer", ResourceNameNormalizer.toIdentifier("Bär"));
    assertEquals("waechter_gross", ResourceNameNormalizer.toIdentifier("Wächter Groß"));
    assertEquals("cafe_creme", ResourceNameNormalizer.toIdentifier("Café Crème"));
    assertEquals("blaahaj_oest", ResourceNameNormalizer.toIdentifier("Blåhaj Øst"));
    assertEquals("nandu", ResourceNameNormalizer.toIdentifier("Ñandú"));
    assertEquals("haendler_and_kunde", ResourceNameNormalizer.toIdentifier("Händler & Kunde"));
  }

  @Test
  @DisplayName("Identifiers that are already valid stay unchanged")
  void alreadyValidIdentifiersStayUnchanged() {
    assertEquals("hndler_begrung", ResourceNameNormalizer.toIdentifier("hndler_begrung"));
    assertEquals("a__b", ResourceNameNormalizer.toIdentifier("a__b"));
    assertEquals("_leading", ResourceNameNormalizer.toIdentifier("_leading"));
    assertEquals("trailing_", ResourceNameNormalizer.toIdentifier("trailing_"));
    assertEquals("button_1", ResourceNameNormalizer.toIdentifier("button_1"));
  }

  @Test
  @DisplayName("Resource paths and file names that are already valid stay unchanged")
  void alreadyValidPathsAndFileNamesStayUnchanged() {
    String uuid = UUID.randomUUID().toString();
    assertEquals(uuid, ResourceNameNormalizer.toResourcePath(uuid));
    assertEquals(uuid + ".png", ResourceNameNormalizer.toFileName(uuid + ".png"));
    assertEquals("sit-down", ResourceNameNormalizer.toResourcePath("sit-down"));
    assertEquals("pose/humanoid/sit", ResourceNameNormalizer.toResourcePath("pose/humanoid/sit"));
    assertEquals("My_Preset.npc", ResourceNameNormalizer.toFileName("My_Preset.npc"));
  }

  @Test
  @DisplayName("Text without any usable character gets a stable fallback")
  void unusableTextGetsStableFallback() {
    assertEquals("", ResourceNameNormalizer.toIdentifier("モデル"));
    String japanese = ResourceNameNormalizer.toIdentifier("モデル", "dialog", 32);
    String cyrillic = ResourceNameNormalizer.toIdentifier("Модель", "dialog", 32);
    String emoji = ResourceNameNormalizer.toIdentifier("🐢", "dialog", 32);

    assertTrue(japanese.startsWith("dialog_"));
    assertEquals(japanese, ResourceNameNormalizer.toIdentifier("モデル", "dialog", 32));
    assertNotEquals(japanese, cyrillic);
    assertNotEquals(cyrillic, emoji);
  }

  @Test
  @DisplayName("Identifiers are limited to the requested length")
  void identifiersAreLimitedToTheRequestedLength() {
    assertEquals("haendl", ResourceNameNormalizer.toIdentifier("Händler", "dialog", 6));
    assertEquals("dialog", ResourceNameNormalizer.toIdentifier("モデル", "dialog", 6));
  }

  @Test
  @DisplayName("Relative path segments are removed")
  void relativePathSegmentsAreRemoved() {
    assertEquals("a/b", ResourceNameNormalizer.toResourcePath("a/../b"));
    assertEquals("etc/passwd", ResourceNameNormalizer.toResourcePath("../../etc/passwd"));
    assertFalse(ResourceNameNormalizer.toFileName("../../secret.npc").contains(".."));
    assertFalse(ResourceNameNormalizer.toResourcePath("a/./b").contains("/./"));
  }

  @Test
  @DisplayName("File names keep their upper case letters and lose path separators")
  void fileNamesKeepUpperCaseAndLosePathSeparators() {
    assertEquals("Baer_Waechter.npc", ResourceNameNormalizer.toFileName("Bär Wächter.npc"));
    assertEquals("folder_preset.npc", ResourceNameNormalizer.toFileName("folder/preset.npc"));
    assertTrue(ResourceNameNormalizer.toFileName("🐢", "preset").startsWith("preset_"));
  }

  @ParameterizedTest
  @ValueSource(
      strings = {
        "Bär",
        "モデル",
        "🐢",
        "Модель",
        "ΑΒΓ αβγ",
        "İstanbul",
        "Ａｎｇｅｌ",
        "  --Trader!!--  ",
        "a/../b",
        "///",
        "..",
        "."
      })
  @DisplayName("Every result is usable as identifier, resource path and file name")
  void everyResultIsUsable(String value) {
    assertTrue(
        ResourceNameNormalizer.toIdentifier(value, "dialog", 32).matches("[a-z0-9_]+"),
        () -> "Invalid identifier for " + value);
    assertTrue(
        ResourceNameNormalizer.toResourcePath(value, "pose").matches("[a-z0-9_./-]+"),
        () -> "Invalid resource path for " + value);
    assertTrue(
        ResourceNameNormalizer.toFileName(value, "preset").matches("[a-zA-Z0-9_.-]+"),
        () -> "Invalid file name for " + value);
  }

  @Test
  @DisplayName("Empty and null input is handled")
  void emptyAndNullInputIsHandled() {
    assertEquals("", ResourceNameNormalizer.toIdentifier(null));
    assertEquals("", ResourceNameNormalizer.toIdentifier(""));
    assertEquals("", ResourceNameNormalizer.toResourcePath(null));
    assertEquals("", ResourceNameNormalizer.toFileName(null));
    assertEquals(
        "preset_" + ResourceNameNormalizer.hash(""),
        ResourceNameNormalizer.toFileName("", "preset"));
  }
}
