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

package de.markusbordihn.easynpc.data.preset;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.io.DataFileHandler;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@DisplayName("PresetExportFormat Tests")
class PresetExportFormatTest {

  @ParameterizedTest
  @CsvSource({
    "'Give Stone Or Dirt To A Player', 'give_stone_or_dirt_to_a_player'",
    "'Test Preset', 'test_preset'",
    "'Multiple   Spaces', 'multiple_spaces'",
    "'UPPERCASE', 'uppercase'",
    "'MixedCase123', 'mixedcase123'",
    "'already_normalized', 'already_normalized'"
  })
  void testNormalizeFilename(String input, String expected) {
    String result = PresetExportFormat.normalizeFilename(input);
    assertEquals(expected, result, "Filename normalization failed for: " + input);
  }

  @ParameterizedTest
  @CsvSource({
    "'test.npc.nbt', true",
    "'test.npc.snbt', true",
    "'test.npc.json', true",
    "'test.txt', false",
    "'test', false",
    "'test.npc', false"
  })
  void testHasPresetExtension(String filename, boolean expected) {
    boolean result = PresetExportFormat.hasPresetExtension(filename);
    assertEquals(expected, result, "Extension check failed for: " + filename);
  }

  @ParameterizedTest
  @CsvSource({
    "'test.npc.nbt', 'test'",
    "'test.npc.snbt', 'test'",
    "'test.npc.json', 'test'",
    "'my_preset.npc.nbt', 'my_preset'",
    "'no_extension', 'no_extension'"
  })
  void testRemovePresetExtension(String input, String expected) {
    String result = PresetExportFormat.removePresetExtension(input);
    assertEquals(expected, result, "Extension removal failed for: " + input);
  }

  @Test
  void testGetFileExtension() {
    assertEquals(".npc.nbt", PresetExportFormat.NBT.getFileExtension());
    assertEquals(".npc.snbt", PresetExportFormat.SNBT.getFileExtension());
    assertEquals(".npc.json", PresetExportFormat.JSON.getFileExtension());
  }

  @ParameterizedTest
  @DisplayName("Complete export filename flow - NBT format")
  @CsvSource({
    "'Give Stone Or Dirt To A Player', 'give_stone_or_dirt_to_a_player.npc.nbt'",
    "'Test Preset', 'test_preset.npc.nbt'",
    "'my preset', 'my_preset.npc.nbt'"
  })
  void testCompleteExportFlowNBT(String originalName, String expectedFilename) {
    String step1 = PresetExportFormat.removePresetExtension(originalName);
    String step2 = PresetExportFormat.normalizeFilename(step1);
    String step3 = step2 + PresetExportFormat.NBT.getFileExtension();
    String finalFilename = DataFileHandler.getPresetFileName(step3);

    assertEquals(expectedFilename, finalFilename, "Export flow failed for: " + originalName);
  }

  @ParameterizedTest
  @DisplayName("Complete export filename flow - SNBT format")
  @CsvSource({
    "'Give Stone Or Dirt To A Player', 'give_stone_or_dirt_to_a_player.npc.snbt'",
    "'Test Preset', 'test_preset.npc.snbt'",
    "'my preset', 'my_preset.npc.snbt'"
  })
  void testCompleteExportFlowSNBT(String originalName, String expectedFilename) {
    String step1 = PresetExportFormat.removePresetExtension(originalName);
    String step2 = PresetExportFormat.normalizeFilename(step1);
    String step3 = step2 + PresetExportFormat.SNBT.getFileExtension();
    String finalFilename = DataFileHandler.getPresetFileName(step3);

    assertEquals(expectedFilename, finalFilename, "Export flow failed for: " + originalName);
  }

  @ParameterizedTest
  @CsvSource({
    "'test.npc.nbt', 'test.npc.nbt'",
    "'test.npc.snbt', 'test.npc.snbt'",
    "'my_preset.npc.nbt', 'my_preset.npc.nbt'"
  })
  void testNoDuplicateExtensions(String input, String expected) {
    String result = DataFileHandler.getPresetFileName(input);
    assertEquals(expected, result, "Duplicate extension added for: " + input);
  }

  @Test
  @DisplayName("Should handle edge case: filename with dots")
  void testFilenameWithDots() {
    String originalName = "my.test.preset";
    String step1 = PresetExportFormat.removePresetExtension(originalName);
    String step2 = PresetExportFormat.normalizeFilename(step1);
    String step3 = step2 + PresetExportFormat.NBT.getFileExtension();
    String finalFilename = DataFileHandler.getPresetFileName(step3);

    assertEquals("my.test.preset.npc.nbt", finalFilename);
  }

  @Test
  @DisplayName("Real world test case - the reported bug")
  void testReportedBugCase() {
    String userInput = "Give Stone Or Dirt To A Player";

    String nbtStep1 = PresetExportFormat.removePresetExtension(userInput);
    String nbtStep2 = PresetExportFormat.normalizeFilename(nbtStep1);
    String nbtStep3 = nbtStep2 + PresetExportFormat.NBT.getFileExtension();
    String nbtFinal = DataFileHandler.getPresetFileName(nbtStep3);

    assertEquals(
        "give_stone_or_dirt_to_a_player.npc.nbt", nbtFinal, "NBT export produces wrong filename");
    assertFalse(nbtFinal.contains("playernpc.nbt"), "NBT filename should not have missing dot");

    String snbtStep1 = PresetExportFormat.removePresetExtension(userInput);
    String snbtStep2 = PresetExportFormat.normalizeFilename(snbtStep1);
    String snbtStep3 = snbtStep2 + PresetExportFormat.SNBT.getFileExtension();
    String snbtFinal = DataFileHandler.getPresetFileName(snbtStep3);

    assertEquals(
        "give_stone_or_dirt_to_a_player.npc.snbt",
        snbtFinal,
        "SNBT export produces wrong filename");
    assertFalse(snbtFinal.contains("playernpc.snbt"), "SNBT filename should not have missing dot");
  }
}
