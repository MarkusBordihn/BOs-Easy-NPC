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

package de.markusbordihn.easynpc.api.preset;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.preset.PresetAccess;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.Entity;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class BasePresetValidationTest {

  private static final Path BASE_PRESET_FOLDER =
      Path.of("src", "main", "resources", "data", "easy_npc", "api", "preset", "base");

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static List<Path> basePresetFiles() throws IOException {
    try (Stream<Path> files = Files.list(BASE_PRESET_FOLDER)) {
      return files.filter(file -> file.toString().endsWith(".npc.snbt")).sorted().toList();
    }
  }

  private static CompoundTag parse(Path basePresetFile) throws IOException {
    try {
      return TagParser.parseTag(Files.readString(basePresetFile, StandardCharsets.UTF_8));
    } catch (Exception exception) {
      throw new IOException("Unable to read " + basePresetFile, exception);
    }
  }

  @Test
  @DisplayName("Every base preset is shipped and readable")
  void testBasePresetsAreShipped() throws IOException {
    assertTrue(
        Files.isDirectory(BASE_PRESET_FOLDER),
        "The base preset folder " + BASE_PRESET_FOLDER.toAbsolutePath() + " must exist");
    assertTrue(!basePresetFiles().isEmpty(), "There must be at least one base preset");
  }

  @Test
  @DisplayName("Every base preset passes the preset validator")
  void testBasePresetsAreValid() throws IOException {
    for (Path basePresetFile : basePresetFiles()) {
      PresetValidationReport report =
          PresetValidator.validateSnbt(
              Files.readString(basePresetFile, StandardCharsets.UTF_8),
              PresetValidationContext.offline().withIdentityFreePreset());

      assertTrue(
          report.isValid(),
          "The base preset "
              + basePresetFile.getFileName()
              + " must be valid: "
              + report.formatIssues());
      assertTrue(
          !report.hasWarnings(),
          "The base preset "
              + basePresetFile.getFileName()
              + " must not warn: "
              + report.formatIssues());
    }
  }

  @Test
  @DisplayName("Every base preset is hidden and names the NPC type of its file")
  void testBasePresetsAreHidden() throws IOException {
    for (Path basePresetFile : basePresetFiles()) {
      CompoundTag basePreset = parse(basePresetFile);
      PresetMetadata metadata =
          PresetMetadata.fromCompoundTag(
              basePreset.getCompound(PresetDataCapable.PRESET_METADATA_TAG));

      assertEquals(
          PresetAccess.INTERNAL,
          metadata.access(),
          "The base preset " + basePresetFile.getFileName() + " must stay hidden");

      String entityTypeId = basePreset.getCompound(PresetData.DATA_TAG).getString(Entity.ID_TAG);
      String expectedEntityTypePath =
          basePresetFile.getFileName().toString().replace(".npc.snbt", "");
      assertEquals(
          "easy_npc:" + expectedEntityTypePath,
          entityTypeId,
          "The base preset " + basePresetFile.getFileName() + " must match its file name");
      assertEquals(
          entityTypeId,
          metadata.entityTypeId(),
          "The base preset " + basePresetFile.getFileName() + " must name its NPC type");
    }
  }
}
