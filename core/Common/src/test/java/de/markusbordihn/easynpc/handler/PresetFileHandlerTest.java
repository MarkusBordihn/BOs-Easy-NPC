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

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtAccounter;
import net.minecraft.nbt.NbtIo;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class PresetFileHandlerTest {

  @TempDir File tempDir;

  private File nbtFile;
  private CompoundTag testPresetData;

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @BeforeEach
  void setUp() throws IOException {
    // Create new wrapper format
    testPresetData = new CompoundTag();

    // PresetMetadata at root level
    CompoundTag metadata = new CompoundTag();
    metadata.putString("name", "Test Preset");
    metadata.putString("author", "TestAuthor");
    metadata.putString("version", "1.0.0");
    metadata.putString("category", "General");
    metadata.putLong("created", System.currentTimeMillis());
    metadata.putLong("modified", System.currentTimeMillis());
    metadata.putString("description", "Test preset");
    metadata.putString("entityTypeId", "minecraft:armor_stand");
    metadata.putString("variantType", "");
    testPresetData.put("PresetMetadata", metadata);

    // Data at root level
    CompoundTag data = new CompoundTag();
    data.putString("id", "minecraft:armor_stand");
    CompoundTagUtils.writeUUID(data, "UUID", UUID.randomUUID());
    data.putString("CustomName", "{\"text\":\"Test NPC\"}");
    data.putFloat("Health", 20.0f);
    testPresetData.put("data", data);

    nbtFile = new File(tempDir, "test_preset.npc.nbt");
  }

  @AfterEach
  void tearDown() {
    if (nbtFile != null && nbtFile.exists()) {
      nbtFile.delete();
    }
  }

  private File getResourceFile(String resourcePath) throws IOException {
    InputStream resourceStream = getClass().getResourceAsStream(resourcePath);
    assertNotNull(resourceStream, "Resource not found: " + resourcePath);

    File tempFile = new File(tempDir, new File(resourcePath).getName());
    Files.copy(resourceStream, tempFile.toPath());
    resourceStream.close();
    return tempFile;
  }

  @Test
  void testLoadPresetFromNbtFile() throws IOException {
    NbtIo.writeCompressed(testPresetData, nbtFile.toPath());

    CompoundTag loaded = PresetFileHandler.load(nbtFile);

    assertNotNull(loaded, "Loaded NBT should not be null");
    assertTrue(loaded.contains("PresetMetadata"), "Should have PresetMetadata at root");
    assertTrue(loaded.contains("data"), "Should have data at root");

    CompoundTag data = loaded.getCompound("data").orElse(new CompoundTag());
    assertEquals("minecraft:armor_stand", data.getString("id").orElse(""));
  }

  @Test
  void testLoadPresetFromSnbtFile() throws IOException {
    File snbtFile =
        getResourceFile("/de/markusbordihn/easynpc/handler/presets/simple_armor_stand.npc.snbt");

    CompoundTag loaded = PresetFileHandler.load(snbtFile);

    assertNotNull(loaded, "Loaded SNBT should not be null");
    assertTrue(loaded.contains("PresetMetadata"), "Should have PresetMetadata at root");
    assertTrue(loaded.contains("data"), "Should have data at root");

    CompoundTag metadata = loaded.getCompound("PresetMetadata").orElse(new CompoundTag());
    assertEquals("Test Preset", metadata.getString("name").orElse(""));
    assertEquals("minecraft:armor_stand", metadata.getString("entityTypeId").orElse(""));

    CompoundTag data = loaded.getCompound("data").orElse(new CompoundTag());
    assertEquals("minecraft:armor_stand", data.getString("id").orElse(""));
    assertTrue(data.contains("UUID"));

    snbtFile.delete();
  }

  @Test
  @DisplayName("Should return empty CompoundTag for unknown file format")
  void testUnknownFileFormat() throws IOException {
    File unknownFile = new File(tempDir, "test_preset.txt");
    Files.writeString(unknownFile.toPath(), "invalid content");

    CompoundTag loaded = PresetFileHandler.load(unknownFile);

    assertNotNull(loaded, "Should return empty CompoundTag for unknown format");
    assertTrue(loaded.isEmpty(), "CompoundTag should be empty");

    unknownFile.delete();
  }

  @Test
  @DisplayName("Should fallback to SNBT when NBT parsing fails")
  void testFallbackToSnbt() throws IOException {
    File snbtFile =
        getResourceFile("/de/markusbordihn/easynpc/handler/presets/zombie_fallback.npc.snbt");

    CompoundTag loaded = PresetFileHandler.load(snbtFile);

    assertNotNull(loaded, "Should load as SNBT after NBT fails");
    assertEquals("minecraft:zombie", loaded.getString("id").orElse(""));

    snbtFile.delete();
  }

  @Test
  @DisplayName("Should store a generated preset UUID in the file instead of minting a new one")
  void testPresetUUIDStaysStableAcrossLoads() throws IOException {
    NbtIo.writeCompressed(testPresetData, nbtFile.toPath());

    CompoundTag firstLoad = PresetFileHandler.loadWithStablePresetUUID(nbtFile);
    CompoundTag secondLoad = PresetFileHandler.loadWithStablePresetUUID(nbtFile);

    UUID firstPresetUUID =
        CompoundTagUtils.readUUID(firstLoad.getCompoundOrEmpty("data"), "PresetUUID");

    assertNotNull(firstPresetUUID);
    assertEquals(
        firstPresetUUID,
        CompoundTagUtils.readUUID(secondLoad.getCompoundOrEmpty("data"), "PresetUUID"));
    assertEquals(
        firstPresetUUID,
        CompoundTagUtils.readUUID(
            NbtIo.readCompressed(nbtFile.toPath(), NbtAccounter.unlimitedHeap())
                .getCompoundOrEmpty("data"),
            "PresetUUID"));
  }

  @Test
  @DisplayName("Should keep an existing preset UUID untouched")
  void testExistingPresetUUIDIsKept() throws IOException {
    UUID presetUUID = UUID.randomUUID();
    CompoundTagUtils.writeUUID(testPresetData.getCompoundOrEmpty("data"), "PresetUUID", presetUUID);
    NbtIo.writeCompressed(testPresetData, nbtFile.toPath());

    CompoundTag loaded = PresetFileHandler.loadWithStablePresetUUID(nbtFile);

    assertEquals(
        presetUUID, CompoundTagUtils.readUUID(loaded.getCompoundOrEmpty("data"), "PresetUUID"));
  }

  @Test
  @DisplayName("Should store the generated preset UUID at root level for the legacy format")
  void testPresetUUIDForLegacyFormat() throws IOException {
    CompoundTag legacyPresetData = new CompoundTag();
    legacyPresetData.putString("id", "minecraft:zombie");
    NbtIo.writeCompressed(legacyPresetData, nbtFile.toPath());

    CompoundTag loaded = PresetFileHandler.loadWithStablePresetUUID(nbtFile);

    UUID presetUUID = CompoundTagUtils.readUUID(loaded, "PresetUUID");

    assertNotNull(presetUUID);
    assertEquals(
        presetUUID,
        CompoundTagUtils.readUUID(
            NbtIo.readCompressed(nbtFile.toPath(), NbtAccounter.unlimitedHeap()), "PresetUUID"));
  }

  @Test
  void testLoadNonExistentFile() {
    File nonExistent = new File(tempDir, "does_not_exist.npc.nbt");

    CompoundTag loaded = PresetFileHandler.load(nonExistent);

    assertNull(loaded, "Should return null for non-existent file");
  }

  @Test
  void testLoadNullFile() {
    CompoundTag loaded = PresetFileHandler.load(null);

    assertNull(loaded, "Should return null for null file");
  }

  @Test
  void testExportPresetToNbtFile() throws IOException {
    File exportFile = new File(tempDir, "exported.npc.nbt");

    boolean result = PresetFileHandler.save(exportFile, testPresetData);

    assertTrue(result, "Export should succeed");
    assertTrue(exportFile.exists(), "Export file should exist");

    CompoundTag loaded = NbtIo.readCompressed(exportFile.toPath(), NbtAccounter.unlimitedHeap());
    assertNotNull(loaded);
    assertTrue(loaded.contains("PresetMetadata"), "Exported file should have PresetMetadata");
    assertTrue(loaded.contains("data"), "Exported file should have data");

    exportFile.delete();
  }

  @Test
  void testExportPresetToSnbtFile() throws IOException {
    File exportFile = new File(tempDir, "exported.npc.snbt");

    boolean result = PresetFileHandler.saveSnbt(exportFile, testPresetData);

    assertTrue(result, "SNBT export should succeed");
    assertTrue(exportFile.exists(), "SNBT export file should exist");

    String content = Files.readString(exportFile.toPath());
    assertTrue(content.contains("PresetMetadata"), "SNBT should contain PresetMetadata");
    assertTrue(content.contains("data"), "SNBT should contain data");
    assertTrue(content.contains("minecraft:armor_stand"), "SNBT should contain entity ID");
    assertTrue(content.contains("Test NPC"), "SNBT should contain custom name");
    assertFalse(content.contains("Skipped"), "SNBT should not skip binary data");
    assertTrue(content.contains("{\n"), "SNBT should be formatted with newlines after {");
    assertTrue(content.contains("\n}"), "SNBT should be formatted with } on new lines");
    assertTrue(content.lines().count() > 1, "SNBT should be formatted across multiple lines");

    exportFile.delete();
  }

  @Test
  void testExportWithNullFile() {
    boolean result = PresetFileHandler.save(null, testPresetData);

    assertFalse(result, "Export should fail with null file");
  }

  @Test
  void testExportWithNullData() {
    File exportFile = new File(tempDir, "test.npc.nbt");

    boolean result = PresetFileHandler.save(exportFile, null);

    assertFalse(result, "Export should fail with null data");
  }

  @Test
  void testExportWithEmptyData() {
    File exportFile = new File(tempDir, "test.npc.nbt");
    CompoundTag emptyData = new CompoundTag();

    boolean result = PresetFileHandler.save(exportFile, emptyData);

    assertFalse(result, "Export should fail with empty data");
  }

  @Test
  void testImportPresetFromSnbt() throws IOException {
    File snbtFile =
        getResourceFile("/de/markusbordihn/easynpc/handler/presets/villager_trader.npc.snbt");

    CompoundTag imported = PresetFileHandler.loadSnbt(snbtFile);

    assertNotNull(imported, "Imported SNBT should not be null");
    assertTrue(imported.contains("PresetMetadata"), "Should have new format");
    CompoundTag importedData = imported.getCompound("data").orElse(new CompoundTag());
    assertEquals("minecraft:villager", importedData.getString("id").orElse(""));
    assertTrue(importedData.contains("Health"));

    snbtFile.delete();
  }

  @Test
  @DisplayName("Should load legacy format for backward compatibility")
  void testLoadLegacyFormat() throws IOException {
    File snbtFile =
        getResourceFile("/de/markusbordihn/easynpc/handler/presets/legacy_format.npc.snbt");

    CompoundTag imported = PresetFileHandler.loadSnbt(snbtFile);

    assertNotNull(imported, "Legacy format should still load");
    // Legacy format has metadata nested in preset_metadata
    assertTrue(
        imported.contains("preset_metadata") || imported.contains("PresetMetadata"),
        "Should have metadata in some form");
    assertEquals("minecraft:zombie", imported.getString("id").orElse(""));

    snbtFile.delete();
  }

  @Test
  void testComplexSnbtWithVariant() throws IOException {
    File snbtFile =
        getResourceFile("/de/markusbordihn/easynpc/handler/presets/complex_humanoid.npc.snbt");

    CompoundTag imported = PresetFileHandler.loadSnbt(snbtFile);

    assertNotNull(imported, "Complex SNBT should be imported");
    assertTrue(imported.contains("PresetMetadata"), "Should have PresetMetadata");

    CompoundTag metadata = imported.getCompound("PresetMetadata").orElse(new CompoundTag());
    assertEquals("easy_npc:humanoid", metadata.getString("entityTypeId").orElse(""));
    assertEquals("STEVE", metadata.getString("variantType").orElse(""));

    CompoundTag data = imported.getCompound("data").orElse(new CompoundTag());
    assertEquals("easy_npc:humanoid", data.getString("id").orElse(""));
    assertEquals("STEVE", data.getString("VariantType").orElse(""));

    snbtFile.delete();
  }

  @Test
  void testImportMalformedSnbt() throws IOException {
    File snbtFile = getResourceFile("/de/markusbordihn/easynpc/handler/presets/malformed.npc.snbt");

    CompoundTag imported = PresetFileHandler.loadSnbt(snbtFile);

    assertNull(imported, "Should return null for malformed SNBT");

    snbtFile.delete();
  }
}
