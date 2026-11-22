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

package de.markusbordihn.easynpc.io;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.config.CustomSkinConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.io.ServerSkinIndex.ServerSkin;
import de.markusbordihn.easynpc.utils.PngCompressionUtils;
import de.markusbordihn.easynpc.validator.PngFileValidator;
import de.markusbordihn.easynpc.validator.PngTestImages;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ServerSkinIndexTest {

  private static final UUID GUARD_SKIN_UUID =
      UUID.nameUUIDFromBytes("Guard.png".getBytes(StandardCharsets.UTF_8));

  @TempDir Path skinDataFolder;

  private static void writeFile(Path file, byte[] content) throws IOException {
    Files.createDirectories(file.getParent());
    Files.write(file, content);
  }

  @BeforeEach
  void loadSkins() throws IOException {
    Path humanoidFolder = this.skinDataFolder.resolve(SkinModel.HUMANOID.getName());
    writeFile(humanoidFolder.resolve("Guard.png"), PngTestImages.validSkin(64, 64));
    writeFile(humanoidFolder.resolve("town/Archer.png"), PngTestImages.validSkin(64, 64));
    writeFile(humanoidFolder.resolve("humanoid_template.png"), PngTestImages.validSkin(64, 64));
    writeFile(humanoidFolder.resolve("payload.png"), "PK\u0003\u0004".getBytes());
    writeFile(humanoidFolder.resolve("huge.png"), new byte[PngFileValidator.MAX_FILE_SIZE + 1]);
    writeFile(humanoidFolder.resolve("notes.txt"), "text".getBytes());
    ServerSkinIndex.load(this.skinDataFolder);
  }

  @AfterEach
  void clearSkins() {
    ServerSkinIndex.clear();
    CustomSkinConfig.SERVER_SKIN_COMPRESSION_LEVEL = PngCompressionUtils.MAX_COMPRESSION_LEVEL;
  }

  @Test
  @DisplayName("Only valid skin files are indexed per skin model")
  void onlyValidSkinFilesAreIndexed() {
    assertEquals(2, ServerSkinIndex.getSkins(SkinModel.HUMANOID).size());
    assertNotNull(ServerSkinIndex.getSkin(SkinModel.HUMANOID, GUARD_SKIN_UUID));
    assertNull(ServerSkinIndex.getSkin(SkinModel.HUMANOID_SLIM, GUARD_SKIN_UUID));
    assertTrue(ServerSkinIndex.getSkins(SkinModel.HUMANOID_SLIM).isEmpty());
  }

  @Test
  @DisplayName("Skins are found by name, case-insensitive name and UUID")
  void skinsAreFoundByNameAndUUID() {
    assertEquals(GUARD_SKIN_UUID, ServerSkinIndex.findSkins("Guard").get(0).skinUUID());
    assertEquals(GUARD_SKIN_UUID, ServerSkinIndex.findSkins("guard").get(0).skinUUID());
    assertEquals(
        GUARD_SKIN_UUID, ServerSkinIndex.findSkins(GUARD_SKIN_UUID.toString()).get(0).skinUUID());
    assertEquals(1, ServerSkinIndex.findSkins("Archer").size());
    assertTrue(ServerSkinIndex.findSkins("humanoid_template").isEmpty());
    assertTrue(ServerSkinIndex.findSkins("payload").isEmpty());
  }

  @Test
  @DisplayName("Poorly compressed skin above the share limit is recompressed and shared")
  void poorlyCompressedSkinIsRecompressed() throws IOException {
    byte[] storedPng = PngTestImages.uncompressedSkin(1024, 512);
    writeFile(this.skinDataFolder.resolve("humanoid_slim/Stored.png"), storedPng);
    assertTrue(storedPng.length > ServerSkinIndex.MAX_SKIN_FILE_SIZE);

    ServerSkinIndex.load(this.skinDataFolder);

    ServerSkin serverSkin = ServerSkinIndex.findSkins("Stored").get(0);
    assertEquals(SkinModel.HUMANOID_SLIM, serverSkin.skinModel());
    assertTrue(serverSkin.pngBytes().length < ServerSkinIndex.MAX_SKIN_FILE_SIZE);
    assertTrue(PngFileValidator.sanitize(serverSkin.pngBytes()).isValid());
  }

  @Test
  @DisplayName("Disabled recompression rejects a skin above the share limit")
  void disabledRecompressionRejectsLargeSkin() throws IOException {
    writeFile(
        this.skinDataFolder.resolve("humanoid_slim/Stored.png"),
        PngTestImages.uncompressedSkin(1024, 512));
    CustomSkinConfig.SERVER_SKIN_COMPRESSION_LEVEL = 0;

    ServerSkinIndex.load(this.skinDataFolder);

    assertTrue(ServerSkinIndex.findSkins("Stored").isEmpty());
    assertEquals(2, ServerSkinIndex.getSkins(SkinModel.HUMANOID).size());
  }

  @Test
  @DisplayName("Display names lose formatting codes and control characters")
  void displayNamesAreSanitized() {
    assertEquals("Evilname", ServerSkinIndex.getDisplayName("§cEvil‮name\u0007.png"));
    assertEquals(
        ServerSkinIndex.MAX_DISPLAY_NAME_LENGTH,
        ServerSkinIndex.getDisplayName("a".repeat(200) + ".png").length());
  }
}
