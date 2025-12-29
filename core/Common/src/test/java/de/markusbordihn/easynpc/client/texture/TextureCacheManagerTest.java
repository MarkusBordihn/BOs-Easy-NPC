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

package de.markusbordihn.easynpc.client.texture;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.skin.SkinModel;
import java.io.File;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("TextureCacheManager Tests")
class TextureCacheManagerTest {

  @Test
  @DisplayName("Should extract UUID from valid PNG filename")
  void testGetUUIDFromFilename_validUUID() {
    String validUUID = "550e8400-e29b-41d4-a716-446655440000";
    String filename = validUUID + ".png";

    UUID result = TextureCacheManager.getUUIDFromFilename(filename);

    assertNotNull(result);
    assertEquals(UUID.fromString(validUUID), result);
  }

  @Test
  @DisplayName("Should generate name-based UUID for invalid UUID format")
  void testGetUUIDFromFilename_invalidUUIDFormat() {
    String filename = "invalid-uuid-format.png";

    UUID result = TextureCacheManager.getUUIDFromFilename(filename);

    assertNotNull(result);
    // Should create a name-based UUID from bytes
    assertEquals(UUID.nameUUIDFromBytes(filename.getBytes()), result);
  }

  @ParameterizedTest
  @NullAndEmptySource
  @DisplayName("Should return null for null or empty filename")
  void testGetUUIDFromFilename_nullOrEmpty(String filename) {
    UUID result = TextureCacheManager.getUUIDFromFilename(filename);
    assertNull(result);
  }

  @ParameterizedTest
  @ValueSource(strings = {"test.jpg", "test.txt", "test", "test.PNG"})
  @DisplayName("Should return null for non-PNG files")
  void testGetUUIDFromFilename_nonPngFile(String filename) {
    UUID result = TextureCacheManager.getUUIDFromFilename(filename);
    assertNull(result);
  }

  @Test
  @DisplayName("Should create TextureModelKey from valid file")
  void testGetTextureModelKey_validFile() {
    UUID testUUID = UUID.fromString("550e8400-e29b-41d4-a716-446655440000");
    File testFile = new File(testUUID + ".png");

    TextureModelKey result = TextureCacheManager.getTextureModelKey(SkinModel.HUMANOID, testFile);

    assertNotNull(result);
    assertEquals(testUUID, result.getUUID());
    assertEquals(SkinModel.HUMANOID, result.getSkinModel());
  }

  @Test
  @DisplayName("Should create TextureModelKey with name-based UUID for non-UUID filename")
  void testGetTextureModelKey_nonUUIDFilename() {
    File testFile = new File("custom-texture.png");

    TextureModelKey result = TextureCacheManager.getTextureModelKey(SkinModel.HUMANOID, testFile);

    assertNotNull(result);
    assertNotNull(result.getUUID());
    assertEquals(SkinModel.HUMANOID, result.getSkinModel());
  }

  @Test
  @DisplayName("Should return null for invalid file extension")
  void testGetTextureModelKey_invalidExtension() {
    File testFile = new File("test.jpg");

    TextureModelKey result = TextureCacheManager.getTextureModelKey(SkinModel.HUMANOID, testFile);

    assertNull(result);
  }

  @Test
  @DisplayName("Should handle different skin models")
  void testGetTextureModelKey_differentSkinModels() {
    UUID testUUID = UUID.randomUUID();
    File testFile = new File(testUUID + ".png");

    TextureModelKey humanoidKey =
        TextureCacheManager.getTextureModelKey(SkinModel.HUMANOID, testFile);
    TextureModelKey slimKey =
        TextureCacheManager.getTextureModelKey(SkinModel.HUMANOID_SLIM, testFile);

    assertNotNull(humanoidKey);
    assertNotNull(slimKey);
    assertEquals(SkinModel.HUMANOID, humanoidKey.getSkinModel());
    assertEquals(SkinModel.HUMANOID_SLIM, slimKey.getSkinModel());
  }
}
