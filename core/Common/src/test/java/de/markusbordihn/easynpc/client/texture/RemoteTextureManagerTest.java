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

import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("RemoteTextureManager Tests")
class RemoteTextureManagerTest {

  private TextureModelKey testKey;

  @BeforeEach
  void setUp() {
    RemoteTextureManager.clearTextureCache();
    testKey =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID);
  }

  @AfterEach
  void tearDown() {
    RemoteTextureManager.clearTextureCache();
  }

  @Test
  @DisplayName("Should mark texture as permanent failure")
  void testMarkPermanentFailure() {
    assertFalse(RemoteTextureManager.hasPermanentFailure(testKey));

    RemoteTextureManager.markPermanentFailure(
        testKey,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.INVALID_IMAGE_SIZE,
        "Test error",
        "http://test.url");

    assertTrue(RemoteTextureManager.hasPermanentFailure(testKey));
  }

  @Test
  @DisplayName("Should not retry texture marked as permanent failure")
  void testNoRetryForPermanentFailure() {
    RemoteTextureManager.markPermanentFailure(
        testKey,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.INVALID_IMAGE_SIZE,
        "Invalid dimensions",
        "http://test.url");

    assertTrue(RemoteTextureManager.hasPermanentFailure(testKey));

    RemoteTextureManager.markPermanentFailure(
        testKey,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.INVALID_IMAGE_SIZE,
        "Invalid dimensions retry",
        "http://test.url");

    assertTrue(RemoteTextureManager.hasPermanentFailure(testKey));
  }

  @Test
  @DisplayName("Should clear specific permanent failure")
  void testClearSpecificPermanentFailure() {
    RemoteTextureManager.markPermanentFailure(
        testKey,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.DECODING_ERROR,
        "Test error",
        "http://test.url");

    assertTrue(RemoteTextureManager.hasPermanentFailure(testKey));

    RemoteTextureManager.clearPermanentFailure(testKey);

    assertFalse(RemoteTextureManager.hasPermanentFailure(testKey));
  }

  @Test
  @DisplayName("Should clear all permanent failures")
  void testClearAllPermanentFailures() {
    TextureModelKey key1 =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID);
    TextureModelKey key2 =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID_SLIM);

    RemoteTextureManager.markPermanentFailure(
        key1,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.INVALID_IMAGE_SIZE,
        "Error 1",
        "http://test1.url");
    RemoteTextureManager.markPermanentFailure(
        key2,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.DECODING_ERROR,
        "Error 2",
        "http://test2.url");

    assertTrue(RemoteTextureManager.hasPermanentFailure(key1));
    assertTrue(RemoteTextureManager.hasPermanentFailure(key2));

    RemoteTextureManager.clearAllPermanentFailures();

    assertFalse(RemoteTextureManager.hasPermanentFailure(key1));
    assertFalse(RemoteTextureManager.hasPermanentFailure(key2));
  }

  @Test
  @DisplayName("Should handle permanent failure types correctly")
  void testPermanentFailureTypes() {
    assertTrue(
        de.markusbordihn.easynpc.data.texture.TextureFailureType.INVALID_IMAGE_SIZE.isPermanent());
    assertTrue(
        de.markusbordihn.easynpc.data.texture.TextureFailureType.DECODING_ERROR.isPermanent());
    assertTrue(
        de.markusbordihn.easynpc.data.texture.TextureFailureType.INVALID_FORMAT.isPermanent());
    assertTrue(
        de.markusbordihn.easynpc.data.texture.TextureFailureType.FILE_TOO_LARGE.isPermanent());
    assertTrue(de.markusbordihn.easynpc.data.texture.TextureFailureType.URL_INVALID.isPermanent());
    assertTrue(
        de.markusbordihn.easynpc.data.texture.TextureFailureType.MAX_RETRIES_EXCEEDED
            .isPermanent());

    assertFalse(
        de.markusbordihn.easynpc.data.texture.TextureFailureType.NETWORK_ERROR.isPermanent());
    assertFalse(de.markusbordihn.easynpc.data.texture.TextureFailureType.TIMEOUT.isPermanent());
  }

  @Test
  @DisplayName("Should not mark non-permanent failures as permanent")
  void testNonPermanentFailureNotMarked() {
    RemoteTextureManager.markPermanentFailure(
        testKey,
        de.markusbordihn.easynpc.data.texture.TextureFailureType.NETWORK_ERROR,
        "Network error",
        "http://test.url");

    assertFalse(RemoteTextureManager.hasPermanentFailure(testKey));
  }

  @Test
  @DisplayName("Should have unique error messages for each failure type")
  void testUniqueFailureMessages() {
    de.markusbordihn.easynpc.data.texture.TextureFailureType[] types =
        de.markusbordihn.easynpc.data.texture.TextureFailureType.values();
    java.util.Set<String> messages = new java.util.HashSet<>();

    for (de.markusbordihn.easynpc.data.texture.TextureFailureType type : types) {
      String message = type.getMessage();
      assertNotNull(message);
      assertFalse(message.isEmpty());
      assertTrue(messages.add(message), "Duplicate message found: " + message);
    }

    assertEquals(types.length, messages.size());
  }
}
