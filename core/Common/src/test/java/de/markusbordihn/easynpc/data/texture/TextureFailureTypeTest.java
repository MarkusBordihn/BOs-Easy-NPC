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

package de.markusbordihn.easynpc.data.texture;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("TextureFailureType Tests")
class TextureFailureTypeTest {

  @Test
  @DisplayName("Should mark INVALID_IMAGE_SIZE as permanent")
  void testInvalidImageSizeIsPermanent() {
    assertTrue(TextureFailureType.INVALID_IMAGE_SIZE.isPermanent());
    assertNotNull(TextureFailureType.INVALID_IMAGE_SIZE.getMessage());
  }

  @Test
  @DisplayName("Should mark DECODING_ERROR as permanent")
  void testDecodingErrorIsPermanent() {
    assertTrue(TextureFailureType.DECODING_ERROR.isPermanent());
    assertNotNull(TextureFailureType.DECODING_ERROR.getMessage());
  }

  @Test
  @DisplayName("Should mark INVALID_FORMAT as permanent")
  void testInvalidFormatIsPermanent() {
    assertTrue(TextureFailureType.INVALID_FORMAT.isPermanent());
    assertNotNull(TextureFailureType.INVALID_FORMAT.getMessage());
  }

  @Test
  @DisplayName("Should mark FILE_TOO_LARGE as permanent")
  void testFileTooLargeIsPermanent() {
    assertTrue(TextureFailureType.FILE_TOO_LARGE.isPermanent());
    assertNotNull(TextureFailureType.FILE_TOO_LARGE.getMessage());
  }

  @Test
  @DisplayName("Should mark URL_INVALID as permanent")
  void testUrlInvalidIsPermanent() {
    assertTrue(TextureFailureType.URL_INVALID.isPermanent());
    assertNotNull(TextureFailureType.URL_INVALID.getMessage());
  }

  @Test
  @DisplayName("Should mark MAX_RETRIES_EXCEEDED as permanent")
  void testMaxRetriesExceededIsPermanent() {
    assertTrue(TextureFailureType.MAX_RETRIES_EXCEEDED.isPermanent());
    assertNotNull(TextureFailureType.MAX_RETRIES_EXCEEDED.getMessage());
  }

  @Test
  @DisplayName("Should mark NETWORK_ERROR as non-permanent")
  void testNetworkErrorIsNotPermanent() {
    assertFalse(TextureFailureType.NETWORK_ERROR.isPermanent());
    assertNotNull(TextureFailureType.NETWORK_ERROR.getMessage());
  }

  @Test
  @DisplayName("Should mark TIMEOUT as non-permanent")
  void testTimeoutIsNotPermanent() {
    assertFalse(TextureFailureType.TIMEOUT.isPermanent());
    assertNotNull(TextureFailureType.TIMEOUT.getMessage());
  }

  @Test
  @DisplayName("Should have unique messages for all failure types")
  void testUniqueMessages() {
    TextureFailureType[] types = TextureFailureType.values();
    java.util.Set<String> messages = new java.util.HashSet<>();

    for (TextureFailureType type : types) {
      String message = type.getMessage();
      assertNotNull(message);
      assertFalse(message.isEmpty());
      assertTrue(messages.add(message), "Duplicate message: " + message);
    }

    assertEquals(types.length, messages.size());
  }

  @Test
  @DisplayName("Should have all expected failure types")
  void testAllFailureTypesPresent() {
    TextureFailureType[] types = TextureFailureType.values();
    assertEquals(8, types.length);

    assertNotNull(TextureFailureType.valueOf("INVALID_IMAGE_SIZE"));
    assertNotNull(TextureFailureType.valueOf("DECODING_ERROR"));
    assertNotNull(TextureFailureType.valueOf("INVALID_FORMAT"));
    assertNotNull(TextureFailureType.valueOf("FILE_TOO_LARGE"));
    assertNotNull(TextureFailureType.valueOf("NETWORK_ERROR"));
    assertNotNull(TextureFailureType.valueOf("URL_INVALID"));
    assertNotNull(TextureFailureType.valueOf("TIMEOUT"));
    assertNotNull(TextureFailureType.valueOf("MAX_RETRIES_EXCEEDED"));
  }

  @Test
  @DisplayName("Should have meaningful error messages")
  void testMeaningfulMessages() {
    assertTrue(
        TextureFailureType.INVALID_IMAGE_SIZE.getMessage().toLowerCase().contains("dimension")
            || TextureFailureType.INVALID_IMAGE_SIZE.getMessage().toLowerCase().contains("size")
            || TextureFailureType.INVALID_IMAGE_SIZE
                .getMessage()
                .toLowerCase()
                .contains("invalid"));
    assertTrue(
        TextureFailureType.DECODING_ERROR.getMessage().toLowerCase().contains("decode")
            || TextureFailureType.DECODING_ERROR.getMessage().toLowerCase().contains("failed"));
    assertTrue(
        TextureFailureType.FILE_TOO_LARGE.getMessage().toLowerCase().contains("size")
            || TextureFailureType.FILE_TOO_LARGE.getMessage().toLowerCase().contains("large")
            || TextureFailureType.FILE_TOO_LARGE.getMessage().toLowerCase().contains("limit"));
    assertTrue(
        TextureFailureType.NETWORK_ERROR.getMessage().toLowerCase().contains("network")
            || TextureFailureType.NETWORK_ERROR.getMessage().toLowerCase().contains("connection"));
    assertTrue(
        TextureFailureType.URL_INVALID.getMessage().toLowerCase().contains("url")
            || TextureFailureType.URL_INVALID.getMessage().toLowerCase().contains("invalid"));
    assertTrue(
        TextureFailureType.TIMEOUT.getMessage().toLowerCase().contains("timeout")
            || TextureFailureType.TIMEOUT.getMessage().toLowerCase().contains("time"));
    assertTrue(
        TextureFailureType.MAX_RETRIES_EXCEEDED.getMessage().toLowerCase().contains("retry")
            || TextureFailureType.MAX_RETRIES_EXCEEDED
                .getMessage()
                .toLowerCase()
                .contains("attempt")
            || TextureFailureType.MAX_RETRIES_EXCEEDED
                .getMessage()
                .toLowerCase()
                .contains("exceed"));
  }
}
