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

@DisplayName("TextureFailureInfo Tests")
class TextureFailureInfoTest {

  @Test
  @DisplayName("Should create failure info with automatic timestamp")
  void testCreateWithAutoTimestamp() {
    long before = System.currentTimeMillis();
    TextureFailureInfo info =
        new TextureFailureInfo(
            TextureFailureType.INVALID_IMAGE_SIZE, "Test details", "http://test.url");
    long after = System.currentTimeMillis();

    assertNotNull(info);
    assertEquals(TextureFailureType.INVALID_IMAGE_SIZE, info.type());
    assertEquals("Test details", info.details());
    assertEquals("http://test.url", info.url());
    assertTrue(info.timestamp() >= before && info.timestamp() <= after);
  }

  @Test
  @DisplayName("Should create failure info with explicit timestamp")
  void testCreateWithExplicitTimestamp() {
    long timestamp = 1000000L;
    TextureFailureInfo info =
        new TextureFailureInfo(
            TextureFailureType.DECODING_ERROR, timestamp, "Test details", "http://test.url");

    assertEquals(timestamp, info.timestamp());
    assertEquals(TextureFailureType.DECODING_ERROR, info.type());
  }

  @Test
  @DisplayName("Should detect expired failure info")
  void testIsExpired() {
    long oldTimestamp = System.currentTimeMillis() - 1000;
    TextureFailureInfo info =
        new TextureFailureInfo(
            TextureFailureType.NETWORK_ERROR, oldTimestamp, "Test details", "http://test.url");

    assertTrue(info.isExpired(500));
    assertFalse(info.isExpired(2000));
  }

  @Test
  @DisplayName("Should not be expired when maxAge not reached")
  void testIsNotExpired() {
    TextureFailureInfo info =
        new TextureFailureInfo(
            TextureFailureType.FILE_TOO_LARGE, "Test details", "http://test.url");

    assertFalse(info.isExpired(10000));
  }

  @Test
  @DisplayName("Should handle different failure types")
  void testDifferentFailureTypes() {
    TextureFailureInfo info1 =
        new TextureFailureInfo(
            TextureFailureType.INVALID_IMAGE_SIZE, "Details 1", "http://test1.url");
    TextureFailureInfo info2 =
        new TextureFailureInfo(TextureFailureType.TIMEOUT, "Details 2", "http://test2.url");

    assertEquals(TextureFailureType.INVALID_IMAGE_SIZE, info1.type());
    assertEquals(TextureFailureType.TIMEOUT, info2.type());
    assertNotEquals(info1.type(), info2.type());
  }
}
