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
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TextureErrorHandlerTest {

  private static final String TEXTURE_URL_NOT_LOADED_BY_OTHER_TESTS =
      "http://example.org/texture_error_handler.png";

  @Test
  @DisplayName("Should not have error message initially")
  void testInitialState() {
    TextureErrorHandler.clearLastErrorMessage();
    assertFalse(TextureErrorHandler.hasLastErrorMessage());
    assertNull(TextureErrorHandler.getLastErrorMessage());
  }

  @Test
  void testProcessingErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID, "test");

    TextureErrorHandler.processingErrorMessage(
        key, TEXTURE_URL_NOT_LOADED_BY_OTHER_TESTS, "Invalid format");
    String errorMessage = TextureErrorHandler.getLastErrorMessage();

    assertNotNull(errorMessage);
    assertTrue(errorMessage.contains("Unable to process texture"));
    assertTrue(errorMessage.contains(TEXTURE_URL_NOT_LOADED_BY_OTHER_TESTS));
    assertTrue(errorMessage.contains("Invalid format"));
  }

  @Test
  void testUrlLoadErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID, "test");

    TextureErrorHandler.urlLoadErrorMessage(
        key, TEXTURE_URL_NOT_LOADED_BY_OTHER_TESTS, "Connection timeout");
    String errorMessage = TextureErrorHandler.getLastErrorMessage();

    assertNotNull(errorMessage);
    assertTrue(errorMessage.contains("Unable to load texture"));
    assertTrue(errorMessage.contains(TEXTURE_URL_NOT_LOADED_BY_OTHER_TESTS));
    assertTrue(errorMessage.contains("Connection timeout"));
  }

  @Test
  void testClearErrorMessage() {
    TextureModelKey key = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID, "test");
    TextureErrorHandler.processingErrorMessage(key, "http://example.com", "Test error");

    assertTrue(TextureErrorHandler.hasLastErrorMessage());

    TextureErrorHandler.clearLastErrorMessage();

    assertFalse(TextureErrorHandler.hasLastErrorMessage());
    assertNull(TextureErrorHandler.getLastErrorMessage());
  }

  @Test
  void testOverwriteErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
    TextureModelKey key1 = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID, "test1");
    TextureModelKey key2 = new TextureModelKey(UUID.randomUUID(), SkinModel.HUMANOID, "test2");

    TextureErrorHandler.processingErrorMessage(key1, "http://example.com/1", "First error");
    String firstMessage = TextureErrorHandler.getLastErrorMessage();

    TextureErrorHandler.urlLoadErrorMessage(key2, "http://example.com/2", "Second error");
    String secondMessage = TextureErrorHandler.getLastErrorMessage();

    assertNotEquals(firstMessage, secondMessage);
    assertTrue(secondMessage.contains("Second error"));
    assertFalse(secondMessage.contains("First error"));
  }
}
