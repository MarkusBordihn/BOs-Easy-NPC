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
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("TextureErrorHandler Tests")
class TextureErrorHandlerTest {

  @Test
  @DisplayName("Should not have error message initially")
  void testInitialState() {
    TextureErrorHandler.clearLastErrorMessage();
    assertFalse(TextureErrorHandler.hasLastErrorMessage());
    assertNull(TextureErrorHandler.getLastErrorMessage());
  }

  @Test
  @DisplayName("Should store processing error message")
  void testProcessingErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
    TextureModelKey key =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID, "test");

    TextureErrorHandler.processingErrorMessage(
        key, "http://example.com/texture.png", "Invalid format");

    assertTrue(TextureErrorHandler.hasLastErrorMessage());
    assertNotNull(TextureErrorHandler.getLastErrorMessage());
    assertTrue(TextureErrorHandler.getLastErrorMessage().contains("Unable to process texture"));
    assertTrue(
        TextureErrorHandler.getLastErrorMessage().contains("http://example.com/texture.png"));
    assertTrue(TextureErrorHandler.getLastErrorMessage().contains("Invalid format"));
  }

  @Test
  @DisplayName("Should store URL load error message")
  void testUrlLoadErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
    TextureModelKey key =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID, "test");

    TextureErrorHandler.urlLoadErrorMessage(
        key, "http://example.com/texture.png", "Connection timeout");

    assertTrue(TextureErrorHandler.hasLastErrorMessage());
    assertNotNull(TextureErrorHandler.getLastErrorMessage());
    assertTrue(TextureErrorHandler.getLastErrorMessage().contains("Unable to load texture"));
    assertTrue(
        TextureErrorHandler.getLastErrorMessage().contains("http://example.com/texture.png"));
    assertTrue(TextureErrorHandler.getLastErrorMessage().contains("Connection timeout"));
  }

  @Test
  @DisplayName("Should clear error message")
  void testClearErrorMessage() {
    TextureModelKey key =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID, "test");
    TextureErrorHandler.processingErrorMessage(key, "http://example.com", "Test error");

    assertTrue(TextureErrorHandler.hasLastErrorMessage());

    TextureErrorHandler.clearLastErrorMessage();

    assertFalse(TextureErrorHandler.hasLastErrorMessage());
    assertNull(TextureErrorHandler.getLastErrorMessage());
  }

  @Test
  @DisplayName("Should overwrite previous error message")
  void testOverwriteErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
    TextureModelKey key1 =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID, "test1");
    TextureModelKey key2 =
        new TextureModelKey(
            UUID.randomUUID(), de.markusbordihn.easynpc.data.skin.SkinModel.HUMANOID, "test2");

    TextureErrorHandler.processingErrorMessage(key1, "http://example.com/1", "First error");
    String firstMessage = TextureErrorHandler.getLastErrorMessage();

    TextureErrorHandler.urlLoadErrorMessage(key2, "http://example.com/2", "Second error");
    String secondMessage = TextureErrorHandler.getLastErrorMessage();

    assertNotEquals(firstMessage, secondMessage);
    assertTrue(secondMessage.contains("Second error"));
    assertFalse(secondMessage.contains("First error"));
  }
}
