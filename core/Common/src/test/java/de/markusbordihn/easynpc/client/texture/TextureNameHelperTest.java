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
import org.junit.jupiter.api.Test;

class TextureNameHelperTest {

  @Test
  void testGetResourceName_withNameAndType() {
    String result = TextureNameHelper.getResourceName("test-texture", "custom");
    assertNotNull(result);
    assertTrue(result.contains("custom"));
    assertTrue(result.contains("test-texture"));
    assertTrue(result.startsWith("easy_npc_client_texture_"));
    assertEquals(result, result.toLowerCase());
  }

  @Test
  void testGetResourceName_sanitizesInvalidCharacters() {
    String result = TextureNameHelper.getResourceName("test@#$%texture", "custom");
    assertFalse(result.contains("@"));
    assertFalse(result.contains("#"));
    assertFalse(result.contains("$"));
    assertFalse(result.contains("%"));
  }

  @Test
  void testGetFileName_fromUUID() {
    UUID testUUID = UUID.fromString("550e8400-e29b-41d4-a716-446655440000");
    String result = TextureNameHelper.getFileName(testUUID);
    assertEquals("550e8400-e29b-41d4-a716-446655440000.png", result);
  }

  @Test
  void testGetFileName_fromString() {
    String result = TextureNameHelper.getFileName("test-texture");
    assertEquals("test-texture.png", result);
  }

  @Test
  void testGetFileName_sanitizesInvalidCharacters() {
    String result = TextureNameHelper.getFileName("test@#$%texture");
    assertFalse(result.contains("@"));
    assertFalse(result.contains("#"));
    assertFalse(result.contains("$"));
    assertFalse(result.contains("%"));
    assertTrue(result.endsWith(".png"));
  }

  @Test
  void testGetFileName_preservesValidCharacters() {
    String result = TextureNameHelper.getFileName("test_texture-123.abc");
    assertTrue(result.contains("test_texture-123.abc"));
    assertTrue(result.endsWith(".png"));
  }
}
