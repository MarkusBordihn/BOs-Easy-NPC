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

package de.markusbordihn.easynpc.client.texture;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.mojang.blaze3d.platform.NativeImage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TextureImageLoaderTest {

  private static final int TRANSPARENT = 0x00000000;
  private static final int OPAQUE_WHITE = 0xFFFFFFFF;

  private static NativeImage newSkin(int width, int height) {
    NativeImage nativeImage = new NativeImage(width, height, true);
    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        nativeImage.setPixelRGBA(x, y, OPAQUE_WHITE);
      }
    }
    return nativeImage;
  }

  private static int alphaAt(NativeImage nativeImage, int x, int y) {
    return nativeImage.getPixelRGBA(x, y) >> 24 & 0xFF;
  }

  @Test
  @DisplayName("Should keep transparent pixels of modern skins for resource pack animations")
  void testKeepTransparencyOfModernSkin() {
    NativeImage nativeImage = newSkin(64, 64);
    nativeImage.setPixelRGBA(2, 2, TRANSPARENT);
    nativeImage.setPixelRGBA(10, 20, TRANSPARENT);

    NativeImage processedImage = TextureImageLoader.processPlayerSkin(nativeImage);

    assertEquals(0, alphaAt(processedImage, 2, 2));
    assertEquals(0, alphaAt(processedImage, 10, 20));
    processedImage.close();
  }

  @Test
  @DisplayName("Should convert legacy skins to 64x64 with an opaque base layer")
  void testConvertLegacySkin() {
    NativeImage nativeImage = newSkin(64, 32);
    nativeImage.setPixelRGBA(2, 2, TRANSPARENT);
    nativeImage.setPixelRGBA(10, 20, TRANSPARENT);

    NativeImage processedImage = TextureImageLoader.processPlayerSkin(nativeImage);

    assertEquals(64, processedImage.getWidth());
    assertEquals(64, processedImage.getHeight());
    assertEquals(255, alphaAt(processedImage, 2, 2));
    assertEquals(255, alphaAt(processedImage, 10, 20));
    processedImage.close();
  }
}
