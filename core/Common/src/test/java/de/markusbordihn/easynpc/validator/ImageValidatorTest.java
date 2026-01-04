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

package de.markusbordihn.easynpc.validator;

import static org.junit.jupiter.api.Assertions.*;

import com.mojang.blaze3d.platform.NativeImage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@DisplayName("ImageValidator Tests")
class ImageValidatorTest {

  @Test
  @DisplayName("Should reject null image")
  void testNullImage() {
    assertFalse(ImageValidator.isValidImage(null));
  }

  @ParameterizedTest
  @DisplayName("Should accept valid image dimensions")
  @CsvSource({
    "64,64", "64,32", "128,128", "256,256", "32,32", "96,96", "48,32", "160,160", "192,192",
    "224,224"
  })
  void testValidImageDimensions(int width, int height) {
    NativeImage image = new NativeImage(width, height, false);
    assertTrue(ImageValidator.isValidImage(image));
    image.close();
  }

  @ParameterizedTest
  @DisplayName("Should reject invalid image dimensions")
  @CsvSource({
    "16,16", "31,31", "33,32", "64,33", "100,100", "64,48", "48,64", "24,24", "8,8", "65,65",
    "63,64", "64,63"
  })
  void testInvalidImageDimensions(int width, int height) {
    NativeImage image = new NativeImage(width, height, false);
    assertFalse(ImageValidator.isValidImage(image));
    image.close();
  }
}
