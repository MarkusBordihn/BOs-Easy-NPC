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

package de.markusbordihn.easynpc.configui.validator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URL;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class RemoteImageValidatorTest {

  private static final String IMAGE_PATH = "/de/markusbordihn/easynpc/configui/validator/images/";

  private static URL resource(String fileName) {
    URL url = RemoteImageValidatorTest.class.getResource(IMAGE_PATH + fileName);
    if (url == null) {
      throw new IllegalArgumentException("Missing test resource " + fileName);
    }
    return url;
  }

  @ParameterizedTest
  @ValueSource(strings = {"valid_32x32.png", "valid_48x32.png", "valid_64x64.png"})
  void testValidImages(String fileName) {
    assertTrue(RemoteImageValidator.isValidImage(resource(fileName)));
  }

  @ParameterizedTest
  @ValueSource(
      strings = {
        "invalid_16x16.png",
        "invalid_33x32.png",
        "broken_image.png",
        "unsupported.webp",
        "missing.png"
      })
  void testInvalidImages(String fileName) throws Exception {
    URL url =
        fileName.equals("missing.png")
            ? new URL(resource("valid_32x32.png"), fileName)
            : resource(fileName);

    assertFalse(RemoteImageValidator.isValidImage(url));
  }
}
