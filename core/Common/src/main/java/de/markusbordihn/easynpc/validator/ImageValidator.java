/*
 * Copyright 2022 Markus Bordihn
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

import com.mojang.blaze3d.platform.NativeImage;
import de.markusbordihn.easynpc.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ImageValidator {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ImageValidator() {}

  public static boolean isValidImage(NativeImage image) {
    if (image == null) {
      log.error("Found no valid image data in native image!");
      return false;
    }

    // Accept some edge case for image size like 48x32
    if (image.getWidth() == 48 && image.getHeight() == 32) {
      return true;
    }

    if (image.getWidth() < 32
        || image.getHeight() < 32
        || image.getWidth() % 32 != 0
        || image.getHeight() % 32 != 0) {
      log.error(
          "Unable to get any valid texture from native image, got {}x{}!",
          image.getWidth(),
          image.getHeight());
      return false;
    }

    return true;
  }
}
