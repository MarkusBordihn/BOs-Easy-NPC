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

import de.markusbordihn.easynpc.Constants;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import javax.imageio.ImageIO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ImageValidator {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ImageValidator() {}

  public static boolean isValidImage(URL remoteUrl) {

    // Check if remote url image is a webp image
    if (remoteUrl.toString().endsWith(".webp")) {
      log.error("WebP images are not supported, please use PNG images!");
    }

    // Load image from remote URL
    BufferedImage image;
    try {
      image = ImageIO.read(remoteUrl);
    } catch (IllegalArgumentException | IOException exception) {
      log.error("Unable to get any valid image from URL {}!", remoteUrl);
      return false;
    }

    // Verify the image data to make sure we got a valid image!
    if (image == null) {
      log.error("Unable to get any valid image from URL {}!", remoteUrl);

      // Load url and first line of file and check if it includes "WEBP" to detect webp images

      return false;
    }

    return isValidImage(image);
  }

  public static boolean isValidImage(BufferedImage image) {
    // Verify the image data to make sure we got a valid image!
    if (image == null) {
      log.error("Found no valid image data in buffer!");
      return false;
    }

    // Verify the image size needs to be at least 32x32 and a multiple of 32!
    if (image.getWidth() < 32
        || image.getHeight() < 32
        || image.getWidth() % 32 != 0
        || image.getHeight() % 32 != 0) {
      log.error(
          "Unable to get any valid texture from image {}, got {}x{}!",
          image,
          image.getWidth(),
          image.getHeight());
      return false;
    }

    return true;
  }
}
