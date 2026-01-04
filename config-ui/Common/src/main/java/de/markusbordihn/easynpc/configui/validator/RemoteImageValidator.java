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

package de.markusbordihn.easynpc.configui.validator;

import de.markusbordihn.easynpc.Constants;
import java.awt.image.BufferedImage;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import javax.imageio.IIOException;
import javax.imageio.ImageIO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RemoteImageValidator {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private RemoteImageValidator() {}

  public static boolean isValidImage(URL remoteUrl) {

    if (remoteUrl.toString().endsWith(".webp")) {
      log.error("WebP images are not supported, please use PNG images!");
      return false;
    }

    BufferedImage image;
    try {
      image = ImageIO.read(remoteUrl);
    } catch (IIOException iioException) {
      log.error("Unable to download image from URL {}: {}", remoteUrl, iioException.getMessage());
      return false;
    } catch (FileNotFoundException fileNotFoundException) {
      log.error("Image not found at URL {}: {}", remoteUrl, fileNotFoundException.getMessage());
      return false;
    } catch (IllegalArgumentException | IOException exception) {
      log.error("Unable to get any valid image from URL {}: {}", remoteUrl, exception.getMessage());
      return false;
    } catch (Exception exception) {
      log.error(
          "Unexpected error loading image from URL {}: {}", remoteUrl, exception.getMessage());
      return false;
    }

    if (image == null) {
      log.error("Unable to get any valid image from URL {}!", remoteUrl);
      return false;
    }

    return isValidImageSize(image);
  }

  private static boolean isValidImageSize(BufferedImage image) {
    if (image == null) {
      return false;
    }

    if (image.getWidth() == 48 && image.getHeight() == 32) {
      return true;
    }

    if (image.getWidth() < 32
        || image.getHeight() < 32
        || image.getWidth() % 32 != 0
        || image.getHeight() % 32 != 0) {
      log.error(
          "Invalid image size {}x{}, must be at least 32x32 and multiple of 32!",
          image.getWidth(),
          image.getHeight());
      return false;
    }

    return true;
  }
}
