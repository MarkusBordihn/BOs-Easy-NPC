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

package de.markusbordihn.easynpc.client.texture;

import com.mojang.blaze3d.platform.NativeImage;
import de.markusbordihn.easynpc.Constants;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureImageLoader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Image Loader]";

  private TextureImageLoader() {}

  public static NativeImage getNativeImage(File file) {
    return getNativeImage(file, false);
  }

  public static NativeImage getNativePlayerImage(File file) {
    return getNativeImage(file, true);
  }

  public static NativeImage getNativeImage(File file, boolean legacySupport) {
    NativeImage nativeImage;
    try (InputStream inputStream = new FileInputStream(file)) {
      nativeImage = NativeImage.read(inputStream);
    } catch (Exception exception) {
      log.error(
          "{} Unable to get native image for file {} because of:", LOG_PREFIX, file, exception);
      return null;
    }

    if (legacySupport && nativeImage.getWidth() == 64 && nativeImage.getHeight() == 32) {
      log.info("{} Processing legacy image {} from 64x32 to 64x64 ...", LOG_PREFIX, nativeImage);
      nativeImage = getNativeImageFromLegacyImage(nativeImage);
    }

    return nativeImage;
  }

  public static NativeImage getNativeImageFromLegacyImage(NativeImage legacyNativeImage) {
    NativeImage nativeImage = new NativeImage(64, 64, true);
    nativeImage.copyFrom(legacyNativeImage);
    legacyNativeImage.close();
    nativeImage.fillRect(0, 32, 64, 32, 0);
    nativeImage.copyRect(4, 16, 16, 32, 4, 4, true, false);
    nativeImage.copyRect(8, 16, 16, 32, 4, 4, true, false);
    nativeImage.copyRect(0, 20, 24, 32, 4, 12, true, false);
    nativeImage.copyRect(4, 20, 16, 32, 4, 12, true, false);
    nativeImage.copyRect(8, 20, 8, 32, 4, 12, true, false);
    nativeImage.copyRect(12, 20, 16, 32, 4, 12, true, false);
    nativeImage.copyRect(44, 16, -8, 32, 4, 4, true, false);
    nativeImage.copyRect(48, 16, -8, 32, 4, 4, true, false);
    nativeImage.copyRect(40, 20, 0, 32, 4, 12, true, false);
    nativeImage.copyRect(44, 20, -8, 32, 4, 12, true, false);
    nativeImage.copyRect(48, 20, -16, 32, 4, 12, true, false);
    nativeImage.copyRect(52, 20, -8, 32, 4, 12, true, false);
    return nativeImage;
  }
}
