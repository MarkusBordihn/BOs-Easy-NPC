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
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.validator.ImageValidator;
import java.io.File;
import java.nio.file.Path;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Manager]";

  private TextureManager() {}

  public static ResourceLocation addCustomTexture(TextureModelKey textureModelKey, File file) {
    // Verify texture model key.
    if (textureModelKey == null) {
      log.error("{} Texture model key for {} is invalid!", LOG_PREFIX, file);
      return null;
    }

    // Verify file to make sure it's not a directory, not null, exists and readable.
    if (file == null || !file.exists() || !file.canRead() || file.isDirectory()) {
      log.error("{} Texture file {} is invalid!", LOG_PREFIX, file);
      return null;
    }

    // Try to load the image from file.
    NativeImage nativeImage =
        textureModelKey.getSkinModel() == SkinModel.HUMANOID
                || textureModelKey.getSkinModel() == SkinModel.HUMANOID_SLIM
            ? TextureImageLoader.getNativePlayerImage(file)
            : TextureImageLoader.getNativeImage(file);
    if (nativeImage == null) {
      log.error(
          "{} Unable to load Texture file {} for {} because of I/O error",
          LOG_PREFIX,
          file,
          textureModelKey);
      return null;
    }

    // Verify the image data to make sure we got a valid image!
    if (!ImageValidator.isValidImage(nativeImage)) {
      log.error(
          "{} Unable to get any valid texture from file {} for {}!",
          LOG_PREFIX,
          file,
          textureModelKey);
      nativeImage.close();
      return null;
    }

    // Adding file to texture manager.
    return TextureRegistrationHelper.registerTexture(textureModelKey, nativeImage);
  }

  public static ResourceLocation addRemoteTexture(
      TextureModelKey textureModelKey, String remoteUrl, Path targetDirectory) {
    return RemoteTextureLoader.loadRemoteTexture(textureModelKey, remoteUrl, targetDirectory);
  }

  public static String getResourceName(TextureModelKey textureModelKey) {
    return TextureNameHelper.getResourceName(textureModelKey);
  }

  public static String getResourceName(String name, String type) {
    return TextureNameHelper.getResourceName(name, type);
  }

  public static String getFileName(UUID uuid) {
    return TextureNameHelper.getFileName(uuid);
  }

  public static String getFileName(String name) {
    return TextureNameHelper.getFileName(name);
  }

  public static ResourceLocation getCachedTexture(
      TextureModelKey textureModelKey, Path targetDirectory) {
    return TextureCacheManager.getCachedTexture(textureModelKey, targetDirectory);
  }

  public static ResourceLocation searchCachedTexture(
      TextureModelKey textureModelKey, Path targetDirectory) {
    return TextureCacheManager.searchCachedTexture(textureModelKey, targetDirectory);
  }

  public static TextureModelKey getTextureModelKey(SkinModel skinModel, File textureFile) {
    return TextureCacheManager.getTextureModelKey(skinModel, textureFile);
  }

  public static UUID getUUIDFromFilename(String fileName) {
    return TextureCacheManager.getUUIDFromFilename(fileName);
  }

  public static boolean hasLastErrorMessage() {
    return TextureErrorHandler.hasLastErrorMessage();
  }

  public static String getLastErrorMessage() {
    return TextureErrorHandler.getLastErrorMessage();
  }

  public static void clearLastErrorMessage() {
    TextureErrorHandler.clearLastErrorMessage();
  }
}
