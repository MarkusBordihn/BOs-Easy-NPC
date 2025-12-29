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
import java.io.File;
import java.nio.file.Path;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureCacheManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Texture Cache Manager]";
  private static final String FILE_EXTENSION_PNG = ".png";

  private TextureCacheManager() {}

  public static ResourceLocation getCachedTexture(
      TextureModelKey textureModelKey, Path targetDirectory) {
    String fileName = String.format("%s.png", textureModelKey.getUUID());
    File file = targetDirectory.resolve(fileName).toFile();
    if (file.exists()) {
      log.info(
          "{} Found texture file in cache, will re-used file {} for {}",
          LOG_PREFIX,
          file,
          textureModelKey);
      NativeImage nativeImage =
          textureModelKey.getSkinModel() == SkinModel.HUMANOID
                  || textureModelKey.getSkinModel() == SkinModel.HUMANOID_SLIM
              ? TextureImageLoader.getNativePlayerImage(file)
              : TextureImageLoader.getNativeImage(file);
      if (nativeImage == null) {
        return null;
      }
      return TextureRegistrationHelper.registerTexture(textureModelKey, nativeImage);
    }
    return null;
  }

  public static ResourceLocation searchCachedTexture(
      TextureModelKey textureModelKey, Path targetDirectory) {
    // Check for cached texture and return if found.
    ResourceLocation resourceLocation = getCachedTexture(textureModelKey, targetDirectory);
    if (resourceLocation != null) {
      return resourceLocation;
    }

    // Search for a matching texture file in cache directory.
    UUID textureUUID = textureModelKey.getUUID();
    File[] files = targetDirectory.toFile().listFiles();
    if (files == null) {
      log.warn("{} Unable to list files in cache directory {}", LOG_PREFIX, targetDirectory);
      return null;
    }

    for (File file : files) {
      if (file != null) {
        String filename = file.getName();
        UUID uuid = getUUIDFromFilename(filename);
        if (textureUUID.equals(uuid)) {
          NativeImage nativeImage =
              textureModelKey.getSkinModel() == SkinModel.HUMANOID
                      || textureModelKey.getSkinModel() == SkinModel.HUMANOID_SLIM
                  ? TextureImageLoader.getNativePlayerImage(file)
                  : TextureImageLoader.getNativeImage(file);
          if (nativeImage == null) {
            log.error(
                "{} Unable to load native image from cached texture file {} for {}",
                LOG_PREFIX,
                file,
                textureModelKey);
            return null;
          }
          ResourceLocation textureResourceLocation =
              TextureRegistrationHelper.registerTexture(textureModelKey, nativeImage);
          if (textureResourceLocation != null) {
            log.info(
                "{} Registered cached texture file {} for {} with {}",
                LOG_PREFIX,
                file,
                textureModelKey,
                textureResourceLocation);
          } else {
            log.error(
                "{} Unable to register cached texture file {} for {}",
                LOG_PREFIX,
                file,
                textureModelKey);
          }
          return textureResourceLocation;
        }
      }
    }
    log.warn(
        "{} Unable to find any cached texture file for {} in {}",
        LOG_PREFIX,
        textureModelKey,
        targetDirectory);
    return null;
  }

  public static TextureModelKey getTextureModelKey(SkinModel skinModel, File textureFile) {
    String filename = textureFile.getName();
    UUID uuid = getUUIDFromFilename(filename);
    if (uuid == null) {
      log.error(
          "{} Unable to get UUID for {} and texture file {}!", LOG_PREFIX, skinModel, filename);
      return null;
    }
    return new TextureModelKey(uuid, skinModel, filename);
  }

  public static UUID getUUIDFromFilename(String fileName) {
    if (fileName == null || fileName.isEmpty()) {
      return null;
    }
    if (!fileName.endsWith(FILE_EXTENSION_PNG)) {
      log.error("{} Unable to get UUID from invalid file name {}!", LOG_PREFIX, fileName);
      return null;
    }
    try {
      return UUID.fromString(fileName.substring(0, fileName.indexOf('.')));
    } catch (IllegalArgumentException e) {
      return UUID.nameUUIDFromBytes(fileName.getBytes());
    }
  }
}
