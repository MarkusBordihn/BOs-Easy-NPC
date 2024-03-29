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
import de.markusbordihn.easynpc.utils.PlayersUtils;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.UUID;
import javax.imageio.ImageIO;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.texture.DynamicTexture;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String TEXTURE_PREFIX = Constants.MOD_ID + "_client_texture_";
  private static final String LOG_PREFIX = "[Texture Manager]";

  private static Path textureCachePath = null;

  private TextureManager() {}

  private static ResourceLocation registerTexture(TextureModelKey textureModelKey, File file) {
    // Using client Texture Manager
    Minecraft client = Minecraft.getInstance();
    net.minecraft.client.renderer.texture.TextureManager textureManager =
        client.getTextureManager();

    // Creative native image from file.
    NativeImage nativeImage =
        textureModelKey.getSkinModel() == SkinModel.HUMANOID
                || textureModelKey.getSkinModel() == SkinModel.HUMANOID_SLIM
            ? getNativePlayerImage(file)
            : getNativeImage(file);
    if (nativeImage == null) {
      log.error("{} Unable to create native image for file {}.", LOG_PREFIX, file);
      return null;
    }

    // Creative dynamic texture from native image.
    DynamicTexture dynamicTexture = new DynamicTexture(nativeImage);

    // Register dynamic texture under resource location.
    String resourceName = getResourceName(textureModelKey);
    ResourceLocation resourceLocation = textureManager.register(resourceName, dynamicTexture);
    log.debug(
        "{} Registered image {} as texture {} with {}.",
        LOG_PREFIX,
        nativeImage,
        dynamicTexture,
        resourceLocation);

    return resourceLocation;
  }

  public static ResourceLocation addCustomTexture(TextureModelKey textureModelKey, File file) {
    // Verify file to make sure it's not a directory, not null, exists and readable.
    if (file == null || !file.exists() || !file.canRead() || file.isDirectory()) {
      log.error("{} Texture file {} is invalid!", LOG_PREFIX, file);
      return null;
    }

    // Try to load the image from file.
    BufferedImage image;
    try {
      image = ImageIO.read(file);
    } catch (IllegalArgumentException | IOException exception) {
      log.error("{} Unable to load Texture file {} because of:", LOG_PREFIX, file, exception);
      return null;
    }

    // Verify the image data to make sure we got a valid image!
    if (image == null) {
      log.error("{} Unable to get any valid texture from file {}!", LOG_PREFIX, file);
      return null;
    } else if (image.getWidth() < 32
        || image.getHeight() < 32
        || image.getWidth() % 32 != 0
        || image.getHeight() % 32 != 0) {
      log.error(
          "{} Unable to get any valid texture from file {}, got {}x{}!",
          LOG_PREFIX,
          file,
          image.getWidth(),
          image.getHeight());
      return null;
    }

    // Adding file to texture manager.
    return TextureManager.registerTexture(textureModelKey, file);
  }

  public static ResourceLocation addRemoteTexture(
      TextureModelKey textureModelKey, String remoteUrl, String targetDirectory) {
    if (!PlayersUtils.isValidUrl(remoteUrl)) {
      log.error("{} Texture URL {} is invalid!", LOG_PREFIX, remoteUrl);
      return null;
    }

    // Check for cached textured.
    ResourceLocation cachedTexture = getCachedTexture(textureModelKey, targetDirectory);
    if (cachedTexture != null) {
      log.debug(
          "{} Found downloaded file in cache, will re-used {} for {}",
          LOG_PREFIX,
          cachedTexture,
          remoteUrl);
      return cachedTexture;
    }

    // Verify URL and follow redirect for 301 and 302, if needed.
    try {
      URL remoteImageURL = new URL(remoteUrl);
      HttpURLConnection connection = (HttpURLConnection) remoteImageURL.openConnection();
      if (connection.getResponseCode() == HttpURLConnection.HTTP_MOVED_PERM
          || connection.getResponseCode() == HttpURLConnection.HTTP_MOVED_TEMP) {
        String redirectUrl = connection.getHeaderField("Location");
        log.debug("{} Following redirect from {} > {}", LOG_PREFIX, remoteUrl, redirectUrl);
        remoteUrl = redirectUrl;
      } else if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
        log.error(
            "{} Unable to load texture from URL {} because of: {}",
            LOG_PREFIX,
            remoteUrl,
            connection.getResponseMessage());
        return null;
      }
    } catch (IllegalArgumentException | IOException exception) {
      log.error(
          "{} Unable to load texture from URL {} because of:", LOG_PREFIX, remoteUrl, exception);
      return null;
    }

    // Download URL to memory
    BufferedImage image;
    try {
      image = ImageIO.read(new URL(remoteUrl));
    } catch (IllegalArgumentException | IOException exception) {
      log.error("{} Unable to parse image from {} because of:", LOG_PREFIX, remoteUrl, exception);
      return null;
    }

    // Verify the image data to make sure we got a valid image!
    if (image == null) {
      log.error("{} Unable to get any valid texture from {}!", LOG_PREFIX, remoteUrl);
      return null;
    } else if (image.getWidth() < 32
        || image.getHeight() < 32
        || image.getWidth() % 32 != 0
        || image.getHeight() % 32 != 0) {
      log.error(
          "{} Unable to get any valid texture from {}, got {}x{}!",
          LOG_PREFIX,
          remoteUrl,
          image.getWidth(),
          image.getHeight());
      return null;
    }

    // Storing file to cache.
    UUID uuid = textureModelKey.getUUID();
    File file = new File(targetDirectory, getFileName(uuid));
    try {
      ImageIO.write(image, "png", file);
    } catch (IllegalArgumentException | IOException exception) {
      log.error(
          "{} Unable to store texture from {} to {} because of:",
          LOG_PREFIX,
          remoteUrl,
          file,
          exception);
      return null;
    }

    // Adding file to texture manager.
    return registerTexture(textureModelKey, file);
  }

  public static String getResourceName(TextureModelKey textureModelKey) {
    return getResourceName(textureModelKey.getUUID().toString(), textureModelKey.getSubType());
  }

  public static String getResourceName(String name, String type) {
    return (TEXTURE_PREFIX + type + "_" + name.replaceAll("[^a-z0-9_.-]", "")).toLowerCase();
  }

  public static String getFileName(UUID uuid) {
    return getFileName(uuid.toString());
  }

  public static String getFileName(String name) {
    return name.replaceAll("[^a-z0-9_.-]", "") + ".png";
  }

  public static ResourceLocation getCachedTexture(
      TextureModelKey textureModelKey, String targetDirectory) {
    String fileName = String.format("%s.png", textureModelKey.getUUID());
    File file = new File(targetDirectory, fileName);
    if (file.exists()) {
      log.debug(
          "{} Found downloaded file in cache, will re-used file {} for {}",
          LOG_PREFIX,
          file,
          textureModelKey);
      return registerTexture(textureModelKey, file);
    }
    return null;
  }

  public static Path getTextureCacheDirectory() {
    if (textureCachePath == null) {
      Path cacheDirectory =
          Paths.get(Constants.GAME_DIR.resolve(Constants.MOD_ID).toString(), "texture_cache");
      if (!cacheDirectory.toFile().exists()) {
        log.info("{} Creating texture cache directory at {}", LOG_PREFIX, cacheDirectory);
        try {
          Files.createDirectories(cacheDirectory);
        } catch (IOException exception) {
          log.error(
              "{} Unable to create texture cache directory at {} because of:",
              LOG_PREFIX,
              cacheDirectory,
              exception);
          return null;
        }
      }
      textureCachePath = cacheDirectory;
    }
    return textureCachePath;
  }

  public static NativeImage getNativeImage(File file) {
    return getNativeImage(file, false);
  }

  public static NativeImage getNativePlayerImage(File file) {
    return getNativeImage(file, true);
  }

  public static NativeImage getNativeImage(File file, boolean legacySupport) {
    NativeImage nativeImage;
    try {
      InputStream inputStream = new FileInputStream(file);
      nativeImage = NativeImage.read(inputStream);
      inputStream.close();
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
