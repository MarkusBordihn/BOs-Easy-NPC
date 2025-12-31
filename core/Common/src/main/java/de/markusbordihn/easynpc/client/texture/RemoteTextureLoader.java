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
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Path;
import java.util.UUID;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RemoteTextureLoader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Remote Texture Loader]";

  private RemoteTextureLoader() {}

  public static Identifier loadRemoteTexture(
      TextureModelKey textureModelKey, String remoteUrl, Path targetDirectory) {
    if (!UrlValidator.isValidUrl(remoteUrl)) {
      TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, "Invalid URL");
      return null;
    }

    // Check for cached texture.
    Identifier cachedTexture =
        TextureCacheManager.getCachedTexture(textureModelKey, targetDirectory);
    if (cachedTexture != null) {
      log.info(
          "{} Found downloaded file in cache, will re-used {} for {}",
          LOG_PREFIX,
          cachedTexture,
          remoteUrl);
      return cachedTexture;
    }

    // Log the start of the download process
    log.warn(
        "{} Starting download of remote texture from {} for {}",
        LOG_PREFIX,
        remoteUrl,
        textureModelKey);

    // Verify URL and follow redirect for 301 and 302, if needed.
    try {
      URL remoteImageURL = new URL(remoteUrl);
      HttpURLConnection connection = (HttpURLConnection) remoteImageURL.openConnection();
      if (connection.getResponseCode() == HttpURLConnection.HTTP_MOVED_PERM
          || connection.getResponseCode() == HttpURLConnection.HTTP_MOVED_TEMP) {
        String redirectUrl = connection.getHeaderField("Location");
        log.info("{} Following redirect from {} > {}", LOG_PREFIX, remoteUrl, redirectUrl);
        remoteUrl = redirectUrl;
      } else if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
        TextureErrorHandler.urlLoadErrorMessage(
            textureModelKey, remoteUrl, connection.getResponseMessage());
        return null;
      }
    } catch (IllegalArgumentException | IOException exception) {
      TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, exception.getMessage());
      return null;
    }

    // Download URL directly to NativeImage
    NativeImage nativeImage;
    try (InputStream inputStream = new URL(remoteUrl).openStream()) {
      nativeImage = NativeImage.read(inputStream);
    } catch (IllegalArgumentException | IOException exception) {
      TextureErrorHandler.processingErrorMessage(
          textureModelKey, remoteUrl, exception.getMessage());
      return null;
    }

    // Verify the image data to make sure we got a valid image!
    if (!de.markusbordihn.easynpc.validator.ImageValidator.isValidImage(nativeImage)) {
      TextureErrorHandler.processingErrorMessage(
          textureModelKey, remoteUrl, "Unable to get any valid texture");
      nativeImage.close();
      return null;
    }

    // Apply legacy support if needed
    if ((textureModelKey.getSkinModel() == SkinModel.HUMANOID
            || textureModelKey.getSkinModel() == SkinModel.HUMANOID_SLIM)
        && nativeImage.getWidth() == 64
        && nativeImage.getHeight() == 32) {
      log.info(
          "{} Processing legacy image from 64x32 to 64x64 for {}", LOG_PREFIX, textureModelKey);
      nativeImage = TextureImageLoader.getNativeImageFromLegacyImage(nativeImage);
    }

    // Store to cache file for future use (async to not block texture registration)
    UUID uuid = textureModelKey.getUUID();
    File cacheFile = targetDirectory.resolve(TextureNameHelper.getFileName(uuid)).toFile();
    try {
      nativeImage.writeToFile(cacheFile.toPath());
      log.info("{} Cached downloaded texture as {} for {}", LOG_PREFIX, cacheFile, textureModelKey);
    } catch (IOException exception) {
      log.warn(
          "{} Unable to cache texture to file {}: {}",
          LOG_PREFIX,
          cacheFile,
          exception.getMessage());
    }

    // Register texture directly with NativeImage
    return TextureRegistrationHelper.registerTexture(textureModelKey, nativeImage);
  }
}
