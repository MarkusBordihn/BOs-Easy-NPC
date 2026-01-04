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
import de.markusbordihn.easynpc.data.texture.TextureFailureType;
import de.markusbordihn.easynpc.validator.ImageValidator;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RemoteTextureLoader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Remote Texture Loader]";
  private static final int CONNECTION_TIMEOUT = 10000;
  private static final int READ_TIMEOUT = 30000;
  private static final long MAX_DOWNLOAD_SIZE = 5 * 1024 * 1024;

  private RemoteTextureLoader() {}

  public static ResourceLocation loadRemoteTexture(
      TextureModelKey textureModelKey, String remoteUrl, Path targetDirectory) {
    if (!UrlValidator.isValidUrl(remoteUrl)) {
      String error = "Invalid URL format or forbidden extension";
      TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.URL_INVALID, error, remoteUrl);
      return null;
    }

    // Check for cached texture.
    ResourceLocation cachedTexture =
        TextureCacheManager.getCachedTexture(textureModelKey, targetDirectory);
    if (cachedTexture != null) {
      log.info(
          "{} Found downloaded file in cache, will re-used {} for {}",
          LOG_PREFIX,
          cachedTexture,
          remoteUrl);
      return cachedTexture;
    }

    // Start downloading the remote texture.
    log.warn(
        "{} Starting download of remote texture from {} for {}",
        LOG_PREFIX,
        remoteUrl,
        textureModelKey);

    HttpURLConnection connection = null;
    NativeImage nativeImage = null;

    try {
      URL remoteImageURL = new URL(remoteUrl);
      connection = (HttpURLConnection) remoteImageURL.openConnection();
      connection.setConnectTimeout(CONNECTION_TIMEOUT);
      connection.setReadTimeout(READ_TIMEOUT);

      // Check content length
      long contentLength = connection.getContentLengthLong();
      if (contentLength > MAX_DOWNLOAD_SIZE) {
        String error =
            String.format(
                "File too large: %d bytes (max %d bytes)", contentLength, MAX_DOWNLOAD_SIZE);
        TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
        RemoteTextureManager.markPermanentFailure(
            textureModelKey, TextureFailureType.FILE_TOO_LARGE, error, remoteUrl);
        return null;
      }

      // Handle redirects
      int responseCode = connection.getResponseCode();
      if (responseCode == HttpURLConnection.HTTP_MOVED_PERM
          || responseCode == HttpURLConnection.HTTP_MOVED_TEMP) {
        String redirectUrl = connection.getHeaderField("Location");
        log.info("{} Following redirect from {} > {}", LOG_PREFIX, remoteUrl, redirectUrl);
        connection.disconnect();

        // Follow redirect
        remoteImageURL = new URL(redirectUrl);
        connection = (HttpURLConnection) remoteImageURL.openConnection();
        connection.setConnectTimeout(CONNECTION_TIMEOUT);
        connection.setReadTimeout(READ_TIMEOUT);
        responseCode = connection.getResponseCode();
        remoteUrl = redirectUrl;
      }

      if (responseCode != HttpURLConnection.HTTP_OK) {
        String error = "HTTP " + responseCode + ": " + connection.getResponseMessage();
        TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
        RemoteTextureManager.markPermanentFailure(
            textureModelKey, TextureFailureType.NETWORK_ERROR, error, remoteUrl);
        return null;
      }

      // Read and decode the image directly from the connection
      try (InputStream inputStream = connection.getInputStream()) {
        nativeImage = NativeImage.read(inputStream);
      }

    } catch (IllegalArgumentException | IOException exception) {
      String error = exception.getClass().getSimpleName() + ": " + exception.getMessage();
      TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.NETWORK_ERROR, error, remoteUrl);
      return null;
    } finally {
      if (connection != null) {
        connection.disconnect();
      }
    }

    // Validate image
    if (nativeImage == null) {
      String error = "Failed to decode image";
      TextureErrorHandler.processingErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.DECODING_ERROR, error, remoteUrl);
      return null;
    }

    if (!ImageValidator.isValidImage(nativeImage)) {
      String error =
          String.format(
              "Invalid image dimensions: %dx%d (expected 64x64, 64x32, or multiples of 32 >= 32x32)",
              nativeImage.getWidth(), nativeImage.getHeight());
      TextureErrorHandler.processingErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.INVALID_IMAGE_SIZE, error, remoteUrl);
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

    // Register texture on the main thread
    NativeImage finalImage = nativeImage;
    CompletableFuture<ResourceLocation> registrationFuture = new CompletableFuture<>();
    Minecraft.getInstance()
        .execute(
            () -> {
              try {
                ResourceLocation resourceLocation =
                    TextureRegistrationHelper.registerTexture(textureModelKey, finalImage);
                registrationFuture.complete(resourceLocation);
              } catch (Exception e) {
                log.error(
                    "{} Failed to register texture on main thread: {}", LOG_PREFIX, e.getMessage());
                finalImage.close();
                registrationFuture.completeExceptionally(e);
              }
            });

    // Wait for registration to complete with timeout
    try {
      return registrationFuture.get(5, java.util.concurrent.TimeUnit.SECONDS);
    } catch (Exception e) {
      log.error(
          "{} Timeout or error waiting for texture registration: {}", LOG_PREFIX, e.getMessage());
      nativeImage.close();
      return null;
    }
  }
}
