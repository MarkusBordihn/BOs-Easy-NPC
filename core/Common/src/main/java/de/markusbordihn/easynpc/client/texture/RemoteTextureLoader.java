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
import de.markusbordihn.easynpc.config.RemoteTextureConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.texture.TextureFailureType;
import de.markusbordihn.easynpc.validator.ImageValidator;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RemoteTextureLoader {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Remote Texture Loader]";
  private static final int CONNECTION_TIMEOUT = 10000;
  private static final int READ_TIMEOUT = 30000;
  private static final long MAX_DOWNLOAD_SIZE = 5 * 1024 * 1024;
  private static final int MAX_REDIRECTS = 5;
  private static final String USER_AGENT = Constants.MOD_NAME + " Minecraft remote texture loader";

  private RemoteTextureLoader() {}

  public static CompletableFuture<ResourceLocation> loadRemoteTextureAsync(
      TextureModelKey textureModelKey, String remoteUrl, Path targetDirectory) {
    if (!UrlValidator.isValidUrl(remoteUrl)) {
      String error = "Invalid URL format or forbidden extension";
      TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.URL_INVALID, error, remoteUrl);
      return CompletableFuture.completedFuture(null);
    }

    // Check for cached texture.
    NativeImage cachedNativeImage =
        TextureCacheManager.getCachedNativeImage(textureModelKey, targetDirectory);
    if (cachedNativeImage != null) {
      log.info(
          "{} Found downloaded file in cache, will re-used {} for {}",
          LOG_PREFIX,
          textureModelKey,
          remoteUrl);
      return TextureRegistrationHelper.registerTextureAsync(textureModelKey, cachedNativeImage);
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
      int responseCode = 0;
      int redirectCount = 0;

      // Follow redirects manually so every hop is re-validated against the URL and address policy.
      while (true) {
        if (isBlockedAddress(remoteImageURL)) {
          String error = "Blocked private/link-local address: " + remoteImageURL.getHost();
          TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
          RemoteTextureManager.markPermanentFailure(
              textureModelKey, TextureFailureType.URL_INVALID, error, remoteUrl);
          return CompletableFuture.completedFuture(null);
        }

        connection = openConnection(remoteImageURL);
        responseCode = connection.getResponseCode();

        if (!isRedirect(responseCode)) {
          break;
        }

        if (++redirectCount > MAX_REDIRECTS) {
          String error = "Too many redirects (max " + MAX_REDIRECTS + ")";
          TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
          RemoteTextureManager.markPermanentFailure(
              textureModelKey, TextureFailureType.NETWORK_ERROR, error, remoteUrl);
          return CompletableFuture.completedFuture(null);
        }

        String redirectLocation = connection.getHeaderField("Location");
        connection.disconnect();
        if (redirectLocation == null || redirectLocation.isEmpty()) {
          break;
        }

        // Resolve relative locations against the current URL and re-validate.
        URL redirectUrl = new URL(remoteImageURL, redirectLocation);
        if (!UrlValidator.isValidUrl(redirectUrl.toString())) {
          String error = "Invalid redirect target: " + redirectUrl;
          TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
          RemoteTextureManager.markPermanentFailure(
              textureModelKey, TextureFailureType.URL_INVALID, error, redirectUrl.toString());
          return CompletableFuture.completedFuture(null);
        }
        log.info("{} Following redirect from {} > {}", LOG_PREFIX, remoteUrl, redirectUrl);
        remoteImageURL = redirectUrl;
        remoteUrl = redirectUrl.toString();
      }

      if (responseCode != HttpURLConnection.HTTP_OK) {
        String error = "HTTP " + responseCode + ": " + connection.getResponseMessage();
        TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
        RemoteTextureManager.markPermanentFailure(
            textureModelKey, getFailureType(responseCode), error, remoteUrl);
        return CompletableFuture.completedFuture(null);
      }

      // Fast-path check on the advertised size (may be absent or -1 for chunked responses).
      long contentLength = connection.getContentLengthLong();
      if (contentLength > MAX_DOWNLOAD_SIZE) {
        String error =
            String.format(
                "File too large: %d bytes (max %d bytes)", contentLength, MAX_DOWNLOAD_SIZE);
        TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
        RemoteTextureManager.markPermanentFailure(
            textureModelKey, TextureFailureType.FILE_TOO_LARGE, error, remoteUrl);
        return CompletableFuture.completedFuture(null);
      }

      try (InputStream inputStream = connection.getInputStream()) {
        byte[] imageBytes = readLimited(inputStream, MAX_DOWNLOAD_SIZE);
        nativeImage = NativeImage.read(new ByteArrayInputStream(imageBytes));
      }

    } catch (IllegalArgumentException | IOException exception) {
      String error = exception.getClass().getSimpleName() + ": " + exception.getMessage();
      TextureErrorHandler.urlLoadErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.NETWORK_ERROR, error, remoteUrl);
      return CompletableFuture.completedFuture(null);
    } finally {
      if (connection != null) {
        connection.disconnect();
      }
    }

    if (nativeImage == null) {
      String error = "Failed to decode image";
      TextureErrorHandler.processingErrorMessage(textureModelKey, remoteUrl, error);
      RemoteTextureManager.markPermanentFailure(
          textureModelKey, TextureFailureType.DECODING_ERROR, error, remoteUrl);
      return CompletableFuture.completedFuture(null);
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
      return CompletableFuture.completedFuture(null);
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

    return TextureRegistrationHelper.registerTextureAsync(textureModelKey, nativeImage);
  }

  private static HttpURLConnection openConnection(URL remoteImageURL) throws IOException {
    HttpURLConnection connection = (HttpURLConnection) remoteImageURL.openConnection();
    connection.setInstanceFollowRedirects(false);
    connection.setConnectTimeout(CONNECTION_TIMEOUT);
    connection.setReadTimeout(READ_TIMEOUT);
    connection.setRequestProperty("User-Agent", USER_AGENT);
    connection.setRequestProperty("Accept", "image/png,image/*,*/*");
    return connection;
  }

  private static boolean isRedirect(int responseCode) {
    return responseCode == HttpURLConnection.HTTP_MOVED_PERM
        || responseCode == HttpURLConnection.HTTP_MOVED_TEMP
        || responseCode == HttpURLConnection.HTTP_SEE_OTHER
        || responseCode == 307
        || responseCode == 308;
  }

  private static boolean isBlockedAddress(URL url) {
    if (!RemoteTextureConfig.BLOCK_PRIVATE_ADDRESSES) {
      return false;
    }
    try {
      for (InetAddress address : InetAddress.getAllByName(url.getHost())) {
        if (address.isLoopbackAddress()
            || address.isAnyLocalAddress()
            || address.isSiteLocalAddress()
            || address.isLinkLocalAddress()) {
          return true;
        }
      }
      return false;
    } catch (UnknownHostException exception) {
      return true;
    }
  }

  private static byte[] readLimited(InputStream inputStream, long maxBytes) throws IOException {
    ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    byte[] chunk = new byte[8192];
    long total = 0;
    int read;
    while ((read = inputStream.read(chunk)) != -1) {
      total += read;
      if (total > maxBytes) {
        throw new IOException(
            "Remote image exceeds maximum allowed size of " + maxBytes + " bytes");
      }
      buffer.write(chunk, 0, read);
    }
    return buffer.toByteArray();
  }

  private static TextureFailureType getFailureType(int responseCode) {
    if (responseCode == HttpURLConnection.HTTP_FORBIDDEN
        || responseCode == HttpURLConnection.HTTP_NOT_FOUND) {
      return TextureFailureType.HTTP_CLIENT_ERROR;
    }
    return TextureFailureType.NETWORK_ERROR;
  }
}
