/*
 * Copyright 2023 Markus Bordihn
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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.texture.TextureFailureInfo;
import de.markusbordihn.easynpc.data.texture.TextureFailureType;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.io.RemoteSkinDataFiles;
import java.io.File;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RemoteTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<TextureModelKey, ResourceLocation> textureCache =
      new ConcurrentHashMap<>();
  private static final Map<TextureModelKey, SkinType> textureSkinTypeCache =
      new ConcurrentHashMap<>();
  private static final Map<TextureModelKey, String> textureSkinURLCache = new ConcurrentHashMap<>();
  private static final Map<UUID, Long> textureReloadProtection = new ConcurrentHashMap<>();
  private static final Map<TextureModelKey, TextureFailureInfo> permanentFailures =
      new ConcurrentHashMap<>();
  private static final Map<TextureModelKey, Integer> retryAttempts = new ConcurrentHashMap<>();
  private static final String LOG_PREFIX = "[Remote Texture Manager] ";
  private static final long BASE_RETRY_DELAY = 60000;
  private static final int MAX_RETRY_ATTEMPTS = 3;
  private static final long CACHE_CLEANUP_INTERVAL = 600000;
  private static volatile long lastCleanup = System.currentTimeMillis();

  private RemoteTextureManager() {}

  public static void markPermanentFailure(
      TextureModelKey key, TextureFailureType type, String details, String url) {
    if (type.isPermanent()) {
      permanentFailures.put(key, new TextureFailureInfo(type, details, url));
      log.warn(
          "{} Marked texture {} as permanently failed: {} - {}", LOG_PREFIX, key, type, details);
    }
  }

  public static boolean hasPermanentFailure(TextureModelKey key) {
    return permanentFailures.containsKey(key);
  }

  public static void clearPermanentFailure(TextureModelKey key) {
    if (permanentFailures.remove(key) != null) {
      retryAttempts.remove(key);
      textureReloadProtection.remove(key.getUUID());
      log.info("{} Cleared permanent failure for {}", LOG_PREFIX, key);
    }
  }

  public static void clearAllPermanentFailures() {
    int count = permanentFailures.size();
    permanentFailures.clear();
    retryAttempts.clear();
    log.info("{} Cleared {} permanent failures", LOG_PREFIX, count);
  }

  private static long calculateRetryDelay(int attempts) {
    if (attempts >= MAX_RETRY_ATTEMPTS) {
      return Long.MAX_VALUE;
    }
    return BASE_RETRY_DELAY * (long) Math.pow(2, attempts);
  }

  private static void cleanupOldEntries() {
    long now = System.currentTimeMillis();
    if (now - lastCleanup < CACHE_CLEANUP_INTERVAL) {
      return;
    }

    textureReloadProtection.entrySet().removeIf(entry -> now - entry.getValue() > 300000);
    retryAttempts.entrySet().removeIf(entry -> textureCache.containsKey(entry.getKey()));

    lastCleanup = now;
  }

  public static Set<UUID> getTextureCacheKeys(SkinModel skinModel) {
    HashSet<UUID> hashSet = new HashSet<>();
    for (TextureModelKey textureModelKey : textureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel()) && hasTextureSkinData(textureModelKey)) {
        hashSet.add(textureModelKey.getUUID());
      }
    }
    return hashSet;
  }

  public static String getTextureSkinURL(TextureModelKey textureModelKey) {
    return textureSkinURLCache.get(textureModelKey);
  }

  public static SkinType getTextureSkinType(TextureModelKey textureModelKey) {
    return textureSkinTypeCache.get(textureModelKey);
  }

  public static boolean hasTextureSkinData(TextureModelKey textureModelKey) {
    return textureSkinTypeCache.containsKey(textureModelKey)
        && textureSkinURLCache.containsKey(textureModelKey);
  }

  public static ResourceLocation getOrCreateTextureWithDefault(
      SkinDataCapable<?> skinData, ResourceLocation defaultResourceLocation) {
    cleanupOldEntries();

    UUID skinUUID = skinData.getSkinUUID();
    if (skinUUID.equals(Constants.BLANK_UUID)) {
      return defaultResourceLocation;
    }

    // Check if there is already any cached resource location.
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID, skinData.getSkinModel());
    ResourceLocation resourceLocation = textureCache.get(textureModelKey);
    String skinURL = skinData.getSkinURL();
    if (resourceLocation != null) {
      if (!hasTextureSkinData(textureModelKey)) {
        textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
        textureSkinURLCache.put(textureModelKey, skinURL);
      }
      return resourceLocation;
    }

    ResourceLocation createdResourceLocation = createTexture(textureModelKey, skinData, skinURL);
    return createdResourceLocation != null ? createdResourceLocation : defaultResourceLocation;
  }

  private static ResourceLocation createTexture(
      TextureModelKey textureModelKey, SkinDataCapable<?> skinData, String skinURL) {

    if (hasPermanentFailure(textureModelKey)) {
      return null;
    }

    UUID skinUUID = textureModelKey.getUUID();
    long currentTime = System.currentTimeMillis();

    // Use atomic operations to prevent race conditions
    int attempts =
        retryAttempts.compute(
            textureModelKey,
            (key, current) -> {
              int currentAttempts = (current == null) ? 0 : current;
              if (currentAttempts >= MAX_RETRY_ATTEMPTS) {
                return currentAttempts;
              }
              return currentAttempts + 1;
            });

    if (attempts > MAX_RETRY_ATTEMPTS) {
      markPermanentFailure(
          textureModelKey,
          TextureFailureType.MAX_RETRIES_EXCEEDED,
          "Maximum retry attempts exceeded",
          skinURL);
      return null;
    }

    long requiredDelay = calculateRetryDelay(attempts - 1);
    Long lastAttempt = textureReloadProtection.get(skinUUID);

    if (lastAttempt != null && currentTime - lastAttempt < requiredDelay) {
      return null;
    }

    // Use putIfAbsent to avoid race condition
    Long existingAttempt = textureReloadProtection.putIfAbsent(skinUUID, currentTime);
    if (existingAttempt != null && currentTime - existingAttempt < requiredDelay) {
      return null;
    }

    // Get the skin model and texture data folder
    SkinModel skinModel = skinData.getSkinModel();
    Path textureDataFolder = RemoteSkinDataFiles.getRemoteSkinDataFolder(skinModel);
    if (textureDataFolder == null) {
      return null;
    }

    // Check the local texture cache for any matching texture.
    ResourceLocation localTextureCache =
        TextureManager.getCachedTexture(textureModelKey, textureDataFolder);
    if (localTextureCache != null) {
      textureCache.put(textureModelKey, localTextureCache);
      textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      textureSkinURLCache.put(textureModelKey, skinURL);
      return localTextureCache;
    }

    AsyncTextureLoader.loadTextureAsync(textureModelKey, skinURL, textureDataFolder)
        .thenAccept(
            resourceLocation -> {
              if (resourceLocation != null) {
                textureCache.put(textureModelKey, resourceLocation);
                textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
                textureSkinURLCache.put(textureModelKey, skinURL);
                retryAttempts.remove(textureModelKey);
              } else {
                log.error(
                    "{} Unable to load remote texture {} ({}) from {}!",
                    LOG_PREFIX,
                    textureModelKey,
                    skinURL,
                    textureDataFolder);
              }
            });

    return null;
  }

  public static void registerTexture(SkinModel skinModel, File textureFile) {
    registerTexture(TextureManager.getTextureModelKey(skinModel, textureFile), textureFile);
  }

  public static void registerTexture(TextureModelKey textureModelKey, File textureFile) {
    ResourceLocation resourceLocation =
        TextureManager.addCustomTexture(textureModelKey, textureFile);
    if (resourceLocation != null) {
      textureCache.put(textureModelKey, resourceLocation);
    }
  }

  public static void clearTextureCache() {
    textureReloadProtection.clear();
    textureCache.clear();
    textureSkinTypeCache.clear();
    textureSkinURLCache.clear();
    permanentFailures.clear();
    retryAttempts.clear();
  }
}
