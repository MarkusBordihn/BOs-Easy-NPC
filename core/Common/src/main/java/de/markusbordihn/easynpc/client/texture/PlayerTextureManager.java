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
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.io.PlayerSkinDataFiles;
import java.io.File;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlayerTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<TextureModelKey, Identifier> textureCache = new ConcurrentHashMap<>();
  private static final Map<TextureModelKey, SkinType> textureSkinTypeCache =
      new ConcurrentHashMap<>();
  private static final Map<UUID, Long> textureReloadProtection = new ConcurrentHashMap<>();
  private static final Set<UUID> revalidatedTextures = ConcurrentHashMap.newKeySet();
  private static final String LOG_PREFIX = "[Player Texture Manager] ";
  private static final long RELOAD_PROTECTION_TIME = 60000;

  private PlayerTextureManager() {}

  public static Set<UUID> getTextureCacheKeys(SkinModel skinModel) {
    HashSet<UUID> hashSet = new HashSet<>();
    for (TextureModelKey textureModelKey : textureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel()) && hasTextureSkinData(textureModelKey)) {
        hashSet.add(textureModelKey.getUUID());
      }
    }
    return hashSet;
  }

  public static SkinType getTextureSkinType(TextureModelKey textureModelKey) {
    return textureSkinTypeCache.get(textureModelKey);
  }

  public static boolean hasTextureSkinData(TextureModelKey textureModelKey) {
    return textureSkinTypeCache.containsKey(textureModelKey);
  }

  public static Identifier getOrCreateTextureWithDefault(
      SkinDataCapable<?> skinData, Identifier defaultIdentifier) {
    // Check if we have a skin UUID otherwise we assume that the texture is unknown.
    UUID skinUUID = skinData.getSkinUUID();
    if (skinUUID.equals(Constants.BLANK_UUID)) {
      return defaultIdentifier;
    }

    // Check if there is already any cached resource location.
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID, skinData.getSkinModel());
    Identifier resourceLocation = textureCache.get(textureModelKey);
    if (resourceLocation != null) {
      if (!hasTextureSkinData(textureModelKey)) {
        textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      }
      revalidateTexture(textureModelKey, skinData);
      return resourceLocation;
    }

    Identifier createdIdentifier = createTexture(textureModelKey, skinData, skinUUID);
    return createdIdentifier != null ? createdIdentifier : defaultIdentifier;
  }

  private static Identifier createTexture(
      TextureModelKey textureModelKey, SkinDataCapable<?> skinData, UUID playerUUID) {

    // Get the skin model and texture data folder
    SkinModel skinModel = skinData.getSkinModel();
    Path textureDataFolder = PlayerSkinDataFiles.getPlayerSkinDataFolder(skinModel);
    if (textureDataFolder == null) {
      return null;
    }

    if (AsyncTextureLoader.hasPendingLoad(textureModelKey)) {
      return null;
    }

    if (TextureManager.hasCachedTexture(textureModelKey, textureDataFolder)) {
      AsyncTextureLoader.loadCachedTextureAsync(textureModelKey, textureDataFolder)
          .thenAccept(
              resourceLocation -> {
                if (resourceLocation != null) {
                  textureCache.put(textureModelKey, resourceLocation);
                  textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
                }
              });
      return null;
    }

    long currentTime = System.currentTimeMillis();
    Long lastAttempt = textureReloadProtection.get(playerUUID);
    if (lastAttempt != null && currentTime - lastAttempt < RELOAD_PROTECTION_TIME) {
      return null;
    }
    textureReloadProtection.put(playerUUID, currentTime);

    AsyncTextureLoader.loadPlayerTextureAsync(textureModelKey, playerUUID, textureDataFolder)
        .thenAccept(
            resourceLocation -> {
              if (resourceLocation != null) {
                log.info(
                    "{} Successfully loaded player texture for {}: {}",
                    LOG_PREFIX,
                    playerUUID,
                    resourceLocation);
                textureCache.put(textureModelKey, resourceLocation);
                textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
              } else {
                log.error(
                    "{} Unable to load player {} texture {} from {}!",
                    LOG_PREFIX,
                    playerUUID,
                    textureModelKey,
                    textureDataFolder);
              }
            });

    return null;
  }

  private static void revalidateTexture(
      TextureModelKey textureModelKey, SkinDataCapable<?> skinData) {
    UUID playerUUID = textureModelKey.getUUID();
    if (!revalidatedTextures.add(playerUUID)) {
      return;
    }

    Path textureDataFolder = PlayerSkinDataFiles.getPlayerSkinDataFolder(skinData.getSkinModel());
    if (textureDataFolder == null) {
      return;
    }

    String cachedTextureSource =
        TextureCacheManager.getCachedTextureSource(textureModelKey, textureDataFolder);
    if (cachedTextureSource == null) {
      return;
    }

    AsyncTextureLoader.revalidatePlayerTextureAsync(
        textureModelKey,
        playerUUID,
        textureDataFolder,
        cachedTextureSource,
        skinData.getSkinType());
  }

  static void updateTexture(
      TextureModelKey textureModelKey, Identifier resourceLocation, SkinType skinType) {
    textureCache.put(textureModelKey, resourceLocation);
    textureSkinTypeCache.put(textureModelKey, skinType);
  }

  public static boolean refreshTexture(TextureModelKey textureModelKey) {
    Path textureDataFolder =
        PlayerSkinDataFiles.getPlayerSkinDataFolder(textureModelKey.getSkinModel());
    if (textureDataFolder == null) {
      return false;
    }

    TextureCacheManager.removeCachedTexture(textureModelKey, textureDataFolder);
    TextureRegistrationQueue.getInstance().clear(Set.of(textureModelKey));
    textureCache.remove(textureModelKey);
    textureSkinTypeCache.remove(textureModelKey);
    textureReloadProtection.remove(textureModelKey.getUUID());
    revalidatedTextures.remove(textureModelKey.getUUID());
    log.info("{} Dropped cached player texture for {}", LOG_PREFIX, textureModelKey);
    return true;
  }

  public static void registerTexture(SkinModel skinModel, File textureFile) {
    registerTexture(TextureManager.getTextureModelKey(skinModel, textureFile), textureFile);
  }

  public static void registerTexture(TextureModelKey textureModelKey, File textureFile) {
    Identifier resourceLocation = TextureManager.addCustomTexture(textureModelKey, textureFile);
    if (resourceLocation != null) {
      textureCache.put(textureModelKey, resourceLocation);
    }
  }

  public static void clearTextureCache() {
    TextureRegistrationQueue.getInstance().clear(textureCache.keySet());
    textureReloadProtection.clear();
    revalidatedTextures.clear();
    textureCache.clear();
    textureSkinTypeCache.clear();
  }
}
