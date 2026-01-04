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
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlayerTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<TextureModelKey, ResourceLocation> textureCache =
      new ConcurrentHashMap<>();
  private static final Map<TextureModelKey, SkinType> textureSkinTypeCache =
      new ConcurrentHashMap<>();
  private static final Map<UUID, Long> textureReloadProtection = new ConcurrentHashMap<>();
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

  public static ResourceLocation getOrCreateTextureWithDefault(
      SkinDataCapable<?> skinData, ResourceLocation defaultResourceLocation) {
    // Check if we have a skin UUID otherwise we assume that the texture is unknown.
    UUID skinUUID = skinData.getSkinUUID();
    if (skinUUID.equals(Constants.BLANK_UUID)) {
      return defaultResourceLocation;
    }

    // Check if there is already any cached resource location.
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID, skinData.getSkinModel());
    ResourceLocation resourceLocation = textureCache.get(textureModelKey);
    if (resourceLocation != null) {
      if (!hasTextureSkinData(textureModelKey)) {
        textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      }
      return resourceLocation;
    }

    ResourceLocation createdResourceLocation = createTexture(textureModelKey, skinData, skinUUID);
    return createdResourceLocation != null ? createdResourceLocation : defaultResourceLocation;
  }

  private static ResourceLocation createTexture(
      TextureModelKey textureModelKey, SkinDataCapable<?> skinData, UUID playerUUID) {

    // Reload protection to avoid multiple texture requests in a short time using atomic operation
    long currentTime = System.currentTimeMillis();
    Long lastAttempt = textureReloadProtection.get(playerUUID);
    if (lastAttempt != null && currentTime - lastAttempt < RELOAD_PROTECTION_TIME) {
      return null;
    }

    // Only proceed if we successfully claim this request
    Long existingAttempt = textureReloadProtection.putIfAbsent(playerUUID, currentTime);
    if (existingAttempt != null && currentTime - existingAttempt < RELOAD_PROTECTION_TIME) {
      return null;
    }

    // Get the skin model and texture data folder
    SkinModel skinModel = skinData.getSkinModel();
    Path textureDataFolder = PlayerSkinDataFiles.getPlayerSkinDataFolder(skinModel);
    if (textureDataFolder == null) {
      return null;
    }

    // Check the local texture cache for any matching texture.
    ResourceLocation localTextureCache =
        TextureManager.getCachedTexture(textureModelKey, textureDataFolder);
    if (localTextureCache != null) {
      textureCache.put(textureModelKey, localTextureCache);
      textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      return localTextureCache;
    }

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
  }
}
