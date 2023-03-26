/**
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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.resources.ResourceLocation;

import net.minecraftforge.fml.loading.FileUtils;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.utils.PlayersUtils;

public class PlayerTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LOG_PREFIX = "[Player Texture Manager]";

  private static Path textureCachePath = null;
  private static HashMap<TextureModelKey, ResourceLocation> playerTextureCache = new HashMap<>();
  private static HashMap<TextureModelKey, SkinType> playerTextureSkinTypeCache = new HashMap<>();
  private static HashMap<TextureModelKey, String> playerTextureSkinURLCache = new HashMap<>();
  private static HashSet<UUID> playerTextureReloadProtection = new HashSet<>();

  protected PlayerTextureManager() {}

  public static Map<TextureModelKey, ResourceLocation> getPlayerTextureCache() {
    return playerTextureCache;
  }

  public static Set<UUID> getPlayerTextureCacheKeys(SkinModel skinModel) {
    HashSet<UUID> hashSet = new HashSet<>();
    for (TextureModelKey textureModelKey : playerTextureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel())
          && hasPlayerTextureSkinData(textureModelKey)) {
        hashSet.add(textureModelKey.getUUID());
      }
    }
    return hashSet;
  }

  public static SkinType getPlayerTextureSkinType(TextureModelKey textureModelKey) {
    return playerTextureSkinTypeCache.get(textureModelKey);
  }

  public static String getPlayerTextureSkinURL(TextureModelKey textureModelKey) {
    return playerTextureSkinURLCache.get(textureModelKey);
  }

  public static boolean hasPlayerTextureSkinData(TextureModelKey textureModelKey) {
    return playerTextureSkinTypeCache.containsKey(textureModelKey)
        && playerTextureSkinURLCache.containsKey(textureModelKey);
  }

  public static ResourceLocation getOrCreateTextureWithDefault(EasyNPCEntity entity,
      ResourceLocation defaultResourceLocation) {
    // Check if we have a skin UUID otherwise we assume that the texture is unknown.
    Optional<UUID> skinUUID = entity.getSkinUUID();
    if (!skinUUID.isPresent()) {
      return defaultResourceLocation;
    }

    // Check if there is already any cached resource location.
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID.get(), entity.getSkinModel());
    ResourceLocation resourceLocation = playerTextureCache.get(textureModelKey);
    if (resourceLocation != null) {
      // Return resource location and update reference, if needed.
      if (!hasPlayerTextureSkinData(textureModelKey)) {
        playerTextureSkinTypeCache.put(textureModelKey, entity.getSkinType());
        playerTextureSkinURLCache.put(textureModelKey, entity.getSkinURL());
      }
      return resourceLocation;
    }

    ResourceLocation createdResourceLocation = createTexture(textureModelKey, entity);
    return createdResourceLocation != null ? createdResourceLocation : defaultResourceLocation;
  }

  private static ResourceLocation createTexture(TextureModelKey textureModelKey,
      EasyNPCEntity entity) {
    return createTexture(textureModelKey, entity.getSkinType(), entity.getSkinURL());
  }

  private static ResourceLocation createTexture(TextureModelKey textureModelKey, SkinType skinType,
      String skinURL) {

    // Check the local texture cache for any matching files.
    SkinModel skinModel = textureModelKey.getSkinModel();
    String targetDirectory = getTextureCacheDirectory(skinModel).toString();
    ResourceLocation localTextureCache =
        TextureManager.getCachedTexture(textureModelKey, targetDirectory);
    if (localTextureCache != null) {
      playerTextureCache.put(textureModelKey, localTextureCache);
      playerTextureSkinTypeCache.put(textureModelKey, skinType);
      playerTextureSkinURLCache.put(textureModelKey, skinURL);
      return localTextureCache;
    }

    // Reload protection to avoid multiple http requests in the same session.
    UUID skinUUID = textureModelKey.getUUID();
    if (playerTextureReloadProtection.contains(skinUUID)) {
      return null;
    }
    playerTextureReloadProtection.add(skinUUID);

    // Get skin texture location depending on skin type.
    String textureSkinURL = null;
    switch (skinType) {
      case PLAYER_SKIN:
        textureSkinURL = PlayersUtils.getUserTexture(skinUUID);
        break;
      case SECURE_REMOTE_URL:
      case INSECURE_REMOTE_URL:
        if (PlayersUtils.isValidUrl(skinURL)) {
          textureSkinURL = skinURL;
        }
        break;
      default:
    }

    // Check if we got any valid texture skin location.
    if (textureSkinURL == null || textureSkinURL.isEmpty()) {
      return null;
    }

    // Add Texture to texture manager.
    ResourceLocation resourceLocation =
        TextureManager.addRemoteTexture(textureModelKey, textureSkinURL, targetDirectory);
    playerTextureCache.put(textureModelKey, resourceLocation);
    playerTextureSkinTypeCache.put(textureModelKey, skinType);
    playerTextureSkinURLCache.put(textureModelKey, textureSkinURL);
    return resourceLocation;
  }

  private static Path getTextureCacheDirectory(SkinModel skinModel) {
    // Get or create main cache directory.
    if (textureCachePath == null) {
      Path cacheDirectory =
          Paths.get(TextureManager.getTextureCacheDirectory().toString(), "player");
      if (!cacheDirectory.toFile().exists()) {
        log.info("{} Creating player texture cache directory at {}", LOG_PREFIX, cacheDirectory);
        FileUtils.getOrCreateDirectory(cacheDirectory, Constants.MOD_ID);
      }
      textureCachePath = cacheDirectory;
    }

    // Get or create model cache directory.
    Path cacheDirectory = Paths.get(textureCachePath.toString(), skinModel.name());
    if (!cacheDirectory.toFile().exists()) {
      log.info("{} Creating player texture model cache directory at {}", LOG_PREFIX,
          cacheDirectory);
      FileUtils.getOrCreateDirectory(cacheDirectory, Constants.MOD_ID);
    }
    return cacheDirectory;
  }

}
