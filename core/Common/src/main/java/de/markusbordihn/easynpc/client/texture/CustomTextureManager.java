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
import de.markusbordihn.easynpc.config.CustomSkinConfig;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.io.CustomSkinDataFiles;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.network.message.server.RequestServerSkinMessage;
import java.io.File;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<TextureModelKey, ResourceLocation> textureCache =
      new ConcurrentHashMap<>();
  private static final Map<UUID, Long> textureReloadProtection = new ConcurrentHashMap<>();
  private static final Set<SkinModel> loadedSkinModels = ConcurrentHashMap.newKeySet();
  private static final Map<TextureModelKey, Long> serverSkinRequests = new ConcurrentHashMap<>();
  private static final Queue<TextureModelKey> queuedServerSkinRequests =
      new ConcurrentLinkedQueue<>();
  private static final Set<TextureModelKey> serverSkins = ConcurrentHashMap.newKeySet();
  private static final String LOG_PREFIX = "[Custom Texture Manager] ";
  private static final long RELOAD_PROTECTION_TIME = 60000;
  private static final long SERVER_SKIN_TIMEOUT = 10000;
  private static final int MAX_PARALLEL_SERVER_SKIN_REQUESTS = 2;
  private static final int MAX_QUEUED_SERVER_SKIN_REQUESTS = 64;
  private static final int MAX_SERVER_SKINS = 256;

  private CustomTextureManager() {}

  public static Set<UUID> getCustomTextureCacheKeys(SkinModel skinModel) {
    return getCustomTextureCacheKeys(skinModel, null);
  }

  public static Set<UUID> getCustomTextureCacheKeys(SkinModel skinModel, String searchName) {
    HashSet<UUID> hashSet = new HashSet<>();
    String skinSearchName =
        searchName != null && !searchName.isEmpty() ? searchName.toLowerCase(Locale.ROOT) : null;
    for (TextureModelKey textureModelKey : textureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel())
          && (skinSearchName == null
              || textureModelKey.getResourceName().isEmpty()
              || textureModelKey
                  .getResourceName()
                  .toLowerCase(Locale.ROOT)
                  .contains(skinSearchName))) {
        hashSet.add(textureModelKey.getUUID());
      }
    }
    return hashSet;
  }

  public static void ensureTextureCacheLoaded(SkinModel skinModel) {
    if (skinModel == null || !loadedSkinModels.add(skinModel)) {
      return;
    }

    CustomSkinDataFiles.registerTextureFiles(skinModel);
  }

  public static void markTextureCacheLoaded(SkinModel skinModel) {
    if (skinModel == null) {
      return;
    }

    loadedSkinModels.add(skinModel);
  }

  public static ResourceLocation getOrCreateTextureWithDefault(
      SkinDataCapable<?> skinData, ResourceLocation defaultResourceLocation) {
    UUID skinUUID = skinData.getSkinUUID();
    if (skinUUID.equals(Constants.BLANK_UUID)) {
      return defaultResourceLocation;
    }

    TextureModelKey textureModelKey = new TextureModelKey(skinUUID, skinData.getSkinModel());
    ResourceLocation resourceLocation = textureCache.get(textureModelKey);
    if (resourceLocation != null) {
      return resourceLocation;
    }

    ResourceLocation createdResourceLocation = createTexture(textureModelKey, skinData);
    return createdResourceLocation != null ? createdResourceLocation : defaultResourceLocation;
  }

  private static ResourceLocation createTexture(
      TextureModelKey textureModelKey, SkinDataCapable<?> skinData) {

    // Reload protection to avoid multiple texture requests in a short time.
    UUID skinUUID = textureModelKey.getUUID();
    Long lastAttempt = textureReloadProtection.get(skinUUID);
    if (lastAttempt != null && System.currentTimeMillis() - lastAttempt < RELOAD_PROTECTION_TIME) {
      return null;
    }

    textureReloadProtection.put(skinUUID, System.currentTimeMillis());

    SkinModel skinModel = skinData.getSkinModel();
    Path textureDataFolder = CustomSkinDataFiles.getCustomSkinDataFolder(skinModel);
    if (textureDataFolder == null) {
      return null;
    }

    ResourceLocation localTextureCache =
        TextureManager.searchCachedTexture(textureModelKey, textureDataFolder);
    if (localTextureCache != null) {
      textureCache.put(textureModelKey, localTextureCache);
      return localTextureCache;
    }

    if (!queueServerSkinRequest(textureModelKey)) {
      showMissingTextureMessage(textureModelKey);
    }
    return null;
  }

  private static boolean queueServerSkinRequest(TextureModelKey textureModelKey) {
    if (!CustomSkinConfig.DOWNLOAD_SERVER_SKINS) {
      return false;
    }

    if (serverSkins.size() >= MAX_SERVER_SKINS) {
      log.warn(
          "{} Not loading {} from the server, limit of {} server skins reached.",
          LOG_PREFIX,
          textureModelKey,
          MAX_SERVER_SKINS);
      return false;
    }

    if (!serverSkinRequests.containsKey(textureModelKey)
        && !queuedServerSkinRequests.contains(textureModelKey)
        && queuedServerSkinRequests.size() < MAX_QUEUED_SERVER_SKIN_REQUESTS) {
      queuedServerSkinRequests.add(textureModelKey);
    }
    return true;
  }

  public static void processServerSkinRequests() {
    if (serverSkinRequests.isEmpty() && queuedServerSkinRequests.isEmpty()) {
      return;
    }

    long now = System.currentTimeMillis();
    serverSkinRequests.values().removeIf(requestTime -> now - requestTime > SERVER_SKIN_TIMEOUT);
    while (serverSkinRequests.size() < MAX_PARALLEL_SERVER_SKIN_REQUESTS
        && !queuedServerSkinRequests.isEmpty()) {
      TextureModelKey textureModelKey = queuedServerSkinRequests.poll();
      serverSkinRequests.put(textureModelKey, now);
      NetworkHandlerManager.sendMessageToServer(
          new RequestServerSkinMessage(textureModelKey.getUUID(), textureModelKey.getSkinModel()));
    }
  }

  public static void handleServerSkinData(UUID skinUUID, SkinModel skinModel, byte[] pngBytes) {
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID, skinModel);
    if (serverSkinRequests.remove(textureModelKey) == null) {
      log.debug("{} Ignoring server skin {} without open request.", LOG_PREFIX, textureModelKey);
      return;
    }

    ResourceLocation resourceLocation =
        pngBytes.length > 0 ? TextureManager.addServerTexture(textureModelKey, pngBytes) : null;
    if (resourceLocation == null) {
      showMissingTextureMessage(textureModelKey);
      return;
    }

    textureCache.put(textureModelKey, resourceLocation);
    serverSkins.add(textureModelKey);
  }

  private static void showMissingTextureMessage(TextureModelKey textureModelKey) {
    Path textureDataFolder =
        CustomSkinDataFiles.getCustomSkinDataFolder(textureModelKey.getSkinModel());
    log.warn(
        "{} Unable to load custom texture {} from {}",
        LOG_PREFIX,
        textureModelKey,
        textureDataFolder);

    Player player = Minecraft.getInstance().player;
    if (player != null) {
      player.displayClientMessage(
          TextComponent.getText(
                  LOG_PREFIX
                      + "Unable to load custom texture "
                      + textureModelKey
                      + " from: "
                      + textureDataFolder)
              .withStyle(ChatFormatting.RED),
          false);
    }
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
    TextureRegistrationQueue.getInstance().clear(textureCache.keySet());
    textureReloadProtection.clear();
    loadedSkinModels.clear();
    textureCache.clear();
    serverSkinRequests.clear();
    queuedServerSkinRequests.clear();
    serverSkins.clear();
  }

  public static void clearTextureCache(SkinModel skinModel) {
    if (skinModel == null) {
      clearTextureCache();
      return;
    }

    Set<TextureModelKey> textureModelKeys = new HashSet<>();
    for (TextureModelKey textureModelKey : textureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel())) {
        textureModelKeys.add(textureModelKey);
      }
    }

    TextureRegistrationQueue.getInstance().clear(textureModelKeys);
    for (TextureModelKey textureModelKey : textureModelKeys) {
      textureCache.remove(textureModelKey);
      textureReloadProtection.remove(textureModelKey.getUUID());
      serverSkins.remove(textureModelKey);
    }
    loadedSkinModels.remove(skinModel);
  }
}
