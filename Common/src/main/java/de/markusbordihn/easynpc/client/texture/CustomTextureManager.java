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
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.io.CustomSkinDataFiles;
import java.io.File;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.ChatFormatting;
import net.minecraft.Util;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final int RELOAD_PROTECTION = 10000;
  private static final HashMap<TextureModelKey, ResourceLocation> textureCache = new HashMap<>();
  private static final HashSet<UUID> textureReloadProtection = new HashSet<>();
  private static final String LOG_PREFIX = "[Custom Texture Manager] ";
  private static int reloadProtectionCounter = 0;

  private CustomTextureManager() {}

  public static Set<UUID> getCustomTextureCacheKeys(SkinModel skinModel) {
    return getCustomTextureCacheKeys(skinModel, null);
  }

  public static Set<UUID> getCustomTextureCacheKeys(SkinModel skinModel, String searchName) {
    HashSet<UUID> hashSet = new HashSet<>();
    String skinSearchName =
        searchName != null && !searchName.isEmpty() ? searchName.toLowerCase() : null;
    for (TextureModelKey textureModelKey : textureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel())
          && (skinSearchName == null
              || textureModelKey.getResourceName().isEmpty()
              || textureModelKey.getResourceName().toLowerCase().contains(skinSearchName))) {
        hashSet.add(textureModelKey.getUUID());
      }
    }
    return hashSet;
  }

  public static ResourceLocation getOrCreateTextureWithDefault(
      SkinData<?> skinData, ResourceLocation defaultResourceLocation) {
    // Check if we have a skin UUID otherwise we assume that the texture is unknown.
    UUID skinUUID = skinData.getSkinUUID();
    if (skinUUID.equals(Constants.BLANK_UUID)) {
      return defaultResourceLocation;
    }

    // Check if there is already any cached resource location.
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID, skinData.getSkinModel());
    ResourceLocation resourceLocation = textureCache.get(textureModelKey);
    if (resourceLocation != null) {
      return resourceLocation;
    }

    ResourceLocation createdResourceLocation = createTexture(textureModelKey, skinData);
    return createdResourceLocation != null ? createdResourceLocation : defaultResourceLocation;
  }

  private static ResourceLocation createTexture(
      TextureModelKey textureModelKey, SkinData<?> skinData) {

    // Reload protection to avoid multiple texture requests in a short time.
    UUID skinUUID = textureModelKey.getUUID();
    if (!textureReloadProtection.add(skinUUID)) {
      if (reloadProtectionCounter++ > RELOAD_PROTECTION) {
        textureReloadProtection.clear();
        reloadProtectionCounter = 0;
      }
      return null;
    }

    // Get the skin model and texture data folder
    SkinModel skinModel = skinData.getSkinModel();
    Path textureDataFolder = CustomSkinDataFiles.getCustomSkinDataFolder(skinModel);
    if (textureDataFolder == null) {
      return null;
    }

    // Search the local texture cache directory for any matching texture.
    ResourceLocation localTextureCache =
        TextureManager.searchCachedTexture(textureModelKey, textureDataFolder);
    if (localTextureCache != null) {
      textureCache.put(textureModelKey, localTextureCache);
      return localTextureCache;
    }

    // Log error if texture could not be loaded.
    log.error(
        "{} Unable to load custom texture {} {} from {}",
        LOG_PREFIX,
        skinModel,
        skinUUID,
        textureDataFolder);

    // Send error message to the user.
    Player player = Minecraft.getInstance().player;
    if (player != null) {
      player.sendMessage(
          new TextComponent(
                  LOG_PREFIX
                      + "Unable to load custom texture "
                      + textureModelKey
                      + " from: "
                      + textureDataFolder)
              .withStyle(ChatFormatting.RED),
          Util.NIL_UUID);
    }

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
  }
}
