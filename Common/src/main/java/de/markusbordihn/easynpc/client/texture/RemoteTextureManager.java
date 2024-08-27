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
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.io.RemoteSkinDataFiles;
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

public class RemoteTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final HashMap<TextureModelKey, ResourceLocation> textureCache = new HashMap<>();
  private static final HashMap<TextureModelKey, SkinType> textureSkinTypeCache = new HashMap<>();
  private static final HashMap<TextureModelKey, String> textureSkinURLCache = new HashMap<>();
  private static final HashSet<UUID> textureReloadProtection = new HashSet<>();
  private static final String LOG_PREFIX = "[Remote Texture Manager] ";

  private RemoteTextureManager() {}

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
      SkinData<?> skinData, ResourceLocation defaultResourceLocation) {
    // Check if we have a skin UUID otherwise we assume that the texture is unknown.
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
      TextureModelKey textureModelKey, SkinData<?> skinData, String skinURL) {

    // Reload protection to avoid multiple texture requests in the same session.
    UUID skinUUID = textureModelKey.getUUID();
    if (!textureReloadProtection.add(skinUUID)) {
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

    // Validate the skin URL and perform some basic sanity checks and
    // process the remote texture.
    ResourceLocation resourceLocation =
        TextureManager.addRemoteTexture(textureModelKey, skinURL, textureDataFolder);
    if (resourceLocation != null) {
      textureCache.put(textureModelKey, resourceLocation);
      textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      textureSkinURLCache.put(textureModelKey, skinURL);
      return resourceLocation;
    }

    // Log error if texture could not be loaded.
    log.error(
        "{} Unable to load remote texture {} ({}) from {}!",
        LOG_PREFIX,
        textureModelKey,
        skinURL,
        textureDataFolder);

    // Send error message to the user.
    Player player = Minecraft.getInstance().player;
    if (player != null) {
      player.sendMessage(
          new TextComponent(
                  LOG_PREFIX + "Unable to load remote " + skinURL + " texture " + textureModelKey)
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
}
