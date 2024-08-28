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
import de.markusbordihn.easynpc.io.PlayerSkinDataFiles;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import java.io.File;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PlayerTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final HashMap<TextureModelKey, ResourceLocation> textureCache = new HashMap<>();
  private static final HashMap<TextureModelKey, SkinType> textureSkinTypeCache = new HashMap<>();
  private static final HashSet<UUID> textureReloadProtection = new HashSet<>();
  private static final String LOG_PREFIX = "[Player Texture Manager] ";

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
      // Return resource location and update reference, if needed.
      if (!hasTextureSkinData(textureModelKey)) {
        textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      }
      return resourceLocation;
    }

    UUID playerUUID = skinData.getSkinUUID();
    ResourceLocation createdResourceLocation = createTexture(textureModelKey, skinData, playerUUID);
    return createdResourceLocation != null ? createdResourceLocation : defaultResourceLocation;
  }

  private static ResourceLocation createTexture(
      TextureModelKey textureModelKey, SkinData<?> skinData, UUID playerUUID) {

    // Reload protection to avoid multiple texture requests in the same session.
    if (!textureReloadProtection.add(playerUUID)) {
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

    // Get skin texture location based on the player UUID.
    String playerSkinUrl = PlayersUtils.getUserTexture(playerUUID);

    // Validate the skin URL and perform some basic sanity checks and
    // process the remote texture.
    ResourceLocation resourceLocation =
        TextureManager.addRemoteTexture(textureModelKey, playerSkinUrl, textureDataFolder);
    if (resourceLocation != null) {
      textureCache.put(textureModelKey, resourceLocation);
      textureSkinTypeCache.put(textureModelKey, skinData.getSkinType());
      return resourceLocation;
    }

    // Log error if texture could not be loaded.
    log.error(
        "{} Unable to load player {} texture {} from {}!",
        LOG_PREFIX,
        playerUUID,
        textureModelKey,
        textureDataFolder);

    // Send error message to the user.
    Player player = Minecraft.getInstance().player;
    if (player != null) {
      player.sendSystemMessage(
          Component.literal(
                  LOG_PREFIX
                      + "Unable to load player "
                      + playerUUID
                      + " texture "
                      + textureModelKey
                      + ": "
                      + playerSkinUrl)
              .withStyle(ChatFormatting.RED));
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
