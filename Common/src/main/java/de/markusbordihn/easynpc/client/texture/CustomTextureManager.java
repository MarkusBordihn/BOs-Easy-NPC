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
import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LOG_PREFIX = "[Custom Texture Manager]";

  private static final HashMap<TextureModelKey, ResourceLocation> customTextureCache =
      new HashMap<>();

  protected CustomTextureManager() {}

  public static Set<UUID> getCustomTextureCacheKeys(SkinModel skinModel) {
    HashSet<UUID> hashSet = new HashSet<>();
    for (TextureModelKey textureModelKey : customTextureCache.keySet()) {
      if (skinModel.equals(textureModelKey.getSkinModel())) {
        hashSet.add(textureModelKey.getUUID());
      }
    }
    return hashSet;
  }

  public static ResourceLocation getOrCreateTextureWithDefault(
      SkinData<?> entity, ResourceLocation defaultResourceLocation) {
    // Check if we have a skin UUID otherwise we assume that the texture is unknown.
    Optional<UUID> skinUUID = entity.getSkinUUID();
    if (skinUUID.isEmpty()) {
      return defaultResourceLocation;
    }

    // Check if there is already any cached resource location.
    TextureModelKey textureModelKey = new TextureModelKey(skinUUID.get(), entity.getSkinModel());
    ResourceLocation resourceLocation = customTextureCache.get(textureModelKey);
    if (resourceLocation != null) {
      return resourceLocation;
    }

    return defaultResourceLocation;
  }

  public static void registerTexture(SkinModel skinModel, File textureFile) {
    UUID uuid = UUID.nameUUIDFromBytes(textureFile.getName().getBytes());
    TextureModelKey textureModelKey = new TextureModelKey(uuid, skinModel);
    registerTexture(textureModelKey, textureFile);
  }

  public static ResourceLocation registerTexture(
      TextureModelKey textureModelKey, File textureFile) {
    log.info(
        "{} Registering custom texture {} with UUID {}.",
        LOG_PREFIX,
        textureFile.getName(),
        textureModelKey.getUUID());
    ResourceLocation resourceLocation =
        TextureManager.addCustomTexture(textureModelKey, textureFile);
    if (resourceLocation != null) {
      customTextureCache.put(textureModelKey, resourceLocation);
    }
    return resourceLocation;
  }

  public static void clearCustomTextureCache() {
    customTextureCache.clear();
  }
}
