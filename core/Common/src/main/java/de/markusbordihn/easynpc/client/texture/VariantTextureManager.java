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
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class VariantTextureManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final EnumMap<SkinModel, Map<Enum<?>, ResourceLocation>> textureRegistry =
      new EnumMap<>(SkinModel.class);
  private static final String LOG_PREFIX = "[Variant Texture Manager] ";

  private VariantTextureManager() {}

  public static void registerVariantTexture(
      SkinModel skinModel, Enum<?> variantType, ResourceLocation texture) {
    if (skinModel == null || variantType == null || texture == null) {
      log.warn(
          "{} Invalid registration attempt: skinModel={}, variantType={}, texture={}",
          LOG_PREFIX,
          skinModel,
          variantType,
          texture);
      return;
    }
    textureRegistry.computeIfAbsent(skinModel, k -> new HashMap<>()).put(variantType, texture);
    log.debug(
        "{} Registered variant texture for {} - {}: {}",
        LOG_PREFIX,
        skinModel,
        variantType,
        texture);
  }

  public static void registerVariantTextures(
      SkinModel skinModel, Map<? extends Enum<?>, ResourceLocation> textures) {
    if (skinModel == null || textures == null || textures.isEmpty()) {
      log.warn(
          "{} Invalid bulk registration attempt: skinModel={}, textures={}",
          LOG_PREFIX,
          skinModel,
          textures);
      return;
    }
    Map<Enum<?>, ResourceLocation> variantMap =
        textureRegistry.computeIfAbsent(skinModel, k -> new HashMap<>());
    variantMap.putAll(textures);
    log.debug("{} Registered {} variant textures for {}", LOG_PREFIX, textures.size(), skinModel);
  }

  public static ResourceLocation getVariantTexture(SkinModel skinModel, Enum<?> variantType) {
    if (skinModel == null || variantType == null) {
      return Constants.BLANK_ENTITY_TEXTURE;
    }
    Map<Enum<?>, ResourceLocation> variantMap = textureRegistry.get(skinModel);
    if (variantMap == null) {
      return Constants.BLANK_ENTITY_TEXTURE;
    }
    return variantMap.getOrDefault(variantType, Constants.BLANK_ENTITY_TEXTURE);
  }

  public static ResourceLocation getVariantTextureOrDefault(
      SkinModel skinModel, Enum<?> variantType, ResourceLocation defaultTexture) {
    ResourceLocation texture = getVariantTexture(skinModel, variantType);
    return texture.equals(Constants.BLANK_ENTITY_TEXTURE) ? defaultTexture : texture;
  }

  public static boolean hasVariantTexture(SkinModel skinModel, Enum<?> variantType) {
    if (skinModel == null || variantType == null) {
      return false;
    }
    Map<Enum<?>, ResourceLocation> variantMap = textureRegistry.get(skinModel);
    return variantMap != null && variantMap.containsKey(variantType);
  }

  public static Map<Enum<?>, ResourceLocation> getVariantTextures(SkinModel skinModel) {
    return textureRegistry.get(skinModel);
  }

  public static void clearVariantTextureCache() {
    textureRegistry.clear();
    log.info("{} Cleared variant texture cache", LOG_PREFIX);
  }
}
