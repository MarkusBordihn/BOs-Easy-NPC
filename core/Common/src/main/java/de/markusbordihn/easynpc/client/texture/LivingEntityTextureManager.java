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
import de.markusbordihn.easynpc.data.skin.VariantTexture;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import java.util.function.Supplier;
import net.minecraft.resources.ResourceLocation;

public class LivingEntityTextureManager {

  private LivingEntityTextureManager() {}

  public static ResourceLocation getTextureByVariant(
      Enum<?> variant, ResourceLocation defaultTexture) {
    if (variant instanceof VariantTexture variantTexture) {
      return variantTexture.getTextureLocation();
    }
    return defaultTexture;
  }

  public static ResourceLocation getCustomTexture(
      SkinDataCapable<?> entity, ResourceLocation defaultTexture) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, defaultTexture);
  }

  public static ResourceLocation getPlayerTexture(
      SkinDataCapable<?> entity, ResourceLocation defaultTexture) {
    return PlayerTextureManager.getOrCreateTextureWithDefault(entity, defaultTexture);
  }

  public static ResourceLocation getRemoteTexture(
      SkinDataCapable<?> entity, ResourceLocation defaultTexture) {
    return RemoteTextureManager.getOrCreateTextureWithDefault(entity, defaultTexture);
  }

  public static ResourceLocation getEntityTexture(
      EasyNPC<?> easyNPC, ResourceLocation defaultTexture) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData, defaultTexture);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData, defaultTexture);
      default ->
          getTextureByVariant(easyNPC.getEasyNPCVariantData().getSkinVariantType(), defaultTexture);
    };
  }

  public static ResourceLocation getEntityPlayerTexture(
      EasyNPC<?> easyNPC, ResourceLocation defaultTexture) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData, defaultTexture);
      case PLAYER_SKIN -> getPlayerTexture(skinData, defaultTexture);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData, defaultTexture);
      default ->
          getTextureByVariant(easyNPC.getEasyNPCVariantData().getSkinVariantType(), defaultTexture);
    };
  }

  public static ResourceLocation getEntityTextureWithDefaultCallback(
      EasyNPC<?> easyNPC,
      ResourceLocation defaultTexture,
      Supplier<ResourceLocation> defaultTextureSupplier) {
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData, defaultTexture);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData, defaultTexture);
      default -> defaultTextureSupplier.get();
    };
  }
}
