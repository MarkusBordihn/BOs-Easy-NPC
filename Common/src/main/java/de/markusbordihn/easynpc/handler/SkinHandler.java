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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.validator.UrlValidator;
import java.util.UUID;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SkinHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private SkinHandler() {
  }

  public static boolean setNoneSkin(EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      log.error("[{}] Error setting none skin", easyNPC);
      return false;
    }
    log.debug("[{}] Setting none skin", easyNPC);
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null) {
      skinData.setSkinType(SkinType.NONE);
      return true;
    }
    return false;
  }

  public static boolean setDefaultSkin(EasyNPC<?> easyNPC, String variant) {
    if (easyNPC == null || variant == null || variant.isEmpty()) {
      log.error("[{}] Error setting default skin to variant {}", easyNPC, variant);
      return false;
    }

    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null && skinData.getSkinType() != SkinType.DEFAULT) {
      log.debug(
          "[{}] Set skin type from {} to {}", easyNPC, skinData.getSkinType(), SkinType.DEFAULT);
      skinData.setSkinType(SkinType.DEFAULT);
      skinData.setSkinName("");
      skinData.setSkinURL("");
      skinData.setSkinUUID(Constants.BLANK_UUID);
    }

    VariantData<?> variantData = skinData.getEasyNPCVariantData();
    if (variantData != null) {
      log.debug("[{}] Set default skin to {}", easyNPC, variant);
      variantData.setVariant(variant);
      return true;
    }
    return false;
  }

  public static boolean setCustomSkin(EasyNPC<?> easyNPC, UUID skinUUID) {
    if (easyNPC == null || skinUUID == null || skinUUID.equals(Constants.BLANK_UUID)) {
      log.error("[{}] Error setting custom skin to UUID {}", easyNPC, skinUUID);
      return false;
    }

    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null) {
      log.debug("[{}] Set custom skin to UUID {}", easyNPC, skinUUID);
      skinData.setSkinType(SkinType.CUSTOM);
      skinData.setSkinName("");
      skinData.setSkinURL("");
      skinData.setSkinUUID(skinUUID);
      return true;
    }
    return false;
  }

  public static boolean setPlayerSkin(EasyNPC<?> easyNPC, String playerName, UUID playerUUID) {
    if (easyNPC == null
        || playerName == null
        || playerName.isEmpty()
        || playerUUID == null
        || playerUUID.equals(Constants.BLANK_UUID)) {
      log.error(
          "[{}] Error setting player skin to {} with UUID {}", easyNPC, playerName, playerUUID);
      return false;
    }

    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null) {
      log.debug("[{}] Setting player skin to {} with UUID {}", easyNPC, playerName, playerUUID);
      skinData.setSkinType(SkinType.PLAYER_SKIN);
      skinData.setSkinName(playerName);
      skinData.setSkinURL("");
      skinData.setSkinUUID(playerUUID);
      return true;
    }
    return false;
  }

  public static boolean setRemoteSkin(EasyNPC<?> easyNPC, String skinURL) {
    if (isSecureRemoteSkin(skinURL)) {
      return setSecureRemoteSkin(easyNPC, skinURL);
    }
    return setInsecureRemoteSkin(easyNPC, skinURL);
  }

  public static boolean setSecureRemoteSkin(EasyNPC<?> easyNPC, String skinURL) {
    if (easyNPC == null || !isSecureRemoteSkin(skinURL) || !UrlValidator.isValidUrl(skinURL)) {
      log.error("[{}] Error setting secure remote skin to URL {}", easyNPC, skinURL);
      return false;
    }

    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null) {
      log.debug("[{}] Set secure remote skin to URL {}", easyNPC, skinURL);
      skinData.setSkinType(SkinType.SECURE_REMOTE_URL);
      skinData.setSkinName("");
      skinData.setSkinURL(skinURL);
      skinData.setSkinUUID(UUID.nameUUIDFromBytes(skinURL.getBytes()));
      return true;
    }
    return false;
  }

  public static boolean setInsecureRemoteSkin(EasyNPC<?> easyNPC, String skinURL) {
    if (easyNPC == null
        || skinURL == null
        || skinURL.isEmpty()
        || !UrlValidator.isValidUrl(skinURL)) {
      log.error("[{}] Error setting insecure remote skin to URL {}", easyNPC, skinURL);
      return false;
    }

    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null) {
      log.debug("[{}] Set insecure remote skin to URL {}", easyNPC, skinURL);
      skinData.setSkinType(SkinType.INSECURE_REMOTE_URL);
      skinData.setSkinName("");
      skinData.setSkinURL(skinURL);
      skinData.setSkinUUID(UUID.nameUUIDFromBytes(skinURL.getBytes()));
      return true;
    }
    return false;
  }

  private static boolean isSecureRemoteSkin(String skinURL) {
    return skinURL != null
        && !skinURL.isEmpty()
        && (skinURL.startsWith("https://www.minecraftskins.com/")
        || skinURL.startsWith("http://novask.in/")
        || skinURL.startsWith("https://minecraft.novaskin.me/")
        || skinURL.startsWith("https://mcskins.top/")
        || skinURL.startsWith("https://skinmc.net/") && UrlValidator.isValidUrl(skinURL));
  }
}
