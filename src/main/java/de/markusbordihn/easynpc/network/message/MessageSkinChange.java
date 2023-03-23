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

package de.markusbordihn.easynpc.network.message;

import java.util.UUID;
import java.util.function.Supplier;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.skin.SkinType;
import de.markusbordihn.easynpc.utils.PlayersUtils;

public class MessageSkinChange {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final UUID uuid;
  protected final String skin;
  protected final String skinURL;
  protected final UUID skinUUID;
  protected final String skinType;

  public MessageSkinChange(UUID uuid, String skin, String skinURL, UUID skinUUID, String skinType) {
    this.uuid = uuid;
    this.skin = skin;
    this.skinURL = skinURL;
    this.skinUUID = skinUUID;
    this.skinType = skinType;
  }

  public UUID getUUID() {
    return this.uuid;
  }

  public String getSkin() {
    return this.skin;
  }

  public String getSkinURL() {
    return this.skinURL;
  }

  public UUID getSkinUUID() {
    return this.skinUUID;
  }

  public String getSkinType() {
    return this.skinType;
  }

  public static void handle(MessageSkinChange message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageSkinChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !MessageHelper.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate skin.
    String skin = message.getSkin();
    if (skin == null) {
      log.error("Invalid skin {} for {} from {}", skin, message, serverPlayer);
      return;
    }

    // Validate skin type.
    String skinTypeName = message.getSkinType();
    if (skinTypeName == null || skinTypeName.isEmpty()) {
      log.error("Invalid skin type name {} for {} from {}", skin, message, serverPlayer);
      return;
    }
    SkinType skinType = SkinType.get(skinTypeName);

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    String skinURL = message.getSkinURL();
    UUID skinUUID = message.getSkinUUID();

    // Pre-process skin information, if needed.
    log.debug("Processing skin:{} uuid:{} url:{} type:{} model:{} for {} from {}", skin, skinUUID, skinURL,
        skinType, easyNPCEntity.getSkinModel(), easyNPCEntity, serverPlayer);

    switch (skinType) {
      case CUSTOM:
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkin(skin);
        easyNPCEntity.setSkinURL("");
        easyNPCEntity.setSkinUUID(Constants.BLANK_UUID);
        break;
      case DEFAULT:
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkin("");
        easyNPCEntity.setSkinURL("");
        easyNPCEntity.setSkinUUID(Constants.BLANK_UUID);
        break;
      case PLAYER_SKIN:
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkin("");
        easyNPCEntity.setSkinURL(skinURL != null && !skinURL.isBlank() ? skinURL : "");
        if (skinUUID != null && !Constants.BLANK_UUID.equals(skinUUID)) {
          easyNPCEntity.setSkinUUID(skinUUID);
        } else {
          UUID userUUID = PlayersUtils.getUserUUID(serverPlayer.getServer(), skin);
          if (!skin.equals(userUUID.toString())) {
            log.debug("Converted user {} to UUID {} ...", skin, userUUID);
          }
          easyNPCEntity.setSkinUUID(userUUID);
        }
        break;
      case INSECURE_REMOTE_URL:
      case SECURE_REMOTE_URL:
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkin("");
        easyNPCEntity.setSkinURL(skinURL != null && !skinURL.isBlank() ? skinURL : skin);
        easyNPCEntity
            .setSkinUUID(skinUUID != null && !Constants.BLANK_UUID.equals(skinUUID) ? skinUUID
                : UUID.nameUUIDFromBytes(skin.getBytes()));
        break;
      default:
        log.error("Failed processing skin:{} uuid:{} url:{} type:{} for {} from {}", skin, skinUUID,
            skinURL, skinType, easyNPCEntity, serverPlayer);
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkin(skin);
    }
  }

}
