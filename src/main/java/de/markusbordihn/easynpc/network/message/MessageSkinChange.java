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

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import de.markusbordihn.easynpc.utils.PlayersUtils;

public class MessageSkinChange extends NetworkMessage {

  protected final String skinName;
  protected final String skinURL;
  protected final UUID skinUUID;
  protected final SkinType skinType;

  public MessageSkinChange(UUID uuid, String skinName, String skinURL, UUID skinUUID,
      SkinType skinType) {
    super(uuid);
    this.skinName = skinName;
    this.skinURL = skinURL;
    this.skinUUID = skinUUID;
    this.skinType = skinType;
  }

  public String getSkinName() {
    return this.skinName;
  }

  public String getSkinURL() {
    return this.skinURL;
  }

  public UUID getSkinUUID() {
    return this.skinUUID;
  }

  public SkinType getSkinType() {
    return this.skinType;
  }

  public static MessageSkinChange decode(final FriendlyByteBuf buffer) {
    return new MessageSkinChange(buffer.readUUID(), buffer.readUtf(), buffer.readUtf(),
        buffer.readUUID(), buffer.readEnum(SkinType.class));
  }

  public static void encode(final MessageSkinChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getSkinName());
    buffer.writeUtf(message.getSkinURL());
    buffer.writeUUID(message.getSkinUUID());
    buffer.writeEnum(message.getSkinType());
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
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate skin.
    String skinName = message.getSkinName();
    if (skinName == null) {
      log.error("Invalid skin {} for {} from {}", skinName, message, serverPlayer);
      return;
    }

    // Validate skin type.
    SkinType skinType = message.getSkinType();
    if (skinType == null) {
      log.error("Invalid skin type {} for {} from {}", skinType, message, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    String skinURL = message.getSkinURL();
    UUID skinUUID = message.getSkinUUID();

    switch (skinType) {
      case CUSTOM:
        log.debug("Setting custom skin for {} to {} from {}", easyNPCEntity, skinUUID,
            serverPlayer);
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkinName("");
        easyNPCEntity.setSkinURL("");
        easyNPCEntity.setSkinUUID(skinUUID);
        break;
      case DEFAULT:
        log.debug("Setting default skin for {} to {} from {}", easyNPCEntity, skinType,
            serverPlayer);
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkinName("");
        easyNPCEntity.setSkinURL("");
        easyNPCEntity.setSkinUUID(Constants.BLANK_UUID);
        break;
      case PLAYER_SKIN:
        log.debug("Setting player skin for {} to {} from {}", easyNPCEntity, skinURL, serverPlayer);
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkinName(skinName);
        easyNPCEntity.setSkinURL(skinURL != null && !skinURL.isBlank() ? skinURL : "");
        if (skinUUID != null && !Constants.BLANK_UUID.equals(skinUUID)) {
          easyNPCEntity.setSkinUUID(skinUUID);
        } else {
          UUID userUUID = PlayersUtils.getUserUUID(serverPlayer.getServer(), skinName);
          if (!skinName.equals(userUUID.toString())) {
            log.debug("Converted user {} to UUID {} ...", skinName, userUUID);
          }
          easyNPCEntity.setSkinUUID(userUUID);
        }
        break;
      case INSECURE_REMOTE_URL:
      case SECURE_REMOTE_URL:
        log.debug("Setting remote skin for {} to {} from {}", easyNPCEntity, skinURL, serverPlayer);
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkinName("");
        easyNPCEntity.setSkinURL(skinURL != null && !skinURL.isBlank() ? skinURL : skinName);
        easyNPCEntity
            .setSkinUUID(skinUUID != null && !Constants.BLANK_UUID.equals(skinUUID) ? skinUUID
                : UUID.nameUUIDFromBytes(skinName.getBytes()));
        break;
      default:
        log.error("Failed processing skin:{} uuid:{} url:{} type:{} for {} from {}", skinName, skinUUID,
            skinURL, skinType, easyNPCEntity, serverPlayer);
        easyNPCEntity.setSkinType(skinType);
        easyNPCEntity.setSkinName(skinName);
    }
  }

}
