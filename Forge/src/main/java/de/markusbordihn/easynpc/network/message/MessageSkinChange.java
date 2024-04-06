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

package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.handler.SkinHandler;
import de.markusbordihn.easynpc.network.NetworkMessage;
import de.markusbordihn.easynpc.utils.PlayersUtils;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageSkinChange extends NetworkMessage {

  protected final String skinName;
  protected final String skinURL;
  protected final UUID skinUUID;
  protected final SkinType skinType;
  protected final String skinVariant;

  public MessageSkinChange(
      UUID uuid,
      String skinName,
      String skinURL,
      UUID skinUUID,
      SkinType skinType,
      String skinVariant) {
    super(uuid);
    this.skinName = skinName;
    this.skinURL = skinURL;
    this.skinUUID = skinUUID;
    this.skinType = skinType;
    this.skinVariant = skinVariant;
  }

  public static MessageSkinChange decode(final FriendlyByteBuf buffer) {
    return new MessageSkinChange(
        buffer.readUUID(),
        buffer.readUtf(),
        buffer.readUtf(),
        buffer.readUUID(),
        buffer.readEnum(SkinType.class),
        buffer.readUtf());
  }

  public static void encode(final MessageSkinChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getSkinName());
    buffer.writeUtf(message.getSkinURL());
    buffer.writeUUID(message.getSkinUUID());
    buffer.writeEnum(message.getSkinType());
    buffer.writeUtf(message.getSkinVariant());
  }

  public static void handle(MessageSkinChange message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageSkinChange message, CustomPayloadEvent.Context context) {
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

    // Validate skin data.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.error("Skin data for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    // Validate variant data.
    String skinVariant = message.getSkinVariant();

    // Perform action.
    String skinURL = message.getSkinURL();
    UUID skinUUID = message.getSkinUUID();

    switch (skinType) {
      case NONE:
        SkinHandler.setNoneSkin(easyNPC);
        break;
      case CUSTOM:
        SkinHandler.setCustomSkin(easyNPC, skinUUID);
        break;
      case DEFAULT:
        SkinHandler.setDefaultSkin(easyNPC, skinVariant);
        break;
      case PLAYER_SKIN:
        UUID userUUID =
            skinUUID != null && !Constants.BLANK_UUID.equals(skinUUID)
                ? skinUUID
                : PlayersUtils.getUserUUID(serverPlayer.getServer(), skinName);
        if (userUUID != null && !skinName.equals(userUUID.toString())) {
          log.debug("Converted user {} to UUID {} ...", skinName, userUUID);
        }
        SkinHandler.setPlayerSkin(easyNPC, skinName, userUUID);
        break;
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL:
        SkinHandler.setRemoteSkin(easyNPC, skinURL);
        break;
      default:
        log.error(
            "Failed processing skin:{} uuid:{} url:{} type:{} for {} from {}",
            skinName,
            skinUUID,
            skinURL,
            skinType,
            easyNPC,
            serverPlayer);
        skinData.setSkinType(skinType);
        skinData.setSkinName(skinName);
    }
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

  public String getSkinVariant() {
    return this.skinVariant;
  }
}
