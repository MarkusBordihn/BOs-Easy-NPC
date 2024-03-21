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

import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageRemoveDialogButton extends NetworkMessage {

  protected final UUID dialogId;
  protected final UUID dialogButtonId;

  public MessageRemoveDialogButton(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    super(uuid);
    this.dialogId = dialogId;
    this.dialogButtonId = dialogButtonId;
  }

  public static MessageRemoveDialogButton decode(final FriendlyByteBuf buffer) {
    return new MessageRemoveDialogButton(buffer.readUUID(), buffer.readUUID(), buffer.readUUID());
  }

  public static void encode(final MessageRemoveDialogButton message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeUUID(message.getDialogButtonId());
  }

  public static void handle(MessageRemoveDialogButton message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageRemoveDialogButton message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      log.error("Unable to remove dialog with message {} from {}", message, context);
      return;
    }

    // Validate dialog ID
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, context);
      return;
    }

    // Validate dialog button ID
    UUID dialogButtonId = message.getDialogButtonId();
    if (dialogButtonId == null) {
      log.error("Invalid dialog button id for {} from {}", message, context);
      return;
    }

    // Validate entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate dialog data
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", message, context);
      return;
    }

    // Validate dialog button
    if (!dialogData.hasDialog(dialogId) || !dialogData.hasDialogButton(dialogId, dialogButtonId)) {
      log.error(
          "Unknown delete dialog request for dialog button {} for dialog {} for {} from {}",
          dialogButtonId,
          dialogId,
          uuid,
          serverPlayer);
      return;
    }

    // Perform action.
    if (dialogData.removeDialogButton(dialogId, dialogButtonId)) {
      log.info(
          "Removed dialog button {} from dialog {} for {} from {}",
          dialogButtonId,
          dialogId,
          easyNPC,
          serverPlayer);
    } else {
      log.warn(
          "Unable to remove dialog button {} from dialog {} for {} from {}",
          dialogButtonId,
          dialogId,
          easyNPC,
          serverPlayer);
    }
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public UUID getDialogButtonId() {
    return this.dialogButtonId;
  }
}
