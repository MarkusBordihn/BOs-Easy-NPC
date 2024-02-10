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

import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class MessageSaveDialog extends NetworkMessage {

  protected final UUID dialogId;
  protected final DialogDataEntry dialogData;

  public MessageSaveDialog(UUID uuid, UUID dialogId, DialogDataEntry dialogData) {
    super(uuid);
    this.dialogId = dialogId;
    this.dialogData = dialogData;
  }

  public static MessageSaveDialog decode(final FriendlyByteBuf buffer) {
    return new MessageSaveDialog(
        buffer.readUUID(), buffer.readUUID(), new DialogDataEntry(buffer.readNbt()));
  }

  public static void encode(final MessageSaveDialog message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeNbt(message.getDialogData().createTag());
  }

  public static void handle(
      MessageSaveDialog message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageSaveDialog message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      log.error("Unable to save dialog with message {} from {}", message, context);
      return;
    }

    // Validate dialog ID
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, context);
      return;
    }

    // Validate dialog data
    DialogDataEntry dialogData = message.getDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", message, context);
      return;
    }

    // Validate entity.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate dialog
    if (!easyNPCEntity.hasDialog(dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          dialogId,
          uuid,
          serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Saving dialog data {} for dialog {} for {} from {}",
        dialogData,
        dialogId,
        uuid,
        serverPlayer);
    easyNPCEntity.setDialog(dialogId, dialogData);
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public DialogDataEntry getDialogData() {
    return this.dialogData;
  }
}
