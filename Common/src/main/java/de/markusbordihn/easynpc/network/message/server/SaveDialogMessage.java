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

package de.markusbordihn.easynpc.network.message.server;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class SaveDialogMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "save_dialog");

  protected final UUID dialogId;
  protected final DialogDataEntry dialogDataEntry;

  public SaveDialogMessage(
      final UUID uuid, final UUID dialogId, final DialogDataEntry dialogDataEntry) {
    super(uuid);
    this.dialogId = dialogId;
    this.dialogDataEntry = dialogDataEntry;
  }

  public static SaveDialogMessage decode(final FriendlyByteBuf buffer) {
    return new SaveDialogMessage(
        buffer.readUUID(), buffer.readUUID(), new DialogDataEntry(buffer.readNbt()));
  }

  public static FriendlyByteBuf encode(
      final SaveDialogMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeNbt(message.getDialogDataEntry().createTag());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final SaveDialogMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate dialog ID
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, serverPlayer);
      return;
    }

    // Validate dialog data entry.
    DialogDataEntry dialogDataEntry = message.getDialogDataEntry();
    if (dialogDataEntry == null) {
      log.error("Invalid dialog data for {} from {}", message, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", message.getUUID(), serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", message, serverPlayer);
      return;
    }

    // Validate dialog
    if (!dialogData.hasDialog(dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          dialogId,
          message.getUUID(),
          serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Saving dialog data {} for dialog {} for {} from {}",
        dialogDataEntry,
        dialogId,
        message.getUUID(),
        serverPlayer);
    dialogData.setDialog(dialogId, dialogDataEntry);
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public DialogDataEntry getDialogDataEntry() {
    return this.dialogDataEntry;
  }
}
