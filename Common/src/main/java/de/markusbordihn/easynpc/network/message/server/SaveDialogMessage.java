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
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record SaveDialogMessage(UUID uuid, UUID dialogId, DialogDataEntry dialogDataEntry)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "save_dialog");
  public static final CustomPacketPayload.Type<SaveDialogMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, SaveDialogMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), SaveDialogMessage::create);

  public static SaveDialogMessage create(final FriendlyByteBuf buffer) {
    return new SaveDialogMessage(
        buffer.readUUID(), buffer.readUUID(), new DialogDataEntry(buffer.readNbt()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.dialogId);
    buffer.writeNbt(this.dialogDataEntry.createTag());
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate dialog ID
    if (this.dialogId == null) {
      log.error("Invalid dialog id for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data entry.
    if (this.dialogDataEntry == null) {
      log.error("Invalid dialog data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog
    if (!dialogData.hasDialog(this.dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          this.dialogId,
          easyNPC,
          serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Saving dialog data {} for dialog {} for {} from {}",
        this.dialogDataEntry,
        this.dialogId,
        easyNPC,
        serverPlayer);
    dialogData.setDialog(this.dialogId, this.dialogDataEntry);
  }
}
