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

public record RemoveDialogButtonMessage(UUID uuid, UUID dialogId, UUID dialogButtonId)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "remove_dialog_button");
  public static final CustomPacketPayload.Type<RemoveDialogButtonMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, RemoveDialogButtonMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), RemoveDialogButtonMessage::create);

  public static RemoveDialogButtonMessage create(final FriendlyByteBuf buffer) {
    return new RemoveDialogButtonMessage(buffer.readUUID(), buffer.readUUID(), buffer.readUUID());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.dialogId);
    buffer.writeUUID(this.dialogButtonId);
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

    // Validate dialog button ID
    if (this.dialogButtonId == null) {
      log.error("Invalid dialog button id for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog button
    if (!dialogData.hasDialog(this.dialogId)
        || !dialogData.hasDialogButton(this.dialogId, this.dialogButtonId)) {
      log.error(
          "Unknown delete dialog request for dialog button {} for dialog {} for {} from {}",
          this.dialogButtonId,
          this.dialogId,
          easyNPC,
          serverPlayer);
      return;
    }

    // Perform action.
    if (dialogData.removeDialogButton(this.dialogId, this.dialogButtonId)) {
      log.info(
          "Removed dialog button {} from dialog {} for {} from {}",
          this.dialogButtonId,
          this.dialogId,
          easyNPC,
          serverPlayer);
    } else {
      log.warn(
          "Unable to remove dialog button {} from dialog {} for {} from {}",
          this.dialogButtonId,
          this.dialogId,
          easyNPC,
          serverPlayer);
    }
  }
}
