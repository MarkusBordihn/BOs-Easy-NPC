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
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ExecuteDialogButtonActionMessage(UUID uuid, UUID dialogId, UUID dialogButtonId)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "dialog_button_action");
  public static final CustomPacketPayload.Type<ExecuteDialogButtonActionMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, ExecuteDialogButtonActionMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ExecuteDialogButtonActionMessage::create);

  public static ExecuteDialogButtonActionMessage create(final FriendlyByteBuf buffer) {
    return new ExecuteDialogButtonActionMessage(
        buffer.readUUID(), buffer.readUUID(), buffer.readUUID());
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
    EasyNPC<?> easyNPC = getEasyNPC(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate dialog id.
    if (this.dialogId == null) {
      log.error("Invalid dialog id for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog button id.
    if (this.dialogButtonId == null) {
      log.error("Invalid dialog button id for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Dialog data for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog button actions.
    if (!dialogData.hasDialogButton(this.dialogId, this.dialogButtonId)) {
      log.error(
          "Unknown dialog button action {} request for dialog {} for {} from {}",
          this.dialogButtonId,
          this.dialogId,
          easyNPC,
          serverPlayer);
      return;
    }

    // Validate dialog button data.
    DialogButtonEntry dialogButtonEntry = dialogData.getDialogButton(dialogId, dialogButtonId);
    if (dialogButtonEntry == null) {
      log.error(
          "Unable to get valid dialog button data for {} and dialog {} from {}",
          easyNPC,
          this.dialogId,
          serverPlayer);
      return;
    }

    // Validate dialog button actions.
    ActionDataSet actionDataSet = dialogButtonEntry.getActionDataSet();
    if (actionDataSet == null || actionDataSet.isEmpty()) {
      log.error(
          "Empty dialog button action {} request for {} and dialog {} from {}",
          this.dialogButtonId,
          easyNPC,
          this.dialogId,
          serverPlayer);
      return;
    }

    // Validate action handler.
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    if (actionHandler == null) {
      log.error("Unable to get valid action handler for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    actionHandler.executeActions(actionDataSet, serverPlayer);
  }
}
