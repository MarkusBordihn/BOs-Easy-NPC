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
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record OpenDialogButtonEditorMessage(UUID uuid, UUID dialogId, UUID dialogButtonId)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_dialog_button_editor");
  public static final CustomPacketPayload.Type<OpenDialogButtonEditorMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, OpenDialogButtonEditorMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), OpenDialogButtonEditorMessage::create);

  public static OpenDialogButtonEditorMessage create(final FriendlyByteBuf buffer) {
    return new OpenDialogButtonEditorMessage(
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
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate dialog id.
    if (this.dialogId == null) {
      log.error("Invalid dialog id for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Unable to get valid dialog data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data set.
    DialogDataSet dialogDataSet = dialogData.getDialogDataSet();
    if (dialogDataSet == null) {
      log.error("Unable to get valid dialog data set for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog(this.dialogId);
    if (dialogDataEntry == null) {
      log.error(
          "Unable to get valid dialog data for dialog {} for {} from {}",
          easyNPC,
          this.dialogId,
          serverPlayer);
      return;
    }

    // Validate dialog button id and create new button if needed.
    UUID newDialogButtonId = this.dialogButtonId;
    if (this.dialogButtonId != null && this.dialogButtonId.equals(EMPTY_UUID)) {
      DialogButtonEntry newDialogButton =
          new DialogButtonEntry("Button " + RANDOM.nextInt(1000), DialogButtonType.DEFAULT);
      log.info(
          "Created new dialog button {} for dialog {} for {} from {}",
          newDialogButton,
          this.dialogId,
          easyNPC,
          serverPlayer);
      dialogDataEntry.setDialogButton(newDialogButton);
      newDialogButtonId = newDialogButton.getId();
    } else if (dialogButtonId != null && !dialogData.hasDialogButton(dialogId, dialogButtonId)) {
      log.error(
          "Invalid dialog button id {} for {} from {}", dialogButtonId, easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Open dialog button editor for dialog {} and button {} for {} from {}",
        this.dialogId,
        newDialogButtonId,
        easyNPC,
        serverPlayer);
    MenuManager.getMenuHandler()
        .openEditorMenu(
            EditorType.DIALOG_BUTTON, serverPlayer, easyNPC, dialogId, newDialogButtonId, 0);
  }
}
