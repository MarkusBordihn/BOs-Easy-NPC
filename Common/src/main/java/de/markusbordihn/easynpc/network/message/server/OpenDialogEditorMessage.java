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

public record OpenDialogEditorMessage(UUID uuid, UUID dialogId) implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_dialog_editor");
  public static final CustomPacketPayload.Type<OpenDialogEditorMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, OpenDialogEditorMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), OpenDialogEditorMessage::create);

  public static OpenDialogEditorMessage create(final FriendlyByteBuf buffer) {
    return new OpenDialogEditorMessage(buffer.readUUID(), buffer.readUUID());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUUID(this.dialogId);
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

    // Validate dialog id and create new dialog if needed.
    UUID newDialogId = this.dialogId;
    if (this.dialogId.equals(EMPTY_UUID)) {
      String dialogName =
          dialogData.getDialogDataSet().hasDialog() ? "Dialog " + RANDOM.nextInt(1000) : "Default";
      DialogDataEntry newDialogData = new DialogDataEntry(dialogName);
      log.info("Create new dialog {} for {} from {}", newDialogData, easyNPC, serverPlayer);
      dialogData.getDialogDataSet().addDialog(newDialogData);
      newDialogId = newDialogData.getId();
    } else if (!dialogData.hasDialog(this.dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          this.dialogId,
          easyNPC,
          serverPlayer);
      log.debug("Available dialogs for {} are {}", easyNPC, dialogData.getDialogDataSet());
      return;
    }

    // Perform action.
    log.info(
        "Open dialog editor with for dialog {} for {} from {}", newDialogId, easyNPC, serverPlayer);
    MenuManager.getMenuHandler()
        .openEditorMenu(EditorType.DIALOG, serverPlayer, easyNPC, newDialogId, 0);
  }
}
