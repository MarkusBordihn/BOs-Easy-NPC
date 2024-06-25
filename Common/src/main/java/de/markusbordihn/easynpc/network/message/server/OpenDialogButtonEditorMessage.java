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
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class OpenDialogButtonEditorMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_dialog_button_editor");

  protected final UUID dialogId;
  protected final UUID dialogButtonId;

  public OpenDialogButtonEditorMessage(UUID uuid, UUID dialogId) {
    this(uuid, dialogId, null, 0);
  }

  public OpenDialogButtonEditorMessage(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    this(uuid, dialogId, dialogButtonId, 0);
  }

  public OpenDialogButtonEditorMessage(
      UUID uuid, UUID dialogId, UUID dialogButtonId, int pageIndex) {
    super(uuid, pageIndex);
    this.dialogId = dialogId;
    this.dialogButtonId = dialogButtonId;
  }

  public static OpenDialogButtonEditorMessage decode(final FriendlyByteBuf buffer) {
    return new OpenDialogButtonEditorMessage(
        buffer.readUUID(), buffer.readUUID(), buffer.readUUID(), buffer.readInt());
  }

  public static FriendlyByteBuf encode(
      final OpenDialogButtonEditorMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeUUID(message.getDialogButtonId());
    buffer.writeInt(message.pageIndex);
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      OpenDialogButtonEditorMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate dialog id.
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, serverPlayer);
      return;
    }

    // Validate page index.
    int pageIndex = message.getPageIndex();
    if (pageIndex < 0) {
      log.error("Invalid page index {} for {} from {}", pageIndex, message, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPC<?> easyNPC =
        LivingEntityManager.getEasyNPCEntityByUUID(message.getUUID(), serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", message.getUUID(), serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Unable to get valid dialog data for {} from {}", message.getUUID(), serverPlayer);
      return;
    }

    // Validate dialog data set.
    DialogDataSet dialogDataSet = dialogData.getDialogDataSet();
    if (dialogDataSet == null) {
      log.error(
          "Unable to get valid dialog data set for {} from {}", message.getUUID(), serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog(dialogId);
    if (dialogDataEntry == null) {
      log.error(
          "Unable to get valid dialog data for dialog {} for {} from {}",
          message.getUUID(),
          dialogId,
          serverPlayer);
      return;
    }

    // Validate dialog button id and create new button if needed.
    UUID dialogButtonId = message.getDialogButtonId();
    if (dialogButtonId != null && dialogButtonId.equals(EMPTY_UUID)) {
      DialogButtonData newDialogButton =
          new DialogButtonData("Button " + RANDOM.nextInt(1000), DialogButtonType.DEFAULT);
      log.info(
          "Created new dialog button {} for dialog {} for {} from {}",
          newDialogButton,
          dialogId,
          message.getUUID(),
          serverPlayer);
      dialogDataEntry.setDialogButton(newDialogButton);
      dialogButtonId = newDialogButton.getId();
    } else if (dialogButtonId != null && !dialogData.hasDialogButton(dialogId, dialogButtonId)) {
      log.error(
          "Invalid dialog button id {} for {} from {}", dialogButtonId, message, serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Open dialog button editor for dialog {} and button {} for {} from {}",
        dialogId,
        dialogButtonId,
        message.getUUID(),
        serverPlayer);
    MenuManager.getMenuHandler()
        .openEditorMenu(
            EditorType.DIALOG_BUTTON, serverPlayer, easyNPC, dialogId, dialogButtonId, pageIndex);
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public UUID getDialogButtonId() {
    return this.dialogButtonId;
  }
}
