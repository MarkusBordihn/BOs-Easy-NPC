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
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class OpenDialogTextEditorMessage extends NetworkMessage<OpenDialogTextEditorMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "open_dialog_text_editor");

  protected final UUID dialogId;

  public OpenDialogTextEditorMessage(UUID uuid, UUID dialogId) {
    this(uuid, dialogId, 0);
  }

  public OpenDialogTextEditorMessage(UUID uuid, UUID dialogId, int pageIndex) {
    super(uuid, pageIndex);
    this.dialogId = dialogId;
  }

  public static OpenDialogTextEditorMessage decode(final FriendlyByteBuf buffer) {
    return new OpenDialogTextEditorMessage(buffer.readUUID(), buffer.readUUID(), buffer.readInt());
  }

  public static FriendlyByteBuf encode(
      final OpenDialogTextEditorMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.dialogId);
    buffer.writeInt(message.pageIndex);
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(OpenDialogTextEditorMessage message, final ServerPlayer serverPlayer) {
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

    // Validate dialog id.
    if (!dialogData.hasDialog(dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          dialogId,
          message.getUUID(),
          serverPlayer);
      log.debug(
          "Available dialogs for {} are {}", message.getUUID(), dialogData.getDialogDataSet());
      return;
    }

    // Perform action.
    log.info(
        "Open dialog editor with for dialog {} for {} from {}",
        dialogId,
        message.getUUID(),
        serverPlayer);
    MenuManager.getMenuHandler()
        .openEditorMenu(EditorType.DIALOG_TEXT, serverPlayer, easyNPC, dialogId, pageIndex);
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public OpenDialogTextEditorMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public UUID getDialogId() {
    return this.dialogId;
  }
}
