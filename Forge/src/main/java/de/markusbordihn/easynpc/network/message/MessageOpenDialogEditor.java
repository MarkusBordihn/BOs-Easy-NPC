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

import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageOpenDialogEditor extends NetworkMessage {

  protected final UUID dialogId;
  protected final ConfigurationType formerConfigurationType;
  protected final int pageIndex;

  public MessageOpenDialogEditor(
      UUID uuid, UUID dialogId, ConfigurationType formerConfigurationType) {
    this(uuid, dialogId, formerConfigurationType, 0);
  }

  public MessageOpenDialogEditor(
      UUID uuid, UUID dialogId, ConfigurationType formerConfigurationType, int pageIndex) {
    super(uuid);
    this.dialogId = dialogId;
    this.pageIndex = pageIndex;
    this.formerConfigurationType = formerConfigurationType;
  }

  public static MessageOpenDialogEditor decode(final FriendlyByteBuf buffer) {
    return new MessageOpenDialogEditor(
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readEnum(ConfigurationType.class),
        buffer.readInt());
  }

  public static void encode(final MessageOpenDialogEditor message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.dialogId);
    buffer.writeEnum(message.formerConfigurationType);
    buffer.writeInt(message.pageIndex);
  }

  public static void handle(MessageOpenDialogEditor message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageOpenDialogEditor message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate dialog id.
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id {} for {} from {}", dialogId, message, context);
      return;
    }

    // Validate page index.
    int pageIndex = message.getPageIndex();
    if (pageIndex < 0) {
      log.error("Invalid page index {} for {} from {}", pageIndex, message, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Unable to get valid dialog data for {} from {}", uuid, serverPlayer);
      return;
    }

    // Validate dialog id and create new dialog if needed.
    if (dialogId.equals(EMPTY_UUID)) {
      String dialogName =
          dialogData.getDialogDataSet().hasDialog() ? "Dialog " + RANDOM.nextInt(1000) : "Default";
      DialogDataEntry newDialogData = new DialogDataEntry(dialogName);
      log.info("Create new dialog {} for {} from {}", newDialogData, uuid, serverPlayer);
      dialogData.getDialogDataSet().addDialog(newDialogData);
      dialogId = newDialogData.getId();
    } else if (!dialogData.hasDialog(dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          dialogId,
          uuid,
          serverPlayer);
      log.debug("Available dialogs for {} are {}", uuid, dialogData.getDialogDataSet());
      return;
    }

    // Perform action.
    ConfigurationType formerConfigurationType = message.getFormerConfigurationType();
    log.info(
        "Open dialog editor with ref: {} for dialog {} for {} from {}",
        formerConfigurationType,
        dialogId,
        uuid,
        serverPlayer);
    MenuManager.getMenuHandler()
        .openDialogEditorMenu(serverPlayer, easyNPC, dialogId, formerConfigurationType, pageIndex);
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public int getPageIndex() {
    return this.pageIndex;
  }

  public ConfigurationType getFormerConfigurationType() {
    return this.formerConfigurationType;
  }
}
