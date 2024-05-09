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
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class MessageOpenDialogButtonEditor extends NetworkMessage {

  protected final UUID dialogId;
  protected final UUID dialogButtonId;
  protected final ConfigurationType formerConfigurationType;
  protected final int pageIndex;

  public MessageOpenDialogButtonEditor(
      UUID uuid, UUID dialogId, ConfigurationType formerConfigurationType) {
    this(uuid, dialogId, null, formerConfigurationType, 0);
  }

  public MessageOpenDialogButtonEditor(
      UUID uuid, UUID dialogId, UUID dialogButtonId, ConfigurationType formerConfigurationType) {
    this(uuid, dialogId, dialogButtonId, formerConfigurationType, 0);
  }

  public MessageOpenDialogButtonEditor(
      UUID uuid,
      UUID dialogId,
      UUID dialogButtonId,
      ConfigurationType formerConfigurationType,
      int pageIndex) {
    super(uuid);
    this.dialogId = dialogId;
    this.dialogButtonId = dialogButtonId;
    this.formerConfigurationType = formerConfigurationType;
    this.pageIndex = pageIndex;
  }

  public static MessageOpenDialogButtonEditor decode(final FriendlyByteBuf buffer) {
    return new MessageOpenDialogButtonEditor(
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readEnum(ConfigurationType.class),
        buffer.readInt());
  }

  public static void encode(
      final MessageOpenDialogButtonEditor message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeUUID(message.getDialogButtonId());
    buffer.writeEnum(message.formerConfigurationType);
    buffer.writeInt(message.pageIndex);
  }

  public static void handle(
      MessageOpenDialogButtonEditor message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageOpenDialogButtonEditor message, NetworkEvent.Context context) {
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

    // Validate dialog data set.
    DialogDataSet dialogDataSet = dialogData.getDialogDataSet();
    if (dialogDataSet == null) {
      log.error("Unable to get valid dialog data set for {} from {}", uuid, serverPlayer);
      return;
    }

    // Validate dialog data.
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog(dialogId);
    if (dialogDataEntry == null) {
      log.error(
          "Unable to get valid dialog data for dialog {} for {} from {}",
          uuid,
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
          uuid,
          serverPlayer);
      dialogDataEntry.setButton(newDialogButton);
      dialogButtonId = newDialogButton.getId();
    } else if (dialogButtonId != null && !dialogData.hasDialogButton(dialogId, dialogButtonId)) {
      log.error("Invalid dialog button id {} for {} from {}", dialogButtonId, message, context);
      return;
    }

    // Perform action.
    ConfigurationType formerConfigurationType = message.getFormerConfigurationType();
    log.info(
        "Open dialog button editor with ref: {} for dialog {} and button {} for {} from {}",
        formerConfigurationType,
        dialogId,
        dialogButtonId,
        uuid,
        serverPlayer);
    MenuManager.getMenuHandler()
        .openDialogButtonEditorMenu(
            serverPlayer, easyNPC, dialogId, dialogButtonId, formerConfigurationType, pageIndex);
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public UUID getDialogButtonId() {
    return this.dialogButtonId;
  }

  public ConfigurationType getFormerConfigurationType() {
    return this.formerConfigurationType;
  }

  public int getPageIndex() {
    return this.pageIndex;
  }
}
