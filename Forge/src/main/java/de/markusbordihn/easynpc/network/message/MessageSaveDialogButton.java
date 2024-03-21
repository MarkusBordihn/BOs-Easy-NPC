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

import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageSaveDialogButton extends NetworkMessage {

  protected final UUID dialogId;
  protected final UUID dialogButtonId;
  protected final DialogButtonData dialogButtonData;

  public MessageSaveDialogButton(
      UUID uuid, UUID dialogId, UUID dialogButtonId, DialogButtonData dialogButtonData) {
    super(uuid);
    this.dialogId = dialogId;
    this.dialogButtonId = dialogButtonId;
    this.dialogButtonData = dialogButtonData;
  }

  public static MessageSaveDialogButton decode(final FriendlyByteBuf buffer) {
    return new MessageSaveDialogButton(
        buffer.readUUID(),
        buffer.readUUID(),
        buffer.readUUID(),
        new DialogButtonData(buffer.readNbt()));
  }

  public static void encode(final MessageSaveDialogButton message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeUUID(message.getDialogButtonId());
    buffer.writeNbt(message.getDialogButtonData().createTag());
  }

  public static void handle(MessageSaveDialogButton message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageSaveDialogButton message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate dialog id.
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, context);
      return;
    }

    // Validate dialog button data.
    DialogButtonData dialogButtonData = message.getDialogButtonData();
    if (dialogButtonData == null) {
      log.error("Invalid dialog button data for {} from {}", uuid, serverPlayer);
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
      log.error("Invalid dialog data for {} from {}", message, context);
      return;
    }

    // Validate action event data.
    ActionEventData<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    if (actionEventData == null) {
      log.error("Invalid action data for {} from {}", message, context);
      return;
    }

    // Validate dialog for dialog button.
    if (!dialogData.hasDialog(dialogId)) {
      log.error(
          "Unknown dialog button editor request for dialog {} for {} from {}",
          dialogId,
          uuid,
          serverPlayer);
      return;
    }

    // Validate dialog button id.
    UUID dialogButtonId = message.getDialogButtonId();
    if (dialogButtonId != null && !dialogData.hasDialogButton(dialogId, dialogButtonId)) {
      log.error("Invalid dialog button {} for {} from {}", dialogButtonId, message, context);
      return;
    }

    // Re-check permission levels for dialog related actions.
    int currentPermissionLevel = actionEventData.getActionPermissionLevel();
    if (currentPermissionLevel == 0) {
      MinecraftServer minecraftServer = serverPlayer.getServer();
      if (minecraftServer != null) {
        int permissionLevel = minecraftServer.getProfilePermissions(serverPlayer.getGameProfile());
        if (permissionLevel > currentPermissionLevel) {
          log.debug(
              "Update owner permission level from {} to {} for {} from {}",
              currentPermissionLevel,
              permissionLevel,
              easyNPC,
              serverPlayer);
          actionEventData.setActionPermissionLevel(permissionLevel);
        }
      }
    }

    // Perform action.
    if (dialogButtonId == null) {
      log.info(
          "Add new dialog button {} for dialog {} for {} from {}",
          dialogButtonData,
          dialogId,
          uuid,
          serverPlayer);
      dialogData.getDialogDataSet().getDialog(dialogId).setButton(dialogButtonData);
    } else {
      log.info(
          "Edit existing dialog button {} for dialog {} for {} from {}",
          dialogButtonData,
          dialogId,
          uuid,
          serverPlayer);
      dialogData.getDialogDataSet().getDialog(dialogId).setButton(dialogButtonId, dialogButtonData);
    }
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public UUID getDialogButtonId() {
    return this.dialogButtonId;
  }

  public DialogButtonData getDialogButtonData() {
    return this.dialogButtonData;
  }
}
