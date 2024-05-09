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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.network.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.Set;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class DialogButtonActionMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "dialog_button_action");
  protected final UUID dialogId;
  protected final UUID dialogButtonId;

  public DialogButtonActionMessage(UUID uuid, UUID dialogId, UUID dialogButtonId) {
    super(uuid);
    this.dialogId = dialogId;
    this.dialogButtonId = dialogButtonId;
  }

  public static DialogButtonActionMessage decode(final FriendlyByteBuf buffer) {
    return new DialogButtonActionMessage(buffer.readUUID(), buffer.readUUID(), buffer.readUUID());
  }

  public static FriendlyByteBuf encode(
      final DialogButtonActionMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeUUID(message.getDialogButtonId());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(DialogButtonActionMessage message, ServerPlayer serverPlayer) {
    UUID uuid = message.getUUID();
    if (serverPlayer == null || uuid == null) {
      log.error(
          "Unable to trigger dialog button action event with message {} from {}",
          message,
          serverPlayer);
      return;
    }

    // Validate dialog id.
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, serverPlayer);
      return;
    }

    // Validate dialog button id.
    UUID dialogButtonId = message.getDialogButtonId();
    if (dialogButtonId == null) {
      log.error("Invalid dialog button id for {} from {}", message, serverPlayer);
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
      log.error("Dialog data for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    // Validate dialog button actions.
    if (!dialogData.hasDialogButton(dialogId, dialogButtonId)) {
      log.error(
          "Unknown dialog button action {} request for dialog {} for UUID {} from {}",
          dialogButtonId,
          dialogId,
          uuid,
          serverPlayer);
      return;
    }

    // Validate dialog button data.
    DialogButtonData dialogButtonData = dialogData.getDialogButton(dialogId, dialogButtonId);
    if (dialogButtonData == null) {
      log.error(
          "Unable to get valid dialog button data for UUID {} and dialog {} from {}",
          uuid,
          dialogId,
          serverPlayer);
      return;
    }

    // Validate dialog button actions.
    Set<ActionDataEntry> actionDataEntryList = dialogButtonData.getActionData();
    if (actionDataEntryList == null || actionDataEntryList.isEmpty()) {
      log.error(
          "Empty dialog button action {} request for UUID {} and dialog {} from {}",
          dialogButtonId,
          uuid,
          dialogId,
          serverPlayer);
      return;
    }

    // Perform action.
    ActionHandler<?> actionHandler = easyNPC.getEasyNPCActionHandler();
    if (actionHandler == null) {
      log.error("Action handler for {} is not available for {}", easyNPC, serverPlayer);
      return;
    }

    if (actionDataEntryList.size() == 1) {
      log.debug(
          "Trigger single dialog button action for {} from {} with action: {}",
          easyNPC,
          serverPlayer,
          actionDataEntryList.iterator().next());
      actionHandler.executeAction(actionDataEntryList.iterator().next(), serverPlayer);
    } else {
      log.debug(
          "Trigger multiple dialog button actions for {} from {} with {} actions: {}",
          easyNPC,
          serverPlayer,
          actionDataEntryList.size(),
          actionDataEntryList);
      actionHandler.executeActions(actionDataEntryList, serverPlayer);
    }
  }

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
