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
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class RemoveDialogMessage extends NetworkMessage<RemoveDialogMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "remove_dialog");

  protected final UUID dialogId;

  public RemoveDialogMessage(final UUID uuid, final UUID dialogId) {
    super(uuid);
    this.dialogId = dialogId;
  }

  public static RemoveDialogMessage decode(final FriendlyByteBuf buffer) {
    return new RemoveDialogMessage(buffer.readUUID(), buffer.readUUID());
  }

  public static FriendlyByteBuf encode(
      final RemoveDialogMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final RemoveDialogMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate dialog ID
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.error("Invalid dialog id for {} from {}", message, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPC<?> easyNPC =
        LivingEntityManager.getEasyNPCEntityByUUID(message.getUUID(), serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", message.getUUID(), serverPlayer);
      return;
    }

    // Validate dialog data
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Invalid dialog data for {} from {}", message, serverPlayer);
      return;
    }

    // Validate dialog
    if (!dialogData.hasDialog(dialogId)) {
      log.error(
          "Unknown delete dialog request for dialog {} for {} from {}",
          dialogId,
          message.getUUID(),
          serverPlayer);
      return;
    }

    // Perform action.
    if (dialogData.removeDialog(dialogId)) {
      log.info("Removed dialog {} for {} from {}", dialogId, easyNPC, serverPlayer);
    } else {
      log.warn("Unable to remove dialog {} for {} from {}", dialogId, easyNPC, serverPlayer);
    }
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public RemoveDialogMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public UUID getDialogId() {
    return this.dialogId;
  }
}
