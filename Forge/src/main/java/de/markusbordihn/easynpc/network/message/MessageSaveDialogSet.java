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
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MessageSaveDialogSet extends NetworkMessage {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final DialogDataSet dialogDataSet;

  public MessageSaveDialogSet(UUID uuid, DialogDataSet dialogDataSet) {
    super(uuid);
    this.dialogDataSet = dialogDataSet;
  }

  public static MessageSaveDialogSet decode(final FriendlyByteBuf buffer) {
    return new MessageSaveDialogSet(buffer.readUUID(), new DialogDataSet(buffer.readNbt()));
  }

  public static void encode(final MessageSaveDialogSet message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeNbt(message.getDialogData().createTag());
  }

  public static void handle(MessageSaveDialogSet message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageSaveDialogSet message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    DialogDataSet dialogDataSet = message.getDialogData();
    if (serverPlayer == null
        || dialogDataSet == null
        || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      log.error("Unable to save dialog with message {} from {}", message, context);
      return;
    }

    // Validate Dialog data.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    DialogData<?> dialogData = easyNPC.getEasyNPCDialogData();
    if (dialogData == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Perform action.
    log.debug("Saving dialog {} for {} from {}", dialogDataSet, easyNPC, serverPlayer);
    dialogData.setDialogDataSet(dialogDataSet);
  }

  public DialogDataSet getDialogData() {
    return this.dialogDataSet;
  }
}
