/**
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

import java.util.UUID;
import java.util.function.Supplier;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;

public class MessageDialogTypeChange {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final UUID uuid;
  protected final DialogType dialogType;

  public MessageDialogTypeChange(UUID uuid, DialogType dialogType) {
    this.uuid = uuid;
    this.dialogType = dialogType;
  }

  public DialogType getDialogType() {
    return this.dialogType;
  }

  public UUID getUUID() {
    return this.uuid;
  }

  public static void handle(
      MessageDialogTypeChange message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageDialogTypeChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    DialogType dialogType = message.getDialogType();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || dialogType == null || !MessageHelper.checkAccess(uuid, serverPlayer)) {
      log.error("Unable to save basic dialog with message {} from {}", message, context);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    log.debug("Change dialog type: {} for {} from {}", dialogType, easyNPCEntity, serverPlayer);
    easyNPCEntity.setDialogType(dialogType);
  }

}
