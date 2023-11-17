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

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EasyNPCEntityMenu;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

public class MessageOpenDialog extends NetworkMessage {

  protected final int pageIndex;
  protected final UUID dialogId;

  public MessageOpenDialog(UUID uuid, UUID dialogId, int pageIndex) {
    super(uuid);
    this.dialogId = dialogId;
    this.pageIndex = pageIndex;
  }

  public static MessageOpenDialog decode(final FriendlyByteBuf buffer) {
    return new MessageOpenDialog(buffer.readUUID(), buffer.readUUID(), buffer.readInt());
  }

  public static void encode(final MessageOpenDialog message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUUID(message.getDialogId());
    buffer.writeInt(message.getPageIndex());
  }

  public static void handle(
      MessageOpenDialog message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageOpenDialog message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || uuid == null) {
      return;
    }

    // Validate dialog ID
    UUID dialogId = message.getDialogId();
    if (dialogId == null) {
      log.warn("Missing dialog ID for {}", uuid);
      return;
    }

    // Validate page index
    int pageIndex = message.getPageIndex();
    if (pageIndex < 0) {
      log.error("Invalid page index {} for {} from {}", pageIndex, uuid, serverPlayer);
      return;
    }

    // Validate Easy NPC entity
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity == null) {
      log.error("Unable to find Easy NPC entity for {} from {}", uuid, serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Open dialog {} with page index {} for {} from {} ",
        dialogId,
        pageIndex,
        easyNPCEntity,
        serverPlayer);
    EasyNPCEntityMenu.openDialogMenu(serverPlayer, easyNPCEntity, dialogId, pageIndex);
  }

  public UUID getDialogId() {
    return this.dialogId;
  }

  public int getPageIndex() {
    return this.pageIndex;
  }
}
