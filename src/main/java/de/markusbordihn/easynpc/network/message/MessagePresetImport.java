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
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessagePresetImport extends NetworkMessage {

  protected final CompoundTag compoundTag;

  public MessagePresetImport(UUID uuid, CompoundTag compoundTag) {
    super(uuid);
    this.compoundTag = compoundTag;
  }

  public static MessagePresetImport decode(final FriendlyByteBuf buffer) {
    return new MessagePresetImport(buffer.readUUID(), buffer.readNbt());
  }

  public static void encode(final MessagePresetImport message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeNbt(message.getCompoundTag());
  }

  public static void handle(MessagePresetImport message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessagePresetImport message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate CompoundTag
    CompoundTag compoundTag = message.getCompoundTag();
    if (compoundTag == null) {
      log.error("Invalid compoundTag {} from {}", compoundTag, serverPlayer);
      return;
    }

    // Validate entity encoded id, if set.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (compoundTag.contains("id")
        && !compoundTag.getString("id").isEmpty()
        && !compoundTag.getString("id").equals(easyNPCEntity.getEncodeId())) {
      log.error(
          "Invalid id {} for {} expected {} from {}",
          compoundTag.getString("id"),
          easyNPCEntity,
          easyNPCEntity.getEncodeId(),
          serverPlayer);
      return;
    }

    // Perform action.
    easyNPCEntity.importPreset(compoundTag);
  }

  public CompoundTag getCompoundTag() {
    return this.compoundTag;
  }
}
