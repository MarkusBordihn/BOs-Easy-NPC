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

import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkEvent.Context;

public class MessageRespawnNPC extends NetworkMessage {

  public MessageRespawnNPC(UUID uuid) {
    super(uuid);
  }

  public static MessageRespawnNPC decode(final FriendlyByteBuf buffer) {
    return new MessageRespawnNPC(buffer.readUUID());
  }

  public static void encode(final MessageRespawnNPC message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
  }

  public static void handle(MessageRespawnNPC message, Supplier<Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageRespawnNPC message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Verify that the entity is not null
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Save entity and entity type
    CompoundTag compoundTag = easyNPC.getEasyNPCEntity().saveWithoutId(new CompoundTag());
    EntityType<?> entityType = easyNPC.getEntity().getType();

    // Create new entity with compoundTag
    ServerLevel serverLevel = serverPlayer.getLevel();
    Entity entity = entityType.create(serverLevel);
    if (entity == null) {
      log.error("Unable to create new entity with type {} for {}", entityType, serverPlayer);
      return;
    }
    entity.load(compoundTag);

    // Remove old entity
    easyNPC.getEntity().discard();

    // Respawn new entity
    log.info("Respawn Easy NPC {} with {} requested by {}", easyNPC, entityType, serverPlayer);
    serverLevel.addFreshEntity(entity);
  }
}
