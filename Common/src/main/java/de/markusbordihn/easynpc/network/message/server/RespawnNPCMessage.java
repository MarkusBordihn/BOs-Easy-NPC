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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

public class RespawnNPCMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "respawn_npc");

  public RespawnNPCMessage(final UUID uuid) {
    super(uuid);
  }

  public static RespawnNPCMessage decode(final FriendlyByteBuf buffer) {
    return new RespawnNPCMessage(buffer.readUUID());
  }

  public static FriendlyByteBuf encode(
      final RespawnNPCMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final RespawnNPCMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Save entity and entity type
    EasyNPC<?> easyNPC = message.getEasyNPC();
    CompoundTag compoundTag = easyNPC.getEntity().saveWithoutId(new CompoundTag());
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

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }
}
