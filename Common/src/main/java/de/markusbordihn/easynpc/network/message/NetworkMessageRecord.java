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
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.message.client.OpenMenuCallbackMessage;
import io.netty.buffer.Unpooled;
import java.util.Random;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface NetworkMessageRecord extends CustomPacketPayload {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  UUID EMPTY_UUID = new UUID(0L, 0L);

  Random RANDOM = new Random();

  static boolean checkAccess(final UUID uuid, final ServerPlayer serverPlayer) {
    // Validate UUID.
    if (uuid == null || uuid.equals(EMPTY_UUID)) {
      log.error("Unable to get valid entity UUID {} for {}", uuid, serverPlayer);
      return false;
    }

    // Validate player.
    if (serverPlayer == null) {
      log.error("Unable to get valid player for entity with UUID {}", uuid);
      return false;
    }

    // Validate entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return false;
    }

    // Validate access.
    if (!LivingEntityManager.hasAccess(uuid, serverPlayer)) {
      log.error("User {} has no access to Easy NPC with uuid {}.", serverPlayer, uuid);
      return false;
    }

    return true;
  }

  ResourceLocation id();

  void write(FriendlyByteBuf friendlyByteBuf);

  default FriendlyByteBuf payload() {
    FriendlyByteBuf friendlyByteBuf = new FriendlyByteBuf(Unpooled.buffer());
    write(friendlyByteBuf);
    return friendlyByteBuf;
  }

  default void handleClient() {
    log.error("Network message client handler not implemented for {}", this);
  }

  default void handleServer(ServerPlayer serverPlayer) {
    log.error("Network message server handler not implemented for {}", this);
  }

  default EasyNPC<?> getEasyNPC(final UUID uuid, final ServerPlayer serverPlayer) {
    // Validate UUID.
    if (uuid == null || uuid.equals(EMPTY_UUID)) {
      log.error("Invalid Easy NPC UUID {} from {}", uuid, serverPlayer);
      return null;
    }

    // Validate player.
    if (serverPlayer == null) {
      log.error("Invalid server player for Easy NPC with UUID {}", uuid);
      return null;
    }
    return LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
  }

  default EasyNPC<?> getEasyNPCAndCheckAccess(final UUID uuid, final ServerPlayer serverPlayer) {
    return checkAccess(uuid, serverPlayer) ? getEasyNPC(uuid, serverPlayer) : null;
  }
}
