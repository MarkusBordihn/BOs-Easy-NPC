/**
 * Copyright 2022 Markus Bordihn
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

import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;

import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

public class MessageNameChange {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final String uuid;
  protected final String name;

  public MessageNameChange(String uuid, String name) {
    this.uuid = uuid;
    this.name = name;
  }

  public String getName() {
    return this.name;
  }

  public String getUUID() {
    return this.uuid;
  }

  public static void handle(MessageNameChange message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageNameChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    if (serverPlayer == null) {
      log.error("Unable to get server player for message {} from {}", message, context);
      return;
    }

    // Validate name.
    String name = message.getName();
    if (name == null || name.isEmpty()) {
      log.error("Invalid name {} for {} from {}", name, message, serverPlayer);
    }

    ServerLevel serverLevel = serverPlayer.getLevel();
    UUID uuid = UUID.fromString(message.getUUID());
    Entity entity = serverLevel.getEntity(uuid);

    // Only accepts commands from owner, log attempts.
    if (entity instanceof EasyNPCEntity easyNPCEntity) {
      log.debug("Change name {} for {} from {}", name, easyNPCEntity, serverPlayer);
      easyNPCEntity.setCustomName(new TextComponent(name));
    }
  }

}
