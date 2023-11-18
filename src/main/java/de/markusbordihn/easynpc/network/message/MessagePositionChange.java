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
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessagePositionChange extends NetworkMessage {

  protected final Vec3 pos;

  public MessagePositionChange(UUID uuid, float x, float y, float z) {
    this(uuid, new Vec3(x, y, z));
  }

  public MessagePositionChange(UUID uuid, Vec3 pos) {
    super(uuid);
    this.pos = pos;
  }

  public static MessagePositionChange decode(final FriendlyByteBuf buffer) {
    return new MessagePositionChange(
        buffer.readUUID(), buffer.readFloat(), buffer.readFloat(), buffer.readFloat());
  }

  public static void encode(final MessagePositionChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeFloat(message.getX());
    buffer.writeFloat(message.getY());
    buffer.writeFloat(message.getZ());
  }

  public static void handle(MessagePositionChange message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessagePositionChange message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate position.
    Vec3 pos = message.getPos();
    if (pos == null) {
      log.error("Invalid pos {} for {} from {}", pos, message, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    log.debug("Change pos {} for {} from {}", pos, easyNPCEntity, serverPlayer);
    easyNPCEntity.setPos(pos);
  }

  public Vec3 getPos() {
    return this.pos;
  }

  public float getX() {
    return (float) this.pos.x;
  }

  public float getY() {
    return (float) this.pos.y;
  }

  public float getZ() {
    return (float) this.pos.z;
  }
}
