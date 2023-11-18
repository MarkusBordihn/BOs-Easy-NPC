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
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessageScaleChange extends NetworkMessage {

  protected final Float scale;
  protected final String scaleAxis;

  public MessageScaleChange(UUID uuid, String scaleAxis, Float scale) {
    super(uuid);
    this.scale = scale;
    this.scaleAxis = scaleAxis;
  }

  public static MessageScaleChange decode(FriendlyByteBuf buffer) {
    return new MessageScaleChange(buffer.readUUID(), buffer.readUtf(), buffer.readFloat());
  }

  public static void encode(MessageScaleChange message, FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getScaleAxis());
    buffer.writeFloat(message.getScale());
  }

  public static void handle(MessageScaleChange message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageScaleChange message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate scale axis.
    String scaleAxis = message.getScaleAxis();
    if (scaleAxis == null
        || !scaleAxis.equals("x") && !scaleAxis.equals("y") && !scaleAxis.equals("z")) {
      log.error("Unknown scale axis {} request for UUID {} from {}", scaleAxis, uuid, serverPlayer);
      return;
    }

    // Validate scale.
    Float scale = message.getScale();
    if (scale == null || scale < 0.1f || scale > 10.0f) {
      log.error("Invalid scale {} request for UUID {} from {}", scale, uuid, serverPlayer);
      return;
    }

    // Validate entity.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPCEntity == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Perform action.
    switch (scaleAxis) {
      case "x":
        easyNPCEntity.setScaleX(scale);
        break;
      case "y":
        easyNPCEntity.setScaleY(scale);
        break;
      case "z":
        easyNPCEntity.setScaleZ(scale);
        break;
      default:
        log.error(
            "Unknown scale axis {} request for UUID {} from {}", scaleAxis, uuid, serverPlayer);
    }
  }

  public Float getScale() {
    return this.scale;
  }

  public String getScaleAxis() {
    return this.scaleAxis;
  }
}
