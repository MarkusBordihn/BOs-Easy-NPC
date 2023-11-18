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

import de.markusbordihn.easynpc.data.CustomPosition;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;
import net.minecraftforge.network.NetworkEvent;

public class MessageModelPositionChange extends NetworkMessage {

  protected final ModelPart modelPart;
  protected final CustomPosition position;

  public MessageModelPositionChange(UUID uuid, ModelPart modelPart, float x, float y, float z) {
    this(uuid, modelPart, new CustomPosition(x, y, z));
  }

  public MessageModelPositionChange(UUID uuid, ModelPart modelPart, CustomPosition position) {
    super(uuid);
    this.modelPart = modelPart;
    this.position = position;
  }

  public static MessageModelPositionChange decode(final FriendlyByteBuf buffer) {
    return new MessageModelPositionChange(
        buffer.readUUID(),
        buffer.readEnum(ModelPart.class),
        buffer.readFloat(),
        buffer.readFloat(),
        buffer.readFloat());
  }

  public static void encode(
      final MessageModelPositionChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getModelPart());
    buffer.writeFloat(message.getX());
    buffer.writeFloat(message.getY());
    buffer.writeFloat(message.getZ());
  }

  public static void handle(
      MessageModelPositionChange message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageModelPositionChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate ModelPart.
    ModelPart modelPart = message.getModelPart();
    if (modelPart == null) {
      log.error("Invalid modelPart {} for {} from {}", modelPart, message, serverPlayer);
      return;
    }

    // Validate Positions.
    CustomPosition position = message.getPosition();
    if (position == null) {
      log.error("Invalid position {} for {} from {}", position, message, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    log.debug(
        "Change {} position to {}° for {} from {}",
        modelPart,
        position,
        easyNPCEntity,
        serverPlayer);
    switch (modelPart) {
      case HEAD:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelHeadPosition(position);
        break;
      case BODY:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelBodyPosition(position);
        break;
      case ARMS:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelArmsPosition(position);
        break;
      case LEFT_ARM:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelLeftArmPosition(position);
        break;
      case RIGHT_ARM:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelRightArmPosition(position);
        break;
      case LEFT_LEG:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelLeftLegPosition(position);
        break;
      case RIGHT_LEG:
        easyNPCEntity.setPose(Pose.STANDING);
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelRightLegPosition(position);
        break;
      case ROOT:
        break;
      default:
        log.error("Invalid modelPart {} for {} from {}", modelPart, message, serverPlayer);
        break;
    }
  }

  public ModelPart getModelPart() {
    return this.modelPart;
  }

  public CustomPosition getPosition() {
    return this.position;
  }

  public float getX() {
    return this.position.x();
  }

  public float getY() {
    return this.position.y();
  }

  public float getZ() {
    return this.position.z();
  }
}
