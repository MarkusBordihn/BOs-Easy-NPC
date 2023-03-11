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

import net.minecraft.core.Rotations;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.model.ModelPart;
import de.markusbordihn.easynpc.model.ModelPose;

public class MessageRotationChange {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final UUID uuid;
  protected final ModelPart modelPart;
  protected final Rotations rotations;

  public MessageRotationChange(UUID uuid, ModelPart modelPart, float x, float y, float z) {
    this(uuid, modelPart, new Rotations(x, y, z));
  }

  public MessageRotationChange(UUID uuid, ModelPart modelPart, Rotations rotations) {
    this.uuid = uuid;
    this.modelPart = modelPart;
    this.rotations = rotations;
  }

  public ModelPart getModelPart() {
    return this.modelPart;
  }

  public Rotations getRotations() {
    return this.rotations;
  }

  public float getX() {
    return this.rotations.getX();
  }

  public float getY() {
    return this.rotations.getY();
  }

  public float getZ() {
    return this.rotations.getZ();
  }

  public UUID getUUID() {
    return this.uuid;
  }

  public static void handle(MessageRotationChange message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessageRotationChange message, NetworkEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !MessageHelper.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate ModelPart.
    ModelPart modelPart = message.getModelPart();
    if (modelPart == null) {
      log.error("Invalid modelPart {} for {} from {}", modelPart, message, serverPlayer);
      return;
    }

    // Validate Rotations.
    Rotations rotations = message.getRotations();
    if (rotations == null) {
      log.error("Invalid rotation {} for {} from {}", rotations, message, serverPlayer);
      return;
    }

    // Perform action.
    EasyNPCEntity easyNPCEntity = EntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    log.debug("Change {} rotation to {}° for {} from {}", modelPart, rotations, easyNPCEntity,
        serverPlayer);
    switch (modelPart) {
      case HEAD:
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelHeadRotation(rotations);
        break;
      case BODY:
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelBodyRotation(rotations);
        break;
      case LEFT_ARM:
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelLeftArmRotation(rotations);
        break;
      case RIGHT_ARM:
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelRightArmRotation(rotations);
        break;
      case LEFT_LEG:
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelLeftLegRotation(rotations);
        break;
      case RIGHT_LEG:
        easyNPCEntity.setModelPose(ModelPose.CUSTOM);
        easyNPCEntity.setModelRightLegRotation(rotations);
        break;
      default:
        log.error("Invalid modelPart {} for {} from {}", modelPart, message, serverPlayer);
        break;
    }
  }

}
