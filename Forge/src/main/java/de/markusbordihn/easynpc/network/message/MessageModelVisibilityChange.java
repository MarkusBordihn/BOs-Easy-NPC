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

import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.util.UUID;
import java.util.function.Supplier;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;
import net.minecraftforge.network.NetworkEvent;

public class MessageModelVisibilityChange extends NetworkMessage {

  protected final ModelPart modelPart;
  protected final boolean visible;

  public MessageModelVisibilityChange(UUID uuid, ModelPart modelPart, boolean visible) {
    super(uuid);
    this.modelPart = modelPart;
    this.visible = visible;
  }

  public static MessageModelVisibilityChange decode(final FriendlyByteBuf buffer) {
    return new MessageModelVisibilityChange(
        buffer.readUUID(), buffer.readEnum(ModelPart.class), buffer.readBoolean());
  }

  public static void encode(
      final MessageModelVisibilityChange message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getModelPart());
    buffer.writeBoolean(message.isVisible());
  }

  public static void handle(
      MessageModelVisibilityChange message, Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessageModelVisibilityChange message, NetworkEvent.Context context) {
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

    // Validate Model data.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("Invalid model data for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Visibility.
    boolean visible = message.isVisible();

    // Perform action.
    log.debug(
        "Change {} visibility to {} for {} from {}", modelPart, visible, easyNPC, serverPlayer);
    switch (modelPart) {
      case HEAD:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelHeadVisible(visible);
        break;
      case BODY:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelBodyVisible(visible);
        break;
      case ARMS:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelArmsVisible(visible);
        break;
      case LEFT_ARM:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelLeftArmVisible(visible);
        break;
      case RIGHT_ARM:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelRightArmVisible(visible);
        break;
      case LEFT_LEG:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelLeftLegVisible(visible);
        break;
      case RIGHT_LEG:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelRightLegVisible(visible);
        break;
      case ROOT:
        break;
      default:
        log.error("Invalid modelPart {} for {} from {}", modelPart, message, serverPlayer);
        break;
    }

    // Verify if custom model pose is really needed.
    if (modelData.getModelPose() == ModelPose.CUSTOM && !modelData.hasChangedModel()) {
      log.debug("Reset custom model pose for {} from {}", easyNPC, serverPlayer);
      modelData.setModelPose(ModelPose.DEFAULT);
      easyNPC.getEntity().setPose(Pose.STANDING);
    }
  }

  public ModelPart getModelPart() {
    return this.modelPart;
  }

  public boolean isVisible() {
    return this.visible;
  }
}
