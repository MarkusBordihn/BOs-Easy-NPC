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

package de.markusbordihn.easynpc.configui.network.message.server;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;

public record ChangeModelRotationMessage(
    UUID uuid, ModelPartType modelPartType, CustomRotation rotation)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "change_model_rotation");
  public static final Type<ChangeModelRotationMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeModelRotationMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeModelRotationMessage::create);

  public static ChangeModelRotationMessage create(final FriendlyByteBuf buffer) {
    return new ChangeModelRotationMessage(
        buffer.readUUID(),
        buffer.readEnum(ModelPartType.class),
        new CustomRotation(
            buffer.readFloat(), buffer.readFloat(), buffer.readFloat(), buffer.readBoolean()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.modelPartType);
    buffer.writeFloat(this.rotation.x());
    buffer.writeFloat(this.rotation.y());
    buffer.writeFloat(this.rotation.z());
    buffer.writeBoolean(this.rotation.locked());
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    // Validate ModelPart.
    if (this.modelPartType == null) {
      log.error("Invalid modelPartType for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate Rotations.
    if (this.rotation == null) {
      log.error("Invalid rotation for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate Model data.
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("Invalid model data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    if (this.modelPartType == ModelPartType.ROOT) {
      modelData.setModelRotation(this.rotation.x(), this.rotation.y(), this.rotation.z());
    } else {
      log.debug(
          "Change {} rotation to {}° {}° {}° for {} from {}",
          this.modelPartType,
          this.rotation.x(),
          this.rotation.y(),
          this.rotation.z(),
          easyNPC,
          serverPlayer);

      // Use custom model pose for model part rotation.
      modelData.setModelPartRotation(this.modelPartType, this.rotation);
      easyNPC.getEntity().setPose(Pose.STANDING);
      modelData.setModelPose(ModelPose.CUSTOM);

      // Verify if custom model pose is really needed.
      if (!modelData.hasChangedModel()
          || (this.modelPartType == ModelPartType.ROOT && this.rotation.hasChanged())) {
        log.debug("Reset custom model pose for {} from {}", easyNPC, serverPlayer);
        modelData.setModelPose(ModelPose.DEFAULT);
        easyNPC.getEntity().setPose(Pose.STANDING);
      }
    }
  }
}
