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
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;

public record ChangeModelPositionMessage(UUID uuid, ModelPart modelPart, CustomPosition position)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "change_model_position");
  public static final CustomPacketPayload.Type<ChangeModelPositionMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeModelPositionMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeModelPositionMessage::create);

  public static ChangeModelPositionMessage create(final FriendlyByteBuf buffer) {
    return new ChangeModelPositionMessage(
        buffer.readUUID(),
        buffer.readEnum(ModelPart.class),
        new CustomPosition(buffer.readFloat(), buffer.readFloat(), buffer.readFloat()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.modelPart);
    buffer.writeFloat(this.position.x());
    buffer.writeFloat(this.position.y());
    buffer.writeFloat(this.position.z());
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
    if (this.modelPart == null) {
      log.error("Invalid modelPart for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate Positions.
    if (this.position == null) {
      log.error("Invalid position for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate Model data.
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("Invalid model data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    log.debug(
        "Change {} position to {}° for {} from {}",
        modelPart,
        this.position,
        easyNPC,
        serverPlayer);

    // Set common properties for all cases except ROOT.
    if (this.modelPart != ModelPart.ROOT) {
      easyNPC.getEntity().setPose(Pose.STANDING);
      modelData.setModelPose(ModelPose.CUSTOM);
    }

    // Apply position change based on the model part.
    modelData.setModelPartPosition(this.modelPart, this.position);

    // Verify if custom model pose is really needed.
    if (!modelData.hasChangedModel()) {
      log.debug("Reset custom model pose for {} from {}", easyNPC, serverPlayer);
      modelData.setModelPose(ModelPose.DEFAULT);
    }
  }
}
