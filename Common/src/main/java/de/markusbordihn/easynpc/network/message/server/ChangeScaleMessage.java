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
import de.markusbordihn.easynpc.data.model.ModelScaleAxis;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ChangeScaleMessage(UUID uuid, ModelScaleAxis scaleAxis, Float scaleValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_scale");
  public static final CustomPacketPayload.Type<ChangeScaleMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeScaleMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ChangeScaleMessage::create);

  public static ChangeScaleMessage create(final FriendlyByteBuf buffer) {
    return new ChangeScaleMessage(
        buffer.readUUID(), buffer.readEnum(ModelScaleAxis.class), buffer.readFloat());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.scaleAxis);
    buffer.writeFloat(this.scaleValue);
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

    // Validate scale axis.
    if (this.scaleAxis == null) {
      log.error("Invalid scale axis request for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Validate scale.
    if (this.scaleValue == null || this.scaleValue < 0.1f || this.scaleValue > 10.0f) {
      log.error(
          "Invalid scale {} request for UUID {} from {}", this.scaleValue, easyNPC, serverPlayer);
      return;
    }

    // Validate scale data.
    ScaleData<?> scaleData = easyNPC.getEasyNPCScaleData();
    if (scaleData == null) {
      log.error("Invalid scale data for {} from {}", easyNPC, serverPlayer);
      return;
    }

    // Perform action.
    scaleData.setModelScaleAxis(this.scaleAxis, this.scaleValue);
  }
}
