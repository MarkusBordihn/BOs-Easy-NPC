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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ChangeScaleMessage extends NetworkMessage<ChangeScaleMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_scale");

  protected final Float scale;
  protected final String scaleAxis;

  public ChangeScaleMessage(final UUID uuid, final String scaleAxis, final Float scale) {
    super(uuid);
    this.scale = scale;
    this.scaleAxis = scaleAxis;
  }

  public static ChangeScaleMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeScaleMessage(buffer.readUUID(), buffer.readUtf(), buffer.readFloat());
  }

  public static FriendlyByteBuf encode(
      final ChangeScaleMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getScaleAxis());
    buffer.writeFloat(message.getScale());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final ChangeScaleMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate scale axis.
    String scaleAxis = message.getScaleAxis();
    if (scaleAxis == null
        || !scaleAxis.equals("x") && !scaleAxis.equals("y") && !scaleAxis.equals("z")) {
      log.error(
          "Unknown scale axis {} request for UUID {} from {}",
          scaleAxis,
          message.getUUID(),
          serverPlayer);
      return;
    }

    // Validate scale.
    Float scale = message.getScale();
    if (scale == null || scale < 0.1f || scale > 10.0f) {
      log.error(
          "Invalid scale {} request for UUID {} from {}", scale, message.getUUID(), serverPlayer);
      return;
    }

    // Validate scale data.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    ScaleData<?> scaleData = easyNPC.getEasyNPCScaleData();
    if (scaleData == null) {
      log.error("Invalid scale data for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    switch (scaleAxis) {
      case "x":
        scaleData.setScaleX(scale);
        break;
      case "y":
        scaleData.setScaleY(scale);
        break;
      case "z":
        scaleData.setScaleZ(scale);
        break;
    }
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ChangeScaleMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public Float getScale() {
    return this.scale;
  }

  public String getScaleAxis() {
    return this.scaleAxis;
  }
}
