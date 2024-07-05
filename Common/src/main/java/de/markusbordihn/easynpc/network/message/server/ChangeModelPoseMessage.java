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
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;

public class ChangeModelPoseMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_model_pose");

  protected final ModelPose modelPose;

  public ChangeModelPoseMessage(final UUID uuid, final ModelPose modelPose) {
    super(uuid);
    this.modelPose = modelPose;
  }

  public static ChangeModelPoseMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeModelPoseMessage(buffer.readUUID(), buffer.readEnum(ModelPose.class));
  }

  public static FriendlyByteBuf encode(
      final ChangeModelPoseMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getModelPose());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final ChangeModelPoseMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate name.
    ModelPose modelPose = message.getModelPose();
    if (modelPose == null) {
      log.error("Invalid modelPose for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Model data.
    EasyNPC<?> easyNPC = message.getEasyNPC();
    ModelData<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData == null) {
      log.error("Invalid model data for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    log.debug("Change modelPose {} for {} from {}", modelPose, easyNPC, serverPlayer);
    modelData.setModelPose(modelPose);
    easyNPC.getEntity().setPose(Pose.STANDING);
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public ModelPose getModelPose() {
    return this.modelPose;
  }
}
