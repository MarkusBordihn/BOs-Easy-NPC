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
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;

public class ChangeModelRotationMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_model_rotation");

  protected final ModelPart modelPart;
  protected final CustomRotation rotations;

  public ChangeModelRotationMessage(
      final UUID uuid, final ModelPart modelPart, final float x, final float y, final float z) {
    this(uuid, modelPart, new CustomRotation(x, y, z));
  }

  public ChangeModelRotationMessage(
      final UUID uuid, final ModelPart modelPart, final CustomRotation rotations) {
    super(uuid);
    this.modelPart = modelPart;
    this.rotations = rotations;
  }

  public static ChangeModelRotationMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeModelRotationMessage(
        buffer.readUUID(),
        buffer.readEnum(ModelPart.class),
        buffer.readFloat(),
        buffer.readFloat(),
        buffer.readFloat());
  }

  public static FriendlyByteBuf encode(
      final ChangeModelRotationMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getModelPart());
    buffer.writeFloat(message.getX());
    buffer.writeFloat(message.getY());
    buffer.writeFloat(message.getZ());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeModelRotationMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate ModelPart.
    ModelPart modelPart = message.getModelPart();
    if (modelPart == null) {
      log.error("Invalid modelPart for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Rotations.
    CustomRotation rotations = message.getRotations();
    if (rotations == null) {
      log.error("Invalid rotation for {} from {}", message, serverPlayer);
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
    log.debug(
        "Change {} rotation to {}° {}° {}° for {} from {}",
        modelPart,
        rotations.getX(),
        rotations.getY(),
        rotations.getZ(),
        easyNPC,
        serverPlayer);
    switch (modelPart) {
      case HEAD:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelHeadRotation(rotations);
        break;
      case BODY:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelBodyRotation(rotations);
        break;
      case ARMS:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelArmsRotation(rotations);
        break;
      case LEFT_ARM:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelLeftArmRotation(rotations);
        break;
      case RIGHT_ARM:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelRightArmRotation(rotations);
        break;
      case LEFT_LEG:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelLeftLegRotation(rotations);
        break;
      case RIGHT_LEG:
        easyNPC.getEntity().setPose(Pose.STANDING);
        modelData.setModelPose(ModelPose.CUSTOM);
        modelData.setModelRightLegRotation(rotations);
        break;
      case ROOT:
        modelData.setModelRootRotation(rotations);
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

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public ModelPart getModelPart() {
    return this.modelPart;
  }

  public CustomRotation getRotations() {
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
}
