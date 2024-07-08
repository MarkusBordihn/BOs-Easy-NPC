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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;

public class ChangeModelVisibilityMessage extends NetworkMessage<ChangeModelVisibilityMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_model_visibility");

  protected final ModelPart modelPart;
  protected final boolean visible;

  public ChangeModelVisibilityMessage(
      final UUID uuid, final ModelPart modelPart, final boolean visible) {
    super(uuid);
    this.modelPart = modelPart;
    this.visible = visible;
  }

  public static ChangeModelVisibilityMessage decode(final FriendlyByteBuf buffer) {
    return new ChangeModelVisibilityMessage(
        buffer.readUUID(), buffer.readEnum(ModelPart.class), buffer.readBoolean());
  }

  public static FriendlyByteBuf encode(
      final ChangeModelVisibilityMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.getModelPart());
    buffer.writeBoolean(message.isVisible());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(
      final ChangeModelVisibilityMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate ModelPart.
    ModelPart modelPart = message.getModelPart();
    if (modelPart == null) {
      log.error("Invalid modelPart for {} from {}", message, serverPlayer);
      return;
    }

    // Validate Model data.
    EasyNPC<?> easyNPC = message.getEasyNPC();
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

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ChangeModelVisibilityMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public ModelPart getModelPart() {
    return this.modelPart;
  }

  public boolean isVisible() {
    return this.visible;
  }
}
