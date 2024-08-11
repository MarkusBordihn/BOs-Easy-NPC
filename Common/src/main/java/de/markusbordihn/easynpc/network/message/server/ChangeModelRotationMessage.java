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
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Pose;

public record ChangeModelRotationMessage(UUID uuid, ModelPart modelPart, CustomRotation rotation)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_model_rotation");

  public static ChangeModelRotationMessage create(final FriendlyByteBuf buffer) {
    return new ChangeModelRotationMessage(
        buffer.readUUID(),
        buffer.readEnum(ModelPart.class),
        new CustomRotation(buffer.readFloat(), buffer.readFloat(), buffer.readFloat()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.modelPart);
    buffer.writeFloat(this.rotation.x());
    buffer.writeFloat(this.rotation.y());
    buffer.writeFloat(this.rotation.z());
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
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

    // Validate Rotations.
    if (this.rotation == null) {
      log.error("Invalid rotation for {} from {}", easyNPC, serverPlayer);
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
        "Change {} rotation to {}° {}° {}° for {} from {}",
        this.modelPart,
        this.rotation.x(),
        this.rotation.y(),
        this.rotation.z(),
        easyNPC,
        serverPlayer);

    // Set common properties for all cases except ROOT.
    if (this.modelPart != ModelPart.ROOT) {
      easyNPC.getEntity().setPose(Pose.STANDING);
      modelData.setModelPose(ModelPose.CUSTOM);
    }

    // Apply rotation based on the model part.
    modelData.setModelPartRotation(this.modelPart, this.rotation);

    // Verify if custom model pose is really needed.
    if (!modelData.hasChangedModel()) {
      log.debug("Reset custom model pose for {} from {}", easyNPC, serverPlayer);
      modelData.setModelPose(ModelPose.DEFAULT);
      easyNPC.getEntity().setPose(Pose.STANDING);
    }
  }
}
