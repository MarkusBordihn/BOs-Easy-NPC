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

package de.markusbordihn.easynpc.network.message.client;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.io.File;
import java.io.IOException;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;

public record ExportClientPresetMessage(
    UUID uuid, String name, SkinModel skinModel, String fileName, CompoundTag data)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "preset_export_client");
  public static final CustomPacketPayload.Type<ExportClientPresetMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, ExportClientPresetMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ExportClientPresetMessage::create);

  public static ExportClientPresetMessage create(final FriendlyByteBuf buffer) {
    return new ExportClientPresetMessage(
        buffer.readUUID(),
        buffer.readUtf(),
        buffer.readEnum(SkinModel.class),
        buffer.readUtf(),
        buffer.readNbt());
  }

  @Override
  public void write(FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUtf(this.name);
    buffer.writeEnum(this.skinModel);
    buffer.writeUtf(this.fileName);
    buffer.writeNbt(this.data);
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<ExportClientPresetMessage> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleClient() {
    if (this.uuid == null || this.uuid.toString().isEmpty()) {
      log.error("Invalid UUID {} for {}", this.uuid, this);
      return;
    }

    // Validate name.
    if (this.name == null || this.name.isEmpty()) {
      log.error("Invalid name {} for {}", this.name, this);
      return;
    }

    // Validate skin model.
    if (this.skinModel == null) {
      log.error("Invalid skin model for {}", this);
      return;
    }

    // Validate data.
    if (this.data == null) {
      log.error("Invalid data for {}", this);
      return;
    }

    // Validate name.
    if (this.fileName == null || this.fileName.isEmpty()) {
      log.warn("Export preset file name is empty for {}", uuid);
      return;
    }

    // Perform action.
    File presetFile = CustomPresetDataFiles.getPresetFile(this.skinModel, this.fileName);
    if (presetFile == null) {
      log.error("Failed to get preset file for {}", this);
      return;
    }

    // Export preset file.
    log.info(
        "Exporting EasyNPC {} with UUID {} and skin {} to {}", name, uuid, skinModel, presetFile);
    try {
      NbtIo.writeCompressed(data, presetFile.toPath());
    } catch (final IOException exception) {
      log.error(
          "Failed to export EasyNPC {} with UUID {} and skin {} to {}:",
          name,
          uuid,
          skinModel,
          presetFile,
          exception);
    }
  }
}
