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

import de.markusbordihn.easynpc.data.CustomPresetData;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.io.File;
import java.io.IOException;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.network.CustomPayloadEvent;
import net.minecraftforge.fml.DistExecutor;

public class MessagePresetExportClient extends NetworkMessage {

  protected final String name;
  protected final SkinModel skinModel;
  protected final String fileName;
  protected final CompoundTag data;

  public MessagePresetExportClient(
      UUID uuid, String name, SkinModel skinModel, String fileName, CompoundTag data) {
    super(uuid);
    this.name = name;
    this.skinModel = skinModel;
    this.fileName = fileName;
    this.data = data;
  }

  public static MessagePresetExportClient decode(final FriendlyByteBuf buffer) {
    return new MessagePresetExportClient(
        buffer.readUUID(),
        buffer.readUtf(),
        buffer.readEnum(SkinModel.class),
        buffer.readUtf(),
        buffer.readNbt());
  }

  public static void encode(final MessagePresetExportClient message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getName());
    buffer.writeEnum(message.getSkinModel());
    buffer.writeUtf(message.getFileName());
    buffer.writeNbt(message.getData());
  }

  public static void handle(MessagePresetExportClient message, CustomPayloadEvent.Context context) {
    context.enqueueWork(
        () -> DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> handlePacket(message)));
    context.setPacketHandled(true);
  }

  public static void handlePacket(MessagePresetExportClient message) {
    UUID uuid = message.getUUID();
    if (uuid == null || uuid.toString().isEmpty()) {
      log.error("Invalid UUID {} for {}", uuid, message);
      return;
    }

    // Validate name.
    String name = message.getName();
    if (name == null || name.isEmpty()) {
      log.error("Invalid name {} for {}", name, message);
      return;
    }

    // Validate skin model.
    SkinModel skinModel = message.getSkinModel();
    if (skinModel == null) {
      log.error("Invalid skin model {} for {}", skinModel, message);
      return;
    }

    // Validate data.
    CompoundTag data = message.getData();
    if (data == null) {
      log.error("Invalid data {} for {}", data, message);
      return;
    }

    // Validate name.
    String fileName = message.getFileName();
    if (fileName == null || fileName.isEmpty()) {
      log.warn("Export preset file name is empty for {}", uuid);
      return;
    }

    // Perform action.
    File presetFile = CustomPresetData.getPresetFile(skinModel, fileName);
    log.info(
        "Exporting EasyNPC {} with UUID {} and skin {} to {}", name, uuid, skinModel, presetFile);
    try {
      NbtIo.writeCompressed(data, presetFile);
    } catch (final IOException exception) {
      log.error(
          "Failed to export EasyNPC "
              + name
              + " with UUID "
              + uuid
              + " and skin "
              + skinModel
              + " to "
              + presetFile,
          exception);
    }
  }

  public String getName() {
    return this.name;
  }

  public SkinModel getSkinModel() {
    return this.skinModel;
  }

  public String getFileName() {
    return this.fileName;
  }

  public CompoundTag getData() {
    return this.data;
  }
}
