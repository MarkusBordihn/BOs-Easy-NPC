/**
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

import java.io.File;
import java.io.IOException;
import java.util.UUID;
import java.util.function.Supplier;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.network.NetworkEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.CustomPresetData;
import de.markusbordihn.easynpc.data.skin.SkinModel;

public class MessagePresetExportClient {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final UUID uuid;
  protected final String name;
  protected final String skinModel;
  protected final String fileName;
  protected final CompoundTag data;

  public MessagePresetExportClient(UUID uuid, String name, String skinModel, String fileName,
      CompoundTag data) {
    this.uuid = uuid;
    this.name = name;
    this.skinModel = skinModel;
    this.fileName = fileName;
    this.data = data;
  }

  public UUID getUUID() {
    return this.uuid;
  }

  public String getName() {
    return this.name;
  }

  public String getSkinModel() {
    return this.skinModel;
  }

  public String getFileName() {
    return this.fileName;
  }

  public CompoundTag getData() {
    return this.data;
  }

  public static void handle(MessagePresetExportClient message,
      Supplier<NetworkEvent.Context> contextSupplier) {
    NetworkEvent.Context context = contextSupplier.get();
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
    String skinModelName = message.getSkinModel();
    SkinModel skinModel = SkinModel.get(skinModelName);
    if (skinModelName == null || skinModelName.isEmpty() || skinModel == null) {
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
    log.info("Exporting EasyNPC {} with UUID {} and skin {} to {}", name, uuid, skinModel,
        presetFile);
    try {
      NbtIo.writeCompressed(data, presetFile);
    } catch (final IOException exception) {
      log.error("Failed to export EasyNPC " + name + " with UUID " + uuid + " and skin " + skinModel
          + " to " + presetFile, exception);
    }
  }

}
