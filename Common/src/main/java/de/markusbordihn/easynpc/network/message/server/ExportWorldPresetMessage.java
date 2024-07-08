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
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import java.io.File;
import java.io.IOException;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ExportWorldPresetMessage extends NetworkMessage<ExportWorldPresetMessage> {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "export_world_preset");

  protected final String name;

  public ExportWorldPresetMessage(final UUID uuid, final String name) {
    super(uuid);
    this.name = name;
  }

  public static ExportWorldPresetMessage decode(final FriendlyByteBuf buffer) {
    return new ExportWorldPresetMessage(buffer.readUUID(), buffer.readUtf());
  }

  public static FriendlyByteBuf encode(
      final ExportWorldPresetMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getName());
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(ExportWorldPresetMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate name.
    String name = message.getName();
    if (name == null || name.isEmpty()) {
      log.warn("Export preset name is empty for {}", message.getUUID());
      return;
    }

    // Validate skin data.
    SkinData<?> skinData = message.getEasyNPC().getEasyNPCSkinData();
    if (skinData == null) {
      log.warn("Export preset skin data is empty for {}", message.getUUID());
      return;
    }

    // Validate Skin Model
    SkinModel skinModel = skinData.getSkinModel();
    if (skinModel == null) {
      log.warn("Export preset skin model is empty for {}", message.getUUID());
      return;
    }

    // Validate data.
    PresetData<?> presetData = message.getEasyNPC().getEasyNPCPresetData();
    CompoundTag compoundTag = presetData.exportPresetData();
    if (compoundTag == null || compoundTag.isEmpty()) {
      log.warn("Export preset data is empty for {}", message.getUUID());
      return;
    }

    // Validate preset file.
    File presetFile = WorldPresetDataFiles.getPresetFile(skinModel, name);
    if (presetFile == null) {
      log.error("Failed to get preset file for {} with name {}", skinModel, name);
      return;
    }

    // Perform action.
    log.info(
        "Exporting EasyNPC {} with UUID {} and skin {} to {}",
        name,
        message.getUUID(),
        skinModel,
        presetFile);
    try {
      NbtIo.writeCompressed(compoundTag, presetFile.toPath());
    } catch (final IOException exception) {
      log.error(
          "Failed to export EasyNPC {} with UUID {} and skin {} to {}",
          name,
          message.getUUID(),
          skinModel,
          presetFile,
          exception);
    }
  }

  @Override
  public FriendlyByteBuf encodeBuffer(FriendlyByteBuf buffer) {
    return encode(this, buffer);
  }

  @Override
  public ExportWorldPresetMessage decodeBuffer(FriendlyByteBuf buffer) {
    return decode(buffer);
  }

  public String getName() {
    return this.name;
  }
}
