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

import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.io.File;
import java.io.IOException;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessagePresetExportWorld extends NetworkMessage {

  protected final String name;

  public MessagePresetExportWorld(UUID uuid, String name) {
    super(uuid);
    this.name = name;
  }

  public static MessagePresetExportWorld decode(final FriendlyByteBuf buffer) {
    return new MessagePresetExportWorld(buffer.readUUID(), buffer.readUtf());
  }

  public static void encode(final MessagePresetExportWorld message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeUtf(message.getName());
  }

  public static void handle(MessagePresetExportWorld message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessagePresetExportWorld message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate name.
    String name = message.getName();
    if (name == null || name.isEmpty()) {
      log.warn("Export preset name is empty for {}", uuid);
      return;
    }

    // Validate skin data.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.warn("Export preset skin data is empty for {}", uuid);
      return;
    }

    // Validate Skin Model
    SkinModel skinModel = skinData.getSkinModel();
    if (skinModel == null) {
      log.warn("Export preset skin model is empty for {}", uuid);
      return;
    }

    // Validate data.
    CompoundTag data = easyNPC.exportPreset();
    if (data == null) {
      log.warn("Export preset data is empty for {}", uuid);
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
        "Exporting EasyNPC {} with UUID {} and skin {} to {}", name, uuid, skinModel, presetFile);
    try {
      NbtIo.writeCompressed(data, presetFile);
    } catch (final IOException exception) {
      log.error(
          "Failed to export EasyNPC {} with UUID {} and skin {} to {}",
          name,
          uuid,
          skinModel,
          presetFile,
          exception);
    }
  }

  public String getName() {
    return this.name;
  }
}
