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

package de.markusbordihn.easynpc.configui.network.message.server;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.data.preset.PresetMetadata;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.io.CustomPresetDataFiles;
import de.markusbordihn.easynpc.io.PresetFileHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.security.NpcFeature;
import de.markusbordihn.easynpc.security.SecurityManager;
import java.io.File;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ExportCustomPresetServerMessage(UUID uuid, String name, PresetMetadata metadata)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "export_custom_preset_server");
  public static final Type<ExportCustomPresetServerMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ExportCustomPresetServerMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ExportCustomPresetServerMessage::create);

  public static ExportCustomPresetServerMessage create(final FriendlyByteBuf buffer) {
    return new ExportCustomPresetServerMessage(
        buffer.readUUID(), buffer.readUtf(), PresetMetadata.fromCompoundTag(buffer.readNbt()));
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeUtf(this.name);
    buffer.writeNbt(this.metadata.toCompoundTag());
  }

  @Override
  public ResourceLocation id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    EasyNPC<?> easyNPC = getEasyNPCAndCheckAccess(this.uuid, serverPlayer);
    if (easyNPC == null) {
      return;
    }

    if (!SecurityManager.checkFeatureAccess(serverPlayer, easyNPC, NpcFeature.CUSTOM_PRESET)
        .allowed()) {
      log.warn("Blocked custom preset export for {} from {}", easyNPC, serverPlayer);
      return;
    }

    if (this.name == null || this.name.isEmpty()) {
      log.warn("Export custom preset name is empty for {}", easyNPC);
      return;
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      log.warn("Export custom preset skin data is empty for {}", easyNPC);
      return;
    }

    SkinModel skinModel = skinData.getSkinModel();
    if (skinModel == null) {
      log.warn("Export custom preset skin model is empty for {}", easyNPC);
      return;
    }

    CompoundTag exportData = PresetHandler.prepareClientExportData(easyNPC, this.metadata);
    if (exportData == null || exportData.isEmpty()) {
      log.warn("Export custom preset data is empty for {}", easyNPC);
      return;
    }

    File presetFile = CustomPresetDataFiles.getPresetFile(skinModel, name);
    if (presetFile == null) {
      log.error("Failed to get custom preset file for {} with name {}", skinModel, name);
      return;
    }

    log.info(
        "Exporting EasyNPC {} with {} and skin {} to custom preset {}",
        name,
        easyNPC,
        skinModel,
        presetFile);
    if (!PresetFileHandler.saveNbt(presetFile, exportData)) {
      log.error(
          "Failed to export EasyNPC {} with {} and skin {} to custom preset {}",
          name,
          easyNPC,
          skinModel,
          presetFile);
      return;
    }

    serverPlayer.sendSystemMessage(
        Component.literal("Preset exported to server. Server-specific data was not included."));
  }
}
