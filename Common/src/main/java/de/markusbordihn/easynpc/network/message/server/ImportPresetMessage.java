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
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public record ImportPresetMessage(
    UUID uuid, PresetType presetType, CompoundTag compoundTag, ResourceLocation resourceLocation)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "import_preset");
  public static final CustomPacketPayload.Type<ImportPresetMessage> PAYLOAD_TYPE =
      new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, ImportPresetMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), ImportPresetMessage::create);

  public static ImportPresetMessage create(final FriendlyByteBuf buffer) {
    return new ImportPresetMessage(
        buffer.readUUID(),
        buffer.readEnum(PresetType.class),
        buffer.readNbt(),
        buffer.readResourceLocation());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeUUID(this.uuid);
    buffer.writeEnum(this.presetType);
    buffer.writeNbt(this.compoundTag);
    buffer.writeResourceLocation(this.resourceLocation);
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

    // Validate preset type and data
    switch (this.presetType) {
      case LOCAL:
        PresetHandler.importLocalPreset(
            serverPlayer.serverLevel(),
            this.compoundTag,
            this.resourceLocation,
            easyNPC.getEntity().position(),
            this.uuid);
        break;
      case CUSTOM:
        PresetHandler.importCustomPreset(
            serverPlayer.serverLevel(),
            this.resourceLocation,
            easyNPC.getEntity().position(),
            this.uuid);
        break;
      case DATA:
        PresetHandler.importDataPreset(
            serverPlayer.serverLevel(),
            this.resourceLocation,
            easyNPC.getEntity().position(),
            this.uuid);
        break;
      case DEFAULT:
        PresetHandler.importDefaultPreset(
            serverPlayer.serverLevel(),
            this.resourceLocation,
            easyNPC.getEntity().position(),
            this.uuid);
        break;
      case WORLD:
        PresetHandler.importWorldPreset(
            serverPlayer.serverLevel(),
            this.resourceLocation,
            easyNPC.getEntity().position(),
            this.uuid);
        break;
      default:
        log.error("Invalid preset type {} from {}", this.presetType, serverPlayer);
    }
  }
}
