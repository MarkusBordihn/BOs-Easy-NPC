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
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;

public record SpawnPresetMessage(
    PresetType presetType,
    Identifier resourceLocation,
    boolean useOriginalData,
    CompoundTag presetData)
    implements NetworkMessageRecord {

  public static final Identifier MESSAGE_ID =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "spawn_preset");
  public static final Type<SpawnPresetMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, SpawnPresetMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), SpawnPresetMessage::create);

  public SpawnPresetMessage(
      PresetType presetType, Identifier resourceLocation, boolean useOriginalData) {
    this(presetType, resourceLocation, useOriginalData, null);
  }

  public static SpawnPresetMessage create(final FriendlyByteBuf buffer) {
    PresetType presetType = buffer.readEnum(PresetType.class);
    Identifier resourceLocation = buffer.readIdentifier();
    boolean useOriginalData = buffer.readBoolean();
    boolean hasPresetData = buffer.readBoolean();
    CompoundTag presetData = hasPresetData ? buffer.readNbt() : null;
    return new SpawnPresetMessage(presetType, resourceLocation, useOriginalData, presetData);
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeEnum(this.presetType);
    buffer.writeIdentifier(this.resourceLocation);
    buffer.writeBoolean(this.useOriginalData);
    buffer.writeBoolean(this.presetData != null);
    if (this.presetData != null) {
      buffer.writeNbt(this.presetData);
    }
  }

  @Override
  public Identifier id() {
    return MESSAGE_ID;
  }

  @Override
  public Type<? extends CustomPacketPayload> type() {
    return PAYLOAD_TYPE;
  }

  @Override
  public void handleServer(final ServerPlayer serverPlayer) {
    var playerLook = serverPlayer.getLookAngle();
    var spawnPos = serverPlayer.position().add(playerLook.x * 3, 0, playerLook.z * 3);
    var uuid = useOriginalData ? null : java.util.UUID.randomUUID();
    var position = useOriginalData ? null : spawnPos;

    boolean success;

    if (this.presetData != null) {
      var presetDataObj =
          PresetData.fromCompoundTag(this.resourceLocation, this.presetType, this.presetData);
      if (presetDataObj != null && presetDataObj.hasValidData()) {
        success =
            PresetHandler.importPreset(
                serverPlayer.level(), presetDataObj, position, uuid, serverPlayer);
      } else {
        log.error("Invalid preset data for {}", this.resourceLocation);
        success = false;
      }
    } else {
      success =
          PresetHandler.importPreset(
              serverPlayer.level(),
              this.presetType,
              this.resourceLocation,
              position,
              uuid,
              serverPlayer);
    }

    if (!success) {
      log.error(
          "Failed to spawn preset {} for player {}",
          this.resourceLocation,
          serverPlayer.getName().getString());
    }
  }
}
