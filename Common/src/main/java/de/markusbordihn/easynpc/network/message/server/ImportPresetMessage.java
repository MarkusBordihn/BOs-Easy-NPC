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
import de.markusbordihn.easynpc.network.message.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ImportPresetMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "import_preset");

  private final CompoundTag compoundTag;
  private final ResourceLocation resourceLocation;
  private final PresetType presetType;

  public ImportPresetMessage(
      final UUID uuid, final PresetType presetType, final CompoundTag compoundTag) {
    super(uuid);
    this.presetType = presetType;
    this.compoundTag = compoundTag;
    this.resourceLocation = null;
  }

  public ImportPresetMessage(
      final UUID uuid, final PresetType presetType, final ResourceLocation resourceLocation) {
    super(uuid);
    this.presetType = presetType;
    this.compoundTag = null;
    this.resourceLocation = resourceLocation;
  }

  public static ImportPresetMessage decode(final FriendlyByteBuf buffer) {
    UUID uuid = buffer.readUUID();
    PresetType presetType = buffer.readEnum(PresetType.class);
    if (presetType == PresetType.LOCAL) {
      CompoundTag compoundTag = buffer.readNbt();
      return new ImportPresetMessage(uuid, presetType, compoundTag);
    } else {
      ResourceLocation resourceLocation = buffer.readResourceLocation();
      return new ImportPresetMessage(uuid, presetType, resourceLocation);
    }
  }

  public static FriendlyByteBuf encode(
      final ImportPresetMessage message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeEnum(message.presetType);
    if (message.presetType == PresetType.LOCAL) {
      buffer.writeNbt(message.compoundTag);
    } else {
      buffer.writeResourceLocation(message.resourceLocation);
    }
    return buffer;
  }

  public static void handle(final FriendlyByteBuf buffer, final ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(final ImportPresetMessage message, final ServerPlayer serverPlayer) {
    if (!message.handleMessage(serverPlayer)) {
      return;
    }

    // Validate preset type and data
    EasyNPC<?> easyNPC = message.getEasyNPC();
    UUID uuid = message.getUUID();
    switch (message.getPresetType()) {
      case LOCAL:
        PresetHandler.importPreset(
            serverPlayer.serverLevel(), message.getCompoundTag(), null, uuid);
        break;
      case CUSTOM:
        PresetHandler.importCustomPreset(
            serverPlayer.serverLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      case DATA:
        PresetHandler.importDataPreset(
            serverPlayer.serverLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      case DEFAULT:
        PresetHandler.importDefaultPreset(
            serverPlayer.serverLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      case WORLD:
        PresetHandler.importWorldPreset(
            serverPlayer.serverLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      default:
        log.error("Invalid preset type {} from {}", message.getPresetType(), serverPlayer);
    }
  }

  @Override
  public FriendlyByteBuf encode() {
    return encode(this, new FriendlyByteBuf(Unpooled.buffer()));
  }

  public PresetType getPresetType() {
    return this.presetType;
  }

  public ResourceLocation getResourceLocation() {
    return this.resourceLocation;
  }

  public CompoundTag getCompoundTag() {
    return this.compoundTag;
  }
}
