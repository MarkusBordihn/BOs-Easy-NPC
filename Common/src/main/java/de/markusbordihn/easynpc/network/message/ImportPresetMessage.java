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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.network.NetworkMessage;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ImportPresetMessage extends NetworkMessage {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "import_preset");

  protected final CompoundTag compoundTag;
  protected final ResourceLocation resourceLocation;
  protected final PresetType presetType;

  public ImportPresetMessage(UUID uuid, PresetType presetType, CompoundTag compoundTag) {
    super(uuid);
    this.presetType = presetType;
    this.compoundTag = compoundTag;
    this.resourceLocation = null;
  }

  public ImportPresetMessage(UUID uuid, PresetType presetType, ResourceLocation resourceLocation) {
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

  public static void handle(final FriendlyByteBuf buffer, ServerPlayer serverPlayer) {
    handle(decode(buffer), serverPlayer);
  }

  public static void handle(ImportPresetMessage message, ServerPlayer serverPlayer) {
    UUID uuid = message.getUUID();
    if (!NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (easyNPC == null) {
      log.error("Unable to get valid entity with UUID {} for {}", uuid, serverPlayer);
      return;
    }

    // Validate preset type and data
    switch (message.getPresetType()) {
      case LOCAL:
        PresetHandler.importPreset(serverPlayer.getLevel(), message.getCompoundTag(), null, uuid);
        break;
      case CUSTOM:
        PresetHandler.importCustomPreset(
            serverPlayer.getLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      case DATA:
        PresetHandler.importDataPreset(
            serverPlayer.getLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      case DEFAULT:
        PresetHandler.importDefaultPreset(
            serverPlayer.getLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      case WORLD:
        PresetHandler.importWorldPreset(
            serverPlayer.getLevel(),
            message.getResourceLocation(),
            easyNPC.getEntity().position(),
            uuid);
        break;
      default:
        log.error("Invalid preset type {} from {}", message.getPresetType(), serverPlayer);
    }
  }

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
