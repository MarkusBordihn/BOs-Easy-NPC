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

import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.io.WorldPresetDataFiles;
import de.markusbordihn.easynpc.network.NetworkMessage;
import java.io.IOException;
import java.nio.file.Path;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.network.CustomPayloadEvent;

public class MessagePresetImportWorld extends NetworkMessage {

  protected final ResourceLocation resourceLocation;

  public MessagePresetImportWorld(UUID uuid, ResourceLocation resourceLocation) {
    super(uuid);
    this.resourceLocation = resourceLocation;
  }

  public static MessagePresetImportWorld decode(final FriendlyByteBuf buffer) {
    return new MessagePresetImportWorld(buffer.readUUID(), buffer.readResourceLocation());
  }

  public static void encode(final MessagePresetImportWorld message, final FriendlyByteBuf buffer) {
    buffer.writeUUID(message.uuid);
    buffer.writeResourceLocation(message.getResourceLocation());
  }

  public static void handle(MessagePresetImportWorld message, CustomPayloadEvent.Context context) {
    context.enqueueWork(() -> handlePacket(message, context));
    context.setPacketHandled(true);
  }

  public static void handlePacket(
      MessagePresetImportWorld message, CustomPayloadEvent.Context context) {
    ServerPlayer serverPlayer = context.getSender();
    UUID uuid = message.getUUID();
    if (serverPlayer == null || !NetworkMessage.checkAccess(uuid, serverPlayer)) {
      return;
    }

    // Validate resource location.
    ResourceLocation resourceLocation = message.getResourceLocation();
    if (resourceLocation == null) {
      log.error("Invalid resource location {} for {}", resourceLocation, serverPlayer);
      return;
    }

    // Check if preset exists.
    Path presetPath = WorldPresetDataFiles.getPresetsResourceLocationPath(resourceLocation);
    if (!presetPath.toFile().exists()) {
      log.error("Preset {} does not exists for {}", resourceLocation, serverPlayer);
      return;
    }

    // Read preset file and create compound tag.
    CompoundTag compoundTag;
    try {
      compoundTag = NbtIo.readCompressed(presetPath.toFile());
    } catch (IOException exception) {
      log.error("Failed to read NBT data from {}", resourceLocation, exception);
      return;
    }
    if (compoundTag == null) {
      log.error("Received empty preset {} for {}", resourceLocation, serverPlayer);
      return;
    }

    // Remove UUID from NBT data, if present.
    if (compoundTag.contains("UUID")) {
      compoundTag.remove("UUID");
    }

    // Remove position from NBT data, if present.
    if (compoundTag.contains("Pos")) {
      compoundTag.remove("Pos");
    }

    // Validate entity encoded id, if set.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, serverPlayer);
    if (compoundTag.contains("id")
        && !compoundTag.getString("id").isEmpty()
        && !compoundTag.getString("id").equals(easyNPC.getEntity().getEncodeId())) {
      log.error(
          "Invalid id {} for {} expected {} from {}",
          compoundTag.getString("id"),
          easyNPC,
          easyNPC.getEntity().getEncodeId(),
          serverPlayer);
      return;
    }

    // Validate preset data.
    PresetData<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      log.error("Invalid preset data for {} from {}", message, serverPlayer);
      return;
    }

    // Perform action.
    presetData.importPresetData(compoundTag);
  }

  public ResourceLocation getResourceLocation() {
    return this.resourceLocation;
  }
}
