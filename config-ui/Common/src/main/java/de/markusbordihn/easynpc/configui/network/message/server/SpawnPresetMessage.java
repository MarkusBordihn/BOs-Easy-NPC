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
import de.markusbordihn.easynpc.handler.PlacementHandler;
import de.markusbordihn.easynpc.handler.PresetFeedback;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.handler.PresetImportResult;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import de.markusbordihn.easynpc.security.CommandSecurity;
import de.markusbordihn.easynpc.security.FeatureSecurity;
import de.markusbordihn.easynpc.security.NpcFeature;
import de.markusbordihn.easynpc.security.SpawnRateLimiter;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public record SpawnPresetMessage(
    PresetType presetType,
    ResourceLocation resourceLocation,
    boolean useOriginalData,
    CompoundTag presetData)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "spawn_preset");
  public static final Type<SpawnPresetMessage> PAYLOAD_TYPE = new Type<>(MESSAGE_ID);
  public static final StreamCodec<RegistryFriendlyByteBuf, SpawnPresetMessage> STREAM_CODEC =
      StreamCodec.of((buffer, message) -> message.write(buffer), SpawnPresetMessage::create);

  public SpawnPresetMessage(
      PresetType presetType, ResourceLocation resourceLocation, boolean useOriginalData) {
    this(presetType, resourceLocation, useOriginalData, null);
  }

  public static SpawnPresetMessage create(final FriendlyByteBuf buffer) {
    PresetType presetType = buffer.readEnum(PresetType.class);
    ResourceLocation resourceLocation = buffer.readResourceLocation();
    boolean useOriginalData = buffer.readBoolean();
    boolean hasPresetData = buffer.readBoolean();
    CompoundTag presetData = hasPresetData ? buffer.readNbt() : null;
    return new SpawnPresetMessage(presetType, resourceLocation, useOriginalData, presetData);
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeEnum(this.presetType);
    buffer.writeResourceLocation(this.resourceLocation);
    buffer.writeBoolean(this.useOriginalData);
    buffer.writeBoolean(this.presetData != null);
    if (this.presetData != null) {
      buffer.writeNbt(this.presetData);
    }
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
    if (!MessageSecurity.checkActorFeatureAccess(
        serverPlayer, NpcFeature.SPAWN_NPC, "NPC browser spawn")) {
      return;
    }

    if (this.useOriginalData
        && !FeatureSecurity.checkActorFeatureAccess(serverPlayer, NpcFeature.POSITION).allowed()) {
      serverPlayer.sendSystemMessage(PresetFeedback.restoreDenied());
      return;
    }

    if (!SpawnRateLimiter.checkAndRecord(serverPlayer)) {
      log.warn(
          "Rate-limited NPC spawn attempt by {}. Adjust security.cfg keys npcSpawnRateLimitCreative or npcSpawnRateLimitAdmin to change this.",
          serverPlayer.getName());
      serverPlayer.sendSystemMessage(
          PresetFeedback.spawnRateLimited(SpawnRateLimiter.spawnLimit(serverPlayer)));
      return;
    }

    UUID uuid = this.useOriginalData ? null : UUID.randomUUID();
    Vec3 position = this.useOriginalData ? null : findSpawnPosition(serverPlayer);

    PresetImportResult importResult =
        this.presetData != null
            ? this.importClientPresetData(serverPlayer, position, uuid)
            : PresetHandler.importPresetWithReport(
                serverPlayer.serverLevel(),
                this.presetType,
                this.resourceLocation,
                position,
                uuid,
                CommandSecurity.getActorContext(serverPlayer),
                serverPlayer);

    if (!importResult.success()) {
      log.error(
          "Failed to spawn preset {} for player {}",
          this.resourceLocation,
          serverPlayer.getName().getString());
    }

    PresetFeedback.sendImportResult(serverPlayer, importResult, this.resourceLocation);
  }

  private static Vec3 findSpawnPosition(ServerPlayer serverPlayer) {
    Vec3 playerLook = serverPlayer.getLookAngle();
    return PlacementHandler.findFreePositionNear(
        serverPlayer.serverLevel(),
        serverPlayer.position().add(playerLook.x * 3, 0, playerLook.z * 3));
  }

  private PresetImportResult importClientPresetData(
      ServerPlayer serverPlayer, Vec3 position, UUID uuid) {
    CompoundTag resolvedPresetData =
        PresetHandler.resolveParentPresets(
            this.presetData,
            this.resourceLocation,
            this.presetType,
            serverPlayer.serverLevel().getServer());
    PresetData resolvedPreset =
        PresetData.fromCompoundTag(this.resourceLocation, this.presetType, resolvedPresetData);
    if (resolvedPreset == null || !resolvedPreset.hasValidData()) {
      log.error("Invalid preset data for {}", this.resourceLocation);
      return PresetImportResult.FAILED;
    }

    return PresetHandler.importPresetWithReport(
        serverPlayer.serverLevel(),
        resolvedPreset,
        position,
        uuid,
        CommandSecurity.getActorContext(serverPlayer),
        serverPlayer,
        null);
  }
}
