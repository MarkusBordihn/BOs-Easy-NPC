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
import de.markusbordihn.easynpc.block.entity.BaseEasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.data.spawner.SpawnerSettingType;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.chunk.LevelChunk.EntityCreationType;

public record ChangeSpawnerSettingMessage(
    BlockPos blockPos, SpawnerSettingType settingType, int settingValue)
    implements NetworkMessageRecord {

  public static final ResourceLocation MESSAGE_ID =
      new ResourceLocation(Constants.MOD_ID, "change_spawner_settings");
  public static final CustomPacketPayload.Type<ChangeSpawnerSettingMessage> PAYLOAD_TYPE =
      CustomPacketPayload.createType(MESSAGE_ID.toString());
  public static final StreamCodec<RegistryFriendlyByteBuf, ChangeSpawnerSettingMessage>
      STREAM_CODEC =
          StreamCodec.of(
              (buffer, message) -> message.write(buffer), ChangeSpawnerSettingMessage::create);

  public static ChangeSpawnerSettingMessage create(final FriendlyByteBuf buffer) {
    return new ChangeSpawnerSettingMessage(
        buffer.readBlockPos(), buffer.readEnum(SpawnerSettingType.class), buffer.readInt());
  }

  @Override
  public void write(final FriendlyByteBuf buffer) {
    buffer.writeBlockPos(this.blockPos);
    buffer.writeEnum(this.settingType);
    buffer.writeInt(this.settingValue);
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
    if (serverPlayer == null) {
      return;
    }

    // Try to get block entity from the current or different server thread.
    ServerLevel serverLevel = serverPlayer.serverLevel();
    BlockEntity blockEntity = serverLevel.getBlockEntity(this.blockPos);
    if (blockEntity == null) {
      blockEntity =
          serverLevel
              .getChunkAt(this.blockPos)
              .getBlockEntity(this.blockPos, EntityCreationType.IMMEDIATE);
    }

    // Verify if block entity is a NPC spawner block entity.
    if (!(blockEntity instanceof BaseEasyNPCSpawnerBlockEntity spawnerBlockEntity)) {
      log.error(
          "Found {}({}) instead of NPC spawner block entity at {}",
          blockEntity,
          serverLevel.getBlockState(this.blockPos),
          this.blockPos);
      return;
    }

    // Verify if player has permission to change the settings.
    if (!serverPlayer.isCreative()
        && spawnerBlockEntity.getOwner() != null
        && !spawnerBlockEntity.getOwner().equals(serverPlayer.getUUID())) {
      log.warn(
          "Player {} has no permission to change the settings of spawner at {}",
          serverPlayer.getName().getString(),
          this.blockPos);
      return;
    }

    // Update the spawner settings
    switch (this.settingType) {
      case SPAWN_RANGE:
        log.debug("Set spawner {} spawn range to {}", spawnerBlockEntity, this.settingValue);
        spawnerBlockEntity.setSpawnRange(this.settingValue);
        break;
      case DESPAWN_RANGE:
        log.debug("Set spawner {} despawn range to {}", spawnerBlockEntity, this.settingValue);
        spawnerBlockEntity.setDespawnRange(this.settingValue);
        break;
      case REQUIRED_PLAYER_RANGE:
        log.debug(
            "Set spawner {} required player range to {}", spawnerBlockEntity, this.settingValue);
        spawnerBlockEntity.setRequiredPlayerRange(this.settingValue);
        break;
      case DELAY:
        log.debug("Set spawner {} delay to {}", spawnerBlockEntity, this.settingValue);
        spawnerBlockEntity.setDelay(this.settingValue);
        break;
      case MAX_NEARBY_ENTITIES:
        log.debug(
            "Set spawner {} max nearby entities to {}", spawnerBlockEntity, this.settingValue);
        spawnerBlockEntity.setMaxNearbyEntities(this.settingValue);
        break;
      case SPAWN_COUNT:
        log.debug("Set spawner {} spawn count to {}", spawnerBlockEntity, this.settingValue);
        spawnerBlockEntity.setSpawnCount(this.settingValue);
        break;
      default:
        log.error("Unknown spawner setting type {} for {}", this.settingType, spawnerBlockEntity);
    }
  }
}
