/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.block.entity;

import de.markusbordihn.easynpc.block.EasyNPCSpawnerBlock;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import de.markusbordihn.easynpc.level.BaseEasyNPCSpawner;
import java.util.Objects;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.protocol.game.ClientboundBlockEntityDataPacket;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class EasyNPCSpawnerBlockEntity extends BlockEntity {

  public static final String NAME = "easy_npc_spawner_entity";
  public static final String SPAWNER_OWNER_TAG = "Owner";
  public static final String UUID_TAG = "UUID";
  private final BaseEasyNPCSpawner spawner;
  private final SpawnerType spawnerType;
  private UUID owner;
  private UUID spawnerUUID;

  public EasyNPCSpawnerBlockEntity(
      BlockEntityType<?> blockEntityType, BlockPos blockPos, BlockState blockState) {
    this(blockEntityType, blockPos, blockState, EasyNPCSpawnerBlock.getSpawnerType(blockState));
  }

  public EasyNPCSpawnerBlockEntity(
      BlockEntityType<?> blockEntityType,
      BlockPos blockPos,
      BlockState blockState,
      SpawnerType spawnerType) {
    super(blockEntityType, blockPos, blockState);
    this.spawnerType = spawnerType;
    this.spawner = new BaseEasyNPCSpawner(spawnerType);
  }

  public static void clientTick(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      EasyNPCSpawnerBlockEntity blockEntity) {
    if (!blockEntity.spawner.hasEasyNPC()) {
      return;
    }
    blockEntity.spawner.clientTick(level, blockPos);
  }

  public static void serverTick(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      EasyNPCSpawnerBlockEntity blockEntity) {
    if (!blockEntity.spawner.hasEasyNPC()) {
      return;
    }
    blockEntity.spawner.serverTick((ServerLevel) level, blockPos);
  }

  public SpawnerType getSpawnerType() {
    return this.spawnerType;
  }

  public UUID getOwner() {
    return this.owner;
  }

  public void setOwner(LivingEntity livingEntity) {
    this.owner = livingEntity.getUUID();
    this.setChanged();
  }

  public UUID getSpawnerUUID() {
    return this.spawnerUUID;
  }

  public void setSpawnerUUID(UUID spawnerUUID) {
    this.spawnerUUID = spawnerUUID;
    this.setChanged();
  }

  @Override
  public void setChanged() {
    super.setChanged();
    this.spawner.updateSpawnData(this.spawner.save(new CompoundTag()));
  }

  @Override
  public ClientboundBlockEntityDataPacket getUpdatePacket() {
    return ClientboundBlockEntityDataPacket.create(this);
  }

  @Override
  public CompoundTag getUpdateTag(HolderLookup.Provider provider) {
    CompoundTag compoundTag = this.saveWithoutMetadata(provider);
    compoundTag.remove("SpawnPotentials");
    return compoundTag;
  }

  @Override
  public boolean triggerEvent(int id, int value) {
    return this.spawner.onEventTriggered(this.level, id) || super.triggerEvent(id, value);
  }

  public BaseEasyNPCSpawner getSpawner() {
    return this.spawner;
  }

  @Override
  public void loadAdditional(CompoundTag compoundTag, HolderLookup.Provider provider) {
    super.loadAdditional(compoundTag, provider);
    this.spawnerUUID =
        compoundTag.contains(UUID_TAG) ? compoundTag.getUUID(UUID_TAG) : UUID.randomUUID();
    this.owner =
        compoundTag.contains(SPAWNER_OWNER_TAG) ? compoundTag.getUUID(SPAWNER_OWNER_TAG) : null;
    this.spawner.load(this.level, this.worldPosition, compoundTag);
  }

  @Override
  public void saveAdditional(CompoundTag compoundTag, HolderLookup.Provider provider) {
    super.saveAdditional(compoundTag, provider);
    compoundTag.putUUID(
        UUID_TAG, Objects.requireNonNullElseGet(this.spawnerUUID, UUID::randomUUID));
    if (this.owner != null) {
      compoundTag.putUUID(SPAWNER_OWNER_TAG, this.owner);
    }
    this.spawner.save(compoundTag);
  }
}
