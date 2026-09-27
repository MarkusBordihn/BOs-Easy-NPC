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

package de.markusbordihn.easynpc.level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.access.SpawnerAccessHelper;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetDataUtils;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.gameevent.GameEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BaseEasyNPCSpawner extends BaseSpawner {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String SPAWN_DATA_TAG = "SpawnData";
  private static final String ENTITY_UUID_TAG = "UUID";
  private static final String STORED_PRESET_DATA_TAG = "StoredPresetData";

  private final SpawnerType spawnerType;
  private boolean isEasyNPC = false;
  private UUID easyNPCPresetUUID;
  private UUID easyNPCUUID;
  private PresetData storedPresetData;

  public BaseEasyNPCSpawner(SpawnerType spawnerType) {
    this.spawnerType = spawnerType;
    ((SpawnerAccessHelper) this).initializeSpawnerData(spawnerType, null);
  }

  @Override
  public void setNextSpawnData(Level level, BlockPos blockPos, SpawnData spawnData) {
    CompoundTag originalEntityData = spawnData.getEntityToSpawn();

    PresetData presetData = PresetDataUtils.fromSpawnData(spawnData);

    if (presetData != null && presetData.hasValidData()) {
      this.storedPresetData = presetData;
      this.easyNPCPresetUUID = presetData.getPresetUUID();
      this.easyNPCUUID = presetData.getEntityUUID();
      if (this.easyNPCUUID == null && this.usesUniqueEntity()) {
        this.easyNPCUUID = UUID.randomUUID();
        this.storedPresetData.data().putUUID(ENTITY_UUID_TAG, this.easyNPCUUID);
        log.debug(
            "[Spawner] Preset without entity UUID at {}, using generated UUID {}",
            blockPos,
            this.easyNPCUUID);
      }

      log.debug(
          "[Spawner] Setting spawn data at {} for type {} (PresetUUID: {}, EntityUUID: {})",
          blockPos,
          this.spawnerType,
          this.easyNPCPresetUUID,
          this.easyNPCUUID);
    }

    CompoundTag entityData = originalEntityData.copy();

    if (this.easyNPCPresetUUID != null) {
      entityData.putUUID(PresetData.PRESET_UUID_TAG, this.easyNPCPresetUUID);
    }
    if (this.easyNPCUUID != null && this.spawnerType != SpawnerType.GROUP_SPAWNER) {
      entityData.putUUID(ENTITY_UUID_TAG, this.easyNPCUUID);
    }

    this.updateEasyNPCData(entityData);

    entityData.remove("Pos");
    entityData.remove("Rotation");

    if (this.spawnerType == SpawnerType.GROUP_SPAWNER) {
      entityData.remove(ENTITY_UUID_TAG);
      log.debug(
          "[Spawner] GROUP_SPAWNER: Use Preset UUID {} to allow multiple spawns",
          this.easyNPCPresetUUID);
    } else {
      log.debug(
          "[Spawner] SINGLE/BOSS/DEFAULT_SPAWNER: Use UUID {} for unique entity", this.easyNPCUUID);
    }

    SpawnData cleanedSpawnData = new SpawnData(entityData, spawnData.getCustomSpawnRules());

    super.setNextSpawnData(level, blockPos, cleanedSpawnData);
  }

  @Override
  public void clientTick(Level level, BlockPos blockPos) {
    if (!this.hasEasyNPC() || !this.canSpawnBasedOnConditions(level, blockPos)) {
      return;
    }

    super.clientTick(level, blockPos);
  }

  @Override
  public void serverTick(ServerLevel serverLevel, BlockPos blockPos) {
    if (!this.hasEasyNPC() || this.spawnerType == SpawnerType.DEFAULT_SPAWNER) {
      super.serverTick(serverLevel, blockPos);
      return;
    }

    SpawnerAccessHelper spawnerAccess = (SpawnerAccessHelper) this;

    if (!this.isNearPlayer(serverLevel, blockPos)) {
      return;
    }

    if (spawnerAccess.getSpawnDelay() == -1) {
      this.resetSpawnDelay(serverLevel);
    }

    if (spawnerAccess.getSpawnDelay() > 0) {
      spawnerAccess.setSpawnDelay(spawnerAccess.getSpawnDelay() - 1);
      return;
    }

    if (!this.canSpawnBasedOnConditions(serverLevel, blockPos)) {
      return;
    }

    boolean spawned = this.performSpawn(serverLevel, blockPos);

    if (spawned) {
      this.resetSpawnDelay(serverLevel);
    }
  }

  private void resetSpawnDelay(ServerLevel serverLevel) {
    SpawnerAccessHelper spawnerAccess = (SpawnerAccessHelper) this;
    int minDelay = spawnerAccess.getMinSpawnDelay();
    int maxDelay = spawnerAccess.getMaxSpawnDelay();
    spawnerAccess.setSpawnDelay(
        minDelay >= maxDelay
            ? minDelay
            : minDelay + serverLevel.random.nextInt(maxDelay - minDelay));
  }

  private boolean isNearPlayer(ServerLevel serverLevel, BlockPos blockPos) {
    int requiredPlayerRange = ((SpawnerAccessHelper) this).getRequiredPlayerRange();

    return serverLevel.hasNearbyAlivePlayer(
        blockPos.getX() + 0.5, blockPos.getY() + 0.5, blockPos.getZ() + 0.5, requiredPlayerRange);
  }

  private boolean performSpawn(ServerLevel serverLevel, BlockPos blockPos) {
    if (this.storedPresetData == null || !this.storedPresetData.hasValidData()) {
      log.warn("[Spawner] No valid preset data available for spawning at {}", blockPos);
      return false;
    }

    RandomSource random = serverLevel.getRandom();
    SpawnerAccessHelper spawnerAccess = (SpawnerAccessHelper) this;
    int spawnCount = spawnerAccess.getSpawnCount();
    int spawnRange = spawnerAccess.getSpawnRange();
    boolean anySpawned = false;
    for (int i = 0; i < spawnCount; ++i) {
      if (this.attemptSpawn(serverLevel, blockPos, random, spawnRange)) {
        anySpawned = true;
      }
    }

    return anySpawned;
  }

  private boolean attemptSpawn(
      ServerLevel serverLevel, BlockPos blockPos, RandomSource random, int spawnRange) {
    CompoundTag entityData = this.storedPresetData.data().copy();
    this.prepareEntityDataWithUUIDs(entityData);

    double spawnX =
        blockPos.getX() + (random.nextDouble() - random.nextDouble()) * (double) spawnRange + 0.5;
    double spawnY = (double) blockPos.getY() + random.nextInt(3) - 1;
    double spawnZ =
        blockPos.getZ() + (random.nextDouble() - random.nextDouble()) * (double) spawnRange + 0.5;

    Optional<EntityType<?>> optionalEntityType = EntityType.by(entityData);
    if (optionalEntityType.isEmpty()) {
      log.warn("[Spawner] Invalid entity type in preset data");
      return false;
    }

    EntityType<?> entityType = optionalEntityType.get();

    if (!serverLevel.noCollision(entityType.getAABB(spawnX, spawnY, spawnZ))) {
      return false;
    }

    Entity entity =
        EntityType.loadEntityRecursive(
            entityData,
            serverLevel,
            loadedEntity -> {
              loadedEntity.moveTo(spawnX, spawnY, spawnZ, random.nextFloat() * 360.0F, 0.0F);
              return loadedEntity;
            });

    if (entity == null) {
      log.warn("[Spawner] Failed to load entity from preset data");
      return false;
    }

    if (entity instanceof Mob mob) {
      mob.finalizeSpawn(
          serverLevel,
          serverLevel.getCurrentDifficultyAt(entity.blockPosition()),
          MobSpawnType.SPAWNER,
          null,
          null);
    }

    if (!serverLevel.tryAddFreshEntityWithPassengers(entity)) {
      log.debug("[Spawner] Failed to add entity to world at {}", entity.blockPosition());
      return false;
    }

    BlockPos spawnPos = entity.blockPosition();
    serverLevel.levelEvent(2004, blockPos, 0);
    serverLevel.gameEvent(entity, GameEvent.ENTITY_PLACE, spawnPos);

    if (entity instanceof Mob mob) {
      mob.spawnAnim();
    }

    log.debug("[Spawner] Successfully spawned {} at {}", entity.getType(), spawnPos);
    return true;
  }

  private void prepareEntityDataWithUUIDs(CompoundTag entityData) {
    if (this.spawnerType == SpawnerType.GROUP_SPAWNER
        || this.spawnerType == SpawnerType.WORLD_SPAWNER) {
      entityData.putUUID(ENTITY_UUID_TAG, UUID.randomUUID());
    } else if (this.easyNPCUUID != null) {
      entityData.putUUID(ENTITY_UUID_TAG, this.easyNPCUUID);
    }

    if (this.easyNPCPresetUUID != null) {
      entityData.putUUID(PresetData.PRESET_UUID_TAG, this.easyNPCPresetUUID);
    }
  }

  private boolean usesUniqueEntity() {
    return this.spawnerType == SpawnerType.SINGLE_SPAWNER
        || this.spawnerType == SpawnerType.BOSS_SPAWNER;
  }

  private boolean canSpawnBasedOnConditions(Level level, BlockPos blockPos) {
    if (!this.hasEasyNPC() || this.storedPresetData == null) {
      return false;
    }

    if (this.spawnerType == SpawnerType.GROUP_SPAWNER
        || this.spawnerType == SpawnerType.WORLD_SPAWNER) {
      // Both count entities sharing the preset UUID: WORLD_SPAWNER across the whole world,
      // GROUP_SPAWNER only within its own dimension.
      if (this.easyNPCPresetUUID != null) {
        int entityCount;
        if (this.spawnerType == SpawnerType.WORLD_SPAWNER) {
          entityCount = LivingEntityManager.getEntityCountByPresetUUID(this.easyNPCPresetUUID);
        } else if (level instanceof ServerLevel serverLevel) {
          entityCount =
              LivingEntityManager.getEntityCountByPresetUUID(this.easyNPCPresetUUID, serverLevel);
        } else {
          entityCount = LivingEntityManager.getEntityCountByPresetUUID(this.easyNPCPresetUUID);
        }

        return entityCount < this.getMaxNearbyEntities();
      }
    } else if (this.usesUniqueEntity()) {
      if (this.easyNPCUUID == null) {
        return false;
      }

      if (level instanceof ServerLevel serverLevel) {
        Entity entity = serverLevel.getEntity(this.easyNPCUUID);
        return entity == null || !entity.isAlive();
      }

      EasyNPC<?> easyNPC = LivingEntityManager.getClientEasyNPCEntityByUUID(this.easyNPCUUID);
      return easyNPC == null || !easyNPC.getLivingEntity().isAlive();
    }

    return true;
  }

  public boolean hasEasyNPC() {
    return this.isEasyNPC;
  }

  private int getMaxNearbyEntities() {
    return ((SpawnerAccessHelper) this).getMaxNearbyEntities();
  }

  @Override
  public void load(Level level, BlockPos blockPos, CompoundTag compoundTag) {
    super.load(level, blockPos, compoundTag);

    if (compoundTag.contains(STORED_PRESET_DATA_TAG, 10)) {
      CompoundTag presetDataTag = compoundTag.getCompound(STORED_PRESET_DATA_TAG);
      this.storedPresetData =
          PresetDataUtils.fromSpawnData(new SpawnData(presetDataTag, Optional.empty()));

      if (this.storedPresetData != null && this.storedPresetData.hasValidData()) {
        this.easyNPCPresetUUID = this.storedPresetData.getPresetUUID();
        this.easyNPCUUID = this.storedPresetData.getEntityUUID();
        log.debug(
            "[Spawner] Loaded stored preset data with PresetUUID: {}, EntityUUID: {}",
            this.easyNPCPresetUUID,
            this.easyNPCUUID);
      }
    } else if (compoundTag.contains(SPAWN_DATA_TAG, 10)) {
      // Fallback: try to load from spawn data (backwards compatibility)
      CompoundTag spawnData = compoundTag.getCompound(SPAWN_DATA_TAG);
      if (spawnData.contains("entity")) {
        this.updateEasyNPCData(spawnData.getCompound("entity"));
      }
    }
  }

  @Override
  public CompoundTag save(CompoundTag compoundTag) {
    CompoundTag savedTag = super.save(compoundTag);

    if (this.storedPresetData != null && this.storedPresetData.hasValidData()) {
      CompoundTag presetDataTag = this.storedPresetData.data().copy();
      savedTag.put(STORED_PRESET_DATA_TAG, presetDataTag);
      log.debug(
          "[Spawner] Saved stored preset data with PresetUUID: {}, EntityUUID: {}",
          this.easyNPCPresetUUID,
          this.easyNPCUUID);
    }

    return savedTag;
  }

  @Override
  public void broadcastEvent(Level level, BlockPos blockPos, int eventId) {
    level.blockEvent(blockPos, Blocks.SPAWNER, eventId, 0);
  }

  private void updateEasyNPCData(CompoundTag compoundTag) {
    this.isEasyNPC = false;
    this.easyNPCUUID = null;
    this.easyNPCPresetUUID = null;

    if (compoundTag.contains(Entity.ID_TAG)) {
      ResourceLocation entityResourceLocation =
          ResourceLocation.tryParse(compoundTag.getString(Entity.ID_TAG));
      this.isEasyNPC =
          entityResourceLocation != null
              && entityResourceLocation.getNamespace().equals(Constants.MOD_ID);
    }

    if (compoundTag.contains(ENTITY_UUID_TAG)) {
      this.easyNPCUUID = compoundTag.getUUID(ENTITY_UUID_TAG);
    }

    if (compoundTag.contains(PresetData.PRESET_UUID_TAG)) {
      this.easyNPCPresetUUID = compoundTag.getUUID(PresetData.PRESET_UUID_TAG);
    }
  }
}
