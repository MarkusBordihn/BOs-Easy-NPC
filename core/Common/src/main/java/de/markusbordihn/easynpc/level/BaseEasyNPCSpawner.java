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
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.ProblemReporter;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;
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
    super();
    this.spawnerType = spawnerType;
    ((SpawnerAccessHelper) this).initializeSpawnerData(spawnerType, null);
  }

  public void updateSpawnData(Level level, BlockPos blockPos, SpawnData spawnData) {
    setNextSpawnData(level, blockPos, spawnData);
  }

  @Override
  protected void setNextSpawnData(Level level, BlockPos blockPos, SpawnData spawnData) {
    CompoundTag originalEntityData = spawnData.getEntityToSpawn();

    // Extract PresetData from SpawnData
    PresetData presetData = PresetDataUtils.fromSpawnData(spawnData);

    // Store preset data and extract UUIDs
    if (presetData != null && presetData.hasValidData()) {
      // Store preset data (Entity UUID is guaranteed by PresetDataUtils.fromSpawnData)
      this.storedPresetData = presetData;

      // Extract UUIDs (both guaranteed to be present)
      this.easyNPCPresetUUID = presetData.getPresetUUID();
      this.easyNPCUUID = presetData.getEntityUUID();

      log.debug(
          "[Spawner] Setting spawn data at {} for type {} (PresetUUID: {}, EntityUUID: {})",
          blockPos,
          this.spawnerType,
          this.easyNPCPresetUUID,
          this.easyNPCUUID);
    }

    // Clean spawn data
    CompoundTag entityData = originalEntityData.copy();

    // Inject UUIDs from spawner (source of truth)
    if (this.easyNPCPresetUUID != null) {
      CompoundTagUtils.writeUUID(entityData, PresetData.PRESET_UUID_TAG, this.easyNPCPresetUUID);
    }
    if (this.easyNPCUUID != null && this.spawnerType != SpawnerType.GROUP_SPAWNER) {
      CompoundTagUtils.writeUUID(entityData, ENTITY_UUID_TAG, this.easyNPCUUID);
    }

    // Update EasyNPC data
    updateEasyNPCData(entityData);

    // Always remove position and rotation
    entityData.remove("Pos");
    entityData.remove("Rotation");

    // Handle UUID based on spawner type
    if (this.spawnerType == SpawnerType.GROUP_SPAWNER) {
      entityData.remove(ENTITY_UUID_TAG);
      log.debug(
          "[Spawner] GROUP_SPAWNER: Use Preset UUID {} to allow multiple spawns",
          this.easyNPCPresetUUID);
    } else {
      log.debug(
          "[Spawner] SINGLE/BOSS/DEFAULT_SPAWNER: Use UUID {} for unique entity", this.easyNPCUUID);
    }

    // Create cleaned spawn data
    SpawnData cleanedSpawnData =
        new SpawnData(entityData, spawnData.getCustomSpawnRules(), spawnData.getEquipment());

    super.setNextSpawnData(level, blockPos, cleanedSpawnData);
  }

  @Override
  public void clientTick(Level level, BlockPos blockPos) {
    if (!hasEasyNPC() || !canSpawnBasedOnConditions(level, blockPos)) {
      return;
    }

    super.clientTick(level, blockPos);
  }

  @Override
  public void serverTick(ServerLevel serverLevel, BlockPos blockPos) {
    // Non-EasyNPC or DEFAULT_SPAWNER: use vanilla logic
    if (!hasEasyNPC() || this.spawnerType == SpawnerType.DEFAULT_SPAWNER) {
      super.serverTick(serverLevel, blockPos);
      return;
    }

    // Custom logic for EasyNPC spawners (SINGLE, BOSS, GROUP)
    SpawnerAccessHelper spawnerAccess = (SpawnerAccessHelper) this;

    // Check if player is nearby
    if (!isNearPlayer(serverLevel, blockPos)) {
      return;
    }

    // Handle spawn delay initialization
    if (spawnerAccess.getSpawnDelay() == -1) {
      resetSpawnDelay(serverLevel);
    }

    // Decrement spawn delay
    if (spawnerAccess.getSpawnDelay() > 0) {
      spawnerAccess.setSpawnDelay(spawnerAccess.getSpawnDelay() - 1);
      return;
    }

    // Check spawn conditions
    if (!canSpawnBasedOnConditions(serverLevel, blockPos)) {
      return;
    }

    // Perform spawn
    boolean spawned = performSpawn(serverLevel, blockPos);

    if (spawned) {
      resetSpawnDelay(serverLevel);
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
      if (attemptSpawn(serverLevel, blockPos, random, spawnRange)) {
        anySpawned = true;
      }
    }

    return anySpawned;
  }

  private boolean attemptSpawn(
      ServerLevel serverLevel, BlockPos blockPos, RandomSource random, int spawnRange) {
    // Prepare entity data with correct UUIDs
    CompoundTag entityData = this.storedPresetData.data().copy();
    prepareEntityDataWithUUIDs(entityData);

    // Calculate random spawn position
    double spawnX =
        blockPos.getX() + (random.nextDouble() - random.nextDouble()) * spawnRange + 0.5;
    double spawnY = blockPos.getY() + random.nextInt(3) - 1;
    double spawnZ =
        blockPos.getZ() + (random.nextDouble() - random.nextDouble()) * spawnRange + 0.5;

    // Get and validate entity type using ValueInput
    ValueInput valueInput =
        TagValueInput.create(ProblemReporter.DISCARDING, serverLevel.registryAccess(), entityData);
    Optional<EntityType<?>> entityTypeOpt = EntityType.by(valueInput);
    if (entityTypeOpt.isEmpty()) {
      log.warn("[Spawner] Invalid entity type in preset data");
      return false;
    }
    EntityType<?> entityType = entityTypeOpt.get();

    // Check collision before spawning
    if (!serverLevel.noCollision(entityType.getSpawnAABB(spawnX, spawnY, spawnZ))) {
      return false;
    }

    // Load and configure entity
    Entity entity =
        EntityType.loadEntityRecursive(
            valueInput,
            serverLevel,
            EntitySpawnReason.SPAWNER,
            loadedEntity -> {
              loadedEntity.snapTo(
                  spawnX, spawnY, spawnZ, loadedEntity.getYRot(), loadedEntity.getXRot());
              return loadedEntity;
            });

    if (entity == null) {
      log.warn("[Spawner] Failed to load entity from preset data");
      return false;
    }

    // Set random rotation after loading
    entity.snapTo(entity.getX(), entity.getY(), entity.getZ(), random.nextFloat() * 360.0F, 0.0F);

    // Finalize spawn for Mobs
    if (entity instanceof Mob mob) {
      mob.finalizeSpawn(
          serverLevel,
          serverLevel.getCurrentDifficultyAt(entity.blockPosition()),
          EntitySpawnReason.SPAWNER,
          null);
    }

    // Try to add entity to world
    if (!serverLevel.tryAddFreshEntityWithPassengers(entity)) {
      log.debug("[Spawner] Failed to add entity to world at {}", entity.blockPosition());
      return false;
    }

    // Spawn effects and animations
    BlockPos spawnPos = entity.blockPosition();
    serverLevel.levelEvent(2004, blockPos, 0);
    serverLevel.gameEvent(
        entity, net.minecraft.world.level.gameevent.GameEvent.ENTITY_PLACE, spawnPos);

    if (entity instanceof Mob mob) {
      mob.spawnAnim();
    }

    log.debug("[Spawner] Successfully spawned {} at {}", entity.getType(), spawnPos);
    return true;
  }

  private void prepareEntityDataWithUUIDs(CompoundTag entityData) {
    // For GROUP_SPAWNER / WORLD_SPAWNER: generate new entity UUID for each spawn
    if (this.spawnerType == SpawnerType.GROUP_SPAWNER
        || this.spawnerType == SpawnerType.WORLD_SPAWNER) {
      CompoundTagUtils.writeUUID(entityData, ENTITY_UUID_TAG, UUID.randomUUID());
    } else if (this.easyNPCUUID != null) {
      // For SINGLE/BOSS/DEFAULT: use stored entity UUID
      CompoundTagUtils.writeUUID(entityData, ENTITY_UUID_TAG, this.easyNPCUUID);
    }

    // Always set preset UUID
    if (this.easyNPCPresetUUID != null) {
      CompoundTagUtils.writeUUID(entityData, PresetData.PRESET_UUID_TAG, this.easyNPCPresetUUID);
    }
  }

  private boolean canSpawnBasedOnConditions(Level level, BlockPos blockPos) {
    if (!hasEasyNPC() || this.storedPresetData == null) {
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

        return entityCount < getMaxNearbyEntities();
      }
    } else {
      // SINGLE_SPAWNER and BOSS_SPAWNER use Entity UUID to check if specific entity is alive
      if (this.easyNPCUUID != null) {
        if (level instanceof ServerLevel serverLevel) {
          Entity entity = serverLevel.getEntity(this.easyNPCUUID);
          return entity == null || !entity.isAlive();
        } else {
          EasyNPC<?> easyNPC = LivingEntityManager.getClientEasyNPCEntityByUUID(this.easyNPCUUID);
          return easyNPC == null || !easyNPC.getLivingEntity().isAlive();
        }
      }
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
  public void load(Level level, BlockPos blockPos, ValueInput valueInput) {
    super.load(level, blockPos, valueInput);

    // Load stored preset data if available
    TagValueOutput tagValueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);
    save(tagValueOutput);
    CompoundTag compoundTag = tagValueOutput.buildResult();
    if (compoundTag.contains(STORED_PRESET_DATA_TAG)) {
      Optional<CompoundTag> presetDataTagOpt = compoundTag.getCompound(STORED_PRESET_DATA_TAG);
      if (presetDataTagOpt.isPresent()) {
        CompoundTag presetDataTag = presetDataTagOpt.get();
        this.storedPresetData =
            PresetDataUtils.fromSpawnData(
                new SpawnData(presetDataTag, Optional.empty(), Optional.empty()));

        if (this.storedPresetData != null && this.storedPresetData.hasValidData()) {
          this.easyNPCPresetUUID = this.storedPresetData.getPresetUUID();
          this.easyNPCUUID = this.storedPresetData.getEntityUUID();
          log.debug(
              "[Spawner] Loaded stored preset data with PresetUUID: {}, EntityUUID: {}",
              this.easyNPCPresetUUID,
              this.easyNPCUUID);
        }
      }
    } else if (compoundTag.contains(SPAWN_DATA_TAG)) {
      // Fallback: try to load from spawn data (backwards compatibility)
      Optional<CompoundTag> spawnDataOpt = compoundTag.getCompound(SPAWN_DATA_TAG);
      if (spawnDataOpt.isPresent()) {
        CompoundTag spawnData = spawnDataOpt.get();
        if (spawnData.contains("entity")) {
          Optional<CompoundTag> entityOpt = spawnData.getCompound("entity");
          entityOpt.ifPresent(this::updateEasyNPCData);
        }
      }
    }
  }

  @Override
  public void save(ValueOutput valueOutput) {
    // Call super first
    super.save(valueOutput);

    // Save stored preset data
    if (this.storedPresetData != null && this.storedPresetData.hasValidData()) {
      CompoundTag presetDataTag = this.storedPresetData.data().copy();
      valueOutput.store(STORED_PRESET_DATA_TAG, CompoundTag.CODEC, presetDataTag);
      log.debug(
          "[Spawner] Saved stored preset data with PresetUUID: {}, EntityUUID: {}",
          this.easyNPCPresetUUID,
          this.easyNPCUUID);
    }
  }

  @Override
  public void broadcastEvent(Level level, BlockPos blockPos, int eventId) {
    level.blockEvent(blockPos, Blocks.SPAWNER, eventId, 0);
  }

  private void updateEasyNPCData(CompoundTag compoundTag) {
    this.isEasyNPC = false;
    this.easyNPCUUID = null;
    this.easyNPCPresetUUID = null;

    if (compoundTag.contains("id")) {
      Optional<String> idOptional = compoundTag.getString("id");
      if (idOptional.isPresent()) {
        Identifier entityIdentifier = Identifier.tryParse(idOptional.get());
        if (entityIdentifier != null) {
          this.isEasyNPC = entityIdentifier.getNamespace().equals(Constants.MOD_ID);
        }
      }
    }

    if (compoundTag.contains(ENTITY_UUID_TAG)) {
      Optional<UUID> uuidOptional = compoundTag.read(ENTITY_UUID_TAG, UUIDUtil.CODEC);
      uuidOptional.ifPresent(uuid -> this.easyNPCUUID = uuid);
    }

    if (compoundTag.contains(PresetDataCapable.PRESET_UUID_TAG)) {
      Optional<UUID> presetUuidOptional =
          compoundTag.read(PresetDataCapable.PRESET_UUID_TAG, UUIDUtil.CODEC);
      presetUuidOptional.ifPresent(uuid -> this.easyNPCPresetUUID = uuid);
    }
  }
}
