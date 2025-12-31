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
import de.markusbordihn.easynpc.data.spawner.SpawnerData;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Optional;
import java.util.Random;
import java.util.UUID;
import java.util.function.Function;
import net.minecraft.core.BlockPos;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.level.storage.ValueInput;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BaseEasyNPCSpawner extends BaseSpawner {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final Random random = new Random();
  private final SpawnerType spawnerType;
  private boolean isEasyNPC;
  private double oSpin;
  private double spin;
  private Entity displayEntity;
  private int maxNearbyEntities;
  private int maxSpawnDelay;
  private int minSpawnDelay;
  private int requiredPlayerRange;
  private int spawnCount;
  private int spawnDelay;
  private int spawnRange;
  private SpawnData nextSpawnData;
  private UUID easyNPCPresetUUID;
  private UUID easyNPCUUID;

  public BaseEasyNPCSpawner(SpawnerType spawnerType) {
    super();
    this.spawnerType = spawnerType;
    this.nextSpawnData = new SpawnData();
    this.spawnDelay = -1;
    CompoundTag compoundTag = new CompoundTag();
    SpawnerData.setSpawnData(spawnerType, compoundTag);
    updateSpawnData(compoundTag);
  }

  @Override
  public void broadcastEvent(Level level, BlockPos blockPos, int eventId) {
    level.blockEvent(blockPos, Blocks.SPAWNER, eventId, 0);
  }

  @Override
  public void setNextSpawnData(Level level, BlockPos blockPos, SpawnData spawnData) {
    super.setNextSpawnData(level, blockPos, spawnData);
    // Reset display entity when spawn data changes
    this.displayEntity = null;
    if (level != null) {
      BlockState blockState = level.getBlockState(blockPos);
      level.sendBlockUpdated(blockPos, blockState, blockState, 4);
    }
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);
    this.save(valueOutput);
    CompoundTag compoundTag = valueOutput.buildResult();
    updateSpawnData(compoundTag);
    log.debug("Updated spawn data for spawner at {} - isEasyNPC: {}", blockPos, this.isEasyNPC);
  }

  @Override
  public void clientTick(Level level, BlockPos blockPos) {
    if (!this.isNearPlayer(level, blockPos, this.requiredPlayerRange)) {
      this.oSpin = this.spin;
    } else if (this.displayEntity != null) {
      double x = blockPos.getX() + level.random.nextDouble();
      double y = blockPos.getY() + level.random.nextDouble();
      double z = blockPos.getZ() + level.random.nextDouble();
      level.addParticle(ParticleTypes.SMOKE, x, y, z, 0.0F, 0.0F, 0.0F);
      level.addParticle(ParticleTypes.FLAME, x, y, z, 0.0F, 0.0F, 0.0F);
      if (this.spawnDelay > 0) {
        --this.spawnDelay;
      }
      this.oSpin = this.spin;
      this.spin = (this.spin + 1000.0 / (this.spawnDelay + 200.0)) % 360.0;
    }
  }

  @Override
  public void serverTick(ServerLevel serverLevel, BlockPos blockPos) {
    if (!this.isNearPlayer(serverLevel, blockPos, this.requiredPlayerRange)) {
      return;
    }

    if (this.spawnDelay == -1) {
      this.delay(serverLevel, blockPos);
    }

    if (this.spawnDelay > 0) {
      --this.spawnDelay;
      return;
    }

    if (!hasEasyNPC()) {
      return;
    }

    if (this.maxNearbyEntities == 1 && this.easyNPCUUID != null) {
      Entity entity = serverLevel.getEntity(this.easyNPCUUID);
      if (entity != null && entity.isAlive()) {
        this.delay(serverLevel, blockPos);
        return;
      }
    }

    if (this.easyNPCPresetUUID != null
        && LivingEntityManager.getEntityCountByPresetUUID(this.easyNPCPresetUUID)
            >= this.maxNearbyEntities) {
      this.delay(serverLevel, blockPos);
      return;
    }

    this.spawnEasyNPC(serverLevel, blockPos);
  }

  @Override
  public Entity getOrCreateDisplayEntity(Level level, BlockPos blockPos) {
    if (this.displayEntity == null) {
      CompoundTag compoundTag = this.nextSpawnData.getEntityToSpawn();
      if (compoundTag.getString("id").isEmpty()) {
        log.debug("No entity id in spawn data for spawner at {}", blockPos);
        return null;
      }
      this.displayEntity =
          EntityType.loadEntityRecursive(
              compoundTag, level, EntitySpawnReason.SPAWNER, Function.identity());
      if (this.displayEntity != null) {
        log.debug(
            "Created display entity {} for spawner at {}", this.displayEntity.getType(), blockPos);
      } else {
        log.warn("Failed to create display entity for spawner at {}", blockPos);
      }
    }
    return this.displayEntity;
  }

  @Override
  public double getSpin() {
    return this.spin;
  }

  @Override
  public double getOSpin() {
    return this.oSpin;
  }

  private boolean isNearPlayer(Level level, BlockPos pos, int requiredPlayerRange) {
    return level.hasNearbyAlivePlayer(
        pos.getX() + 0.5, pos.getY() + 0.5, pos.getZ() + 0.5, requiredPlayerRange);
  }

  private void delay(Level level, BlockPos pos) {
    this.spawnDelay =
        (this.maxSpawnDelay <= this.minSpawnDelay)
            ? this.minSpawnDelay
            : this.minSpawnDelay + this.random.nextInt(this.maxSpawnDelay - this.minSpawnDelay);
    this.broadcastEvent(level, pos, 1);
  }

  private void spawnEasyNPC(ServerLevel level, BlockPos pos) {
    boolean spawned = false;

    for (int i = 0; i < this.spawnCount; ++i) {
      CompoundTag entityTag = this.nextSpawnData.getEntityToSpawn();
      ValueInput valueInput =
          TagValueInput.create(ProblemReporter.DISCARDING, level.registryAccess(), entityTag);
      Optional<EntityType<?>> entityType = EntityType.by(valueInput);
      if (entityType.isEmpty()) {
        this.delay(level, pos);
        return;
      }

      // Remove UUID when spawning multiple entities to avoid duplicate UUIDs
      if (this.maxNearbyEntities > 1 && entityTag.contains("UUID")) {
        entityTag.remove("UUID");
      }

      ListTag posList = entityTag.getListOrEmpty("Pos");
      int posSize = posList.size();
      double x =
          posSize >= 1
              ? posList.getDouble(0).orElse(0.0)
              : pos.getX()
                  + (level.random.nextDouble() - level.random.nextDouble()) * this.spawnRange
                  + 0.5;
      double y =
          posSize >= 2
              ? posList.getDouble(1).orElse(0.0)
              : pos.getY() + level.random.nextInt(3) - 1;
      double z =
          posSize >= 3
              ? posList.getDouble(2).orElse(0.0)
              : pos.getZ()
                  + (level.random.nextDouble() - level.random.nextDouble()) * this.spawnRange
                  + 0.5;

      if (!level.noCollision(entityType.get().getSpawnAABB(x, y, z))) {
        if (this.maxNearbyEntities > 1) {
          BlockPos possibleSpawnPositions =
              BlockPos.findClosestMatch(
                      pos,
                      this.spawnRange,
                      this.spawnRange,
                      possibleBlockPos ->
                          level.noCollision(
                                  entityType
                                      .get()
                                      .getSpawnAABB(
                                          possibleBlockPos.getX() + 0.5,
                                          possibleBlockPos.getY(),
                                          possibleBlockPos.getZ() + 0.5))
                              && !level.getBlockState(possibleBlockPos.below()).isAir())
                  .orElse(null);
          if (possibleSpawnPositions != null) {
            x = possibleSpawnPositions.getX() + 0.5;
            y = possibleSpawnPositions.getY();
            z = possibleSpawnPositions.getZ() + 0.5;
          } else {
            continue;
          }
        } else {
          continue;
        }
      }

      // Load the entity and set the position
      double finalX = x;
      double finalY = y;
      double finalZ = z;
      Entity entity =
          EntityType.loadEntityRecursive(
              entityTag,
              level,
              EntitySpawnReason.SPAWNER,
              loadedEntity -> {
                loadedEntity.snapTo(
                    finalX, finalY, finalZ, loadedEntity.getYRot(), loadedEntity.getXRot());
                return loadedEntity;
              });
      if (entity == null) {
        this.delay(level, pos);
        return;
      }
      entity.snapTo(
          entity.getX(), entity.getY(), entity.getZ(), level.random.nextFloat() * 360.0F, 0.0F);

      // Finalize the spawn and add the entity to the level
      if (entity instanceof Mob mob) {
        if (this.nextSpawnData.getCustomSpawnRules().isEmpty()
                && !mob.checkSpawnRules(level, EntitySpawnReason.SPAWNER)
            || !mob.checkSpawnObstruction(level)) {
          continue;
        }
        mob.finalizeSpawn(
            level,
            level.getCurrentDifficultyAt(mob.blockPosition()),
            EntitySpawnReason.SPAWNER,
            null);
      }
      if (!level.tryAddFreshEntityWithPassengers(entity)) {
        this.delay(level, pos);
        return;
      }

      level.levelEvent(2004, pos, 0);
      if (entity instanceof Mob mob) {
        mob.spawnAnim();
      }

      spawned = true;
    }

    if (spawned) {
      this.delay(level, pos);
    }
  }

  public boolean hasEasyNPC() {
    return this.isEasyNPC;
  }

  public UUID getEasyNPCUUID() {
    return this.easyNPCUUID;
  }

  public UUID getEasyNPCPresetUUID() {
    return this.easyNPCPresetUUID;
  }

  @Override
  public void load(Level level, BlockPos blockPos, ValueInput valueInput) {
    super.load(level, blockPos, valueInput);
    this.displayEntity = null;
    TagValueOutput valueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);
    this.save(valueOutput);
    updateSpawnData(valueOutput.buildResult());
  }

  public void updateSpawnData(CompoundTag compoundTag) {
    // Load spawner configuration from CompoundTag
    this.spawnDelay = SpawnerData.getDelay(compoundTag);
    this.minSpawnDelay = SpawnerData.getMinSpawnDelay(compoundTag);
    this.maxSpawnDelay = SpawnerData.getMaxSpawnDelay(compoundTag);
    this.spawnCount = SpawnerData.getSpawnCount(compoundTag);
    this.maxNearbyEntities = SpawnerData.getMaxNearbyEntities(compoundTag);
    this.requiredPlayerRange = SpawnerData.getRequiredPlayerRange(compoundTag);
    this.spawnRange = SpawnerData.getSpawnRange(compoundTag);
    this.nextSpawnData = SpawnerData.getSpawnData(compoundTag);

    // Reset EasyNPC-specific data
    this.isEasyNPC = false;
    this.easyNPCUUID = null;
    this.easyNPCPresetUUID = null;

    // Extract EasyNPC-specific fields from spawn data
    if (SpawnerData.hasSpawnData(compoundTag)) {
      CompoundTag spawnData = compoundTag.getCompoundOrEmpty(SpawnerData.SPAWN_DATA_TAG);
      if (spawnData.contains("entity")) {
        CompoundTag entityData = spawnData.getCompoundOrEmpty("entity");
        if (entityData.contains("id")) {
          Identifier entityId = Identifier.tryParse(entityData.getString("id").orElse(""));
          this.isEasyNPC = entityId != null && entityId.getNamespace().equals(Constants.MOD_ID);
        }

        if (entityData.contains("UUID")) {
          this.easyNPCUUID = CompoundTagUtils.readUUID(entityData);
        }

        if (entityData.contains(PresetDataCapable.PRESET_UUID_TAG)) {
          this.easyNPCPresetUUID =
              CompoundTagUtils.readUUID(entityData, PresetDataCapable.PRESET_UUID_TAG);
        }
      }
    }
  }
}
