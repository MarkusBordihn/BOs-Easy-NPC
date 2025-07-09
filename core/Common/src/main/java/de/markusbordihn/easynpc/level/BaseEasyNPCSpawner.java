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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import java.util.Optional;
import java.util.Random;
import java.util.UUID;
import java.util.function.Function;
import net.minecraft.core.BlockPos;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BaseEasyNPCSpawner extends BaseSpawner {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final Random random = new Random();
  private final SpawnerType spawnerType;
  private boolean isEasyNPC = false;
  private double oSpin;
  private double spin;
  private Entity displayEntity;
  private int maxNearbyEntities = 6;
  private int maxSpawnDelay = 800;
  private int minSpawnDelay = 200;
  private int requiredPlayerRange = 16;
  private int spawnCount = 4;
  private int spawnDelay = 20;
  private int spawnRange = 4;
  private ResourceLocation entityResourceLocation;
  private SpawnData nextSpawnData;
  private UUID easyNPCPresetUUID;
  private UUID easyNPCUUID;

  public BaseEasyNPCSpawner(SpawnerType spawnerType) {
    super();
    this.spawnerType = spawnerType;
    CompoundTag spawnerData = this.save(new CompoundTag());
    SpawnerData.setSpawnData(spawnerType, spawnerData);
    updateSpawnData(spawnerData);
    this.load(null, null, spawnerData);
  }

  @Override
  public void broadcastEvent(Level level, BlockPos blockPos, int eventId) {
    level.blockEvent(blockPos, Blocks.SPAWNER, eventId, 0);
  }

  @Override
  public void setNextSpawnData(Level level, BlockPos blockPos, SpawnData spawnData) {
    super.setNextSpawnData(level, blockPos, spawnData);
    if (level != null) {
      BlockState blockState = level.getBlockState(blockPos);
      level.sendBlockUpdated(blockPos, blockState, blockState, 4);
    }
    updateSpawnData(this.save(new CompoundTag()));
  }

  @Override
  public void clientTick(Level level, BlockPos blockPos) {
    // Check if we have a valid EasyNPC entity
    if (!hasEasyNPC()) {
      return;
    }

    if (!this.isNearPlayer(level, blockPos, this.requiredPlayerRange)) {
      this.oSpin = this.spin;
      return;
    }

    if (this.maxNearbyEntities == 1) {
      EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(this.easyNPCUUID);
      if (easyNPC != null && easyNPC.getEntity().isAlive()) {
        this.oSpin = this.spin;
        return;
      }
    }

    if (this.easyNPCPresetUUID != null
        && LivingEntityManager.getEntityCountByPresetUUID(this.easyNPCPresetUUID)
            >= this.maxNearbyEntities) {
      this.oSpin = this.spin;
      return;
    }

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

  @Override
  public void serverTick(ServerLevel serverLevel, BlockPos blockPos) {
    // Check if we have a valid EasyNPC entity
    if (!hasEasyNPC()) {
      return;
    }

    // Check if we are near a player
    if (this.requiredPlayerRange > 0
        && !this.isNearPlayer(serverLevel, blockPos, this.requiredPlayerRange)) {
      return;
    }

    // Handle spawn delay
    if (this.spawnDelay <= 0) {
      this.delay(serverLevel, blockPos);
    } else {
      --this.spawnDelay;
      return;
    }

    // If we have a single entity, and it is still alive, we don't need to spawn a new one.
    if (this.maxNearbyEntities == 1 && this.easyNPCUUID != null) {
      Entity entity = serverLevel.getEntity(this.easyNPCUUID);
      if (entity != null && entity.isAlive()) {
        this.delay(serverLevel, blockPos);
        return;
      }
    }

    // If we have a multiple entities, we need to check the entities based on the preset UUID.
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
      this.displayEntity =
          EntityType.loadEntityRecursive(
              this.nextSpawnData.getEntityToSpawn(), level, Function.identity());
    }

    return this.displayEntity;
  }

  @Override
  public double getSpin() {
    return this.spin;
  }

  @Override
  public double getoSpin() {
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
      Optional<EntityType<?>> entityType = EntityType.by(entityTag);
      if (entityType.isEmpty()) {
        this.delay(level, pos);
        return;
      }

      // Make sure UUID is removed if spawn of multiple entities is allowed.
      if (this.maxNearbyEntities > 1 && entityTag.contains("UUID")) {
        entityTag.remove("UUID");
      }

      // Use the provided position or calculate a new one
      ListTag posList = entityTag.getList("Pos", 6);
      int posSize = posList.size();
      double x =
          posSize >= 1
              ? posList.getDouble(0)
              : pos.getX()
                  + (level.random.nextDouble() - level.random.nextDouble()) * this.spawnRange
                  + 0.5;
      double y = posSize >= 2 ? posList.getDouble(1) : pos.getY() + level.random.nextInt(3) - 1;
      double z =
          posSize >= 3
              ? posList.getDouble(2)
              : pos.getZ()
                  + (level.random.nextDouble() - level.random.nextDouble()) * this.spawnRange
                  + 0.5;

      // Check if the entity can be spawned at the given position or find an alternative position.
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
              loadedEntity -> {
                loadedEntity.moveTo(
                    finalX, finalY, finalZ, loadedEntity.getYRot(), loadedEntity.getXRot());
                return loadedEntity;
              });
      if (entity == null) {
        this.delay(level, pos);
        return;
      }
      entity.moveTo(
          entity.getX(), entity.getY(), entity.getZ(), level.random.nextFloat() * 360.0F, 0.0F);

      // Finalize the spawn and add the entity to the level
      if (entity instanceof Mob mob) {
        if (this.nextSpawnData.getCustomSpawnRules().isEmpty()
                && !mob.checkSpawnRules(level, MobSpawnType.SPAWNER)
            || !mob.checkSpawnObstruction(level)) {
          continue;
        }
        mob.finalizeSpawn(
            level, level.getCurrentDifficultyAt(mob.blockPosition()), MobSpawnType.SPAWNER, null);
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
  public void load(Level level, BlockPos blockPos, CompoundTag compoundTag) {
    super.load(level, blockPos, compoundTag);
    updateSpawnData(compoundTag);
  }

  public void updateSpawnData(CompoundTag compoundTag) {
    this.spawnDelay = SpawnerData.getDelay(compoundTag);
    this.minSpawnDelay = SpawnerData.getMinSpawnDelay(compoundTag);
    this.maxSpawnDelay = SpawnerData.getMaxSpawnDelay(compoundTag);
    this.spawnCount = SpawnerData.getSpawnCount(compoundTag);
    this.maxNearbyEntities = SpawnerData.getMaxNearbyEntities(compoundTag);
    this.requiredPlayerRange = SpawnerData.getRequiredPlayerRange(compoundTag);
    this.spawnRange = SpawnerData.getSpawnRange(compoundTag);
    this.nextSpawnData = SpawnerData.getSpawnData(compoundTag);

    // Reset easy NPC specific data
    this.isEasyNPC = false;
    this.easyNPCUUID = null;
    this.easyNPCPresetUUID = null;

    if (SpawnerData.hasSpawnData(compoundTag)) {
      CompoundTag spawnData = compoundTag.getCompound(SpawnerData.SPAWN_DATA_TAG);
      if (spawnData.contains("entity")) {
        CompoundTag entityData = spawnData.getCompound("entity");

        if (entityData.contains("id")) {
          this.entityResourceLocation = ResourceLocation.tryParse(entityData.getString("id"));
          this.isEasyNPC = this.entityResourceLocation.getNamespace().equals(Constants.MOD_ID);
        }

        if (entityData.contains("UUID")) {
          this.easyNPCUUID = entityData.getUUID("UUID");
        }

        if (entityData.contains(PresetDataCapable.PRESET_UUID_TAG)) {
          this.easyNPCPresetUUID = entityData.getUUID(PresetDataCapable.PRESET_UUID_TAG);
        }
      }
    }
  }
}
