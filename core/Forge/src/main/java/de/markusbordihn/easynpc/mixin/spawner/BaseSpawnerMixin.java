/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.mixin.spawner;

import de.markusbordihn.easynpc.access.SpawnerAccessHelper;
import de.markusbordihn.easynpc.data.spawner.SpawnerData;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import net.minecraft.core.BlockPos;
import net.minecraft.util.random.SimpleWeightedRandomList;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;

@Mixin(BaseSpawner.class)
public abstract class BaseSpawnerMixin implements SpawnerAccessHelper {

  @Shadow private SpawnData nextSpawnData;

  @Shadow private int spawnDelay;

  @Shadow private int minSpawnDelay;

  @Shadow private int maxSpawnDelay;

  @Shadow private int spawnCount;

  @Shadow private int maxNearbyEntities;

  @Shadow private int requiredPlayerRange;

  @Shadow private int spawnRange;

  @Shadow private SimpleWeightedRandomList<SpawnData> spawnPotentials;

  @Unique private SpawnerType easyNPC$spawnerType;

  @Shadow
  protected abstract void setNextSpawnData(Level level, BlockPos blockPos, SpawnData spawnData);

  @Override
  public void setSpawnDataDirect(Level level, BlockPos blockPos, SpawnData spawnData) {
    this.setNextSpawnData(level, blockPos, spawnData);
  }

  @Override
  public SpawnData getSpawnDataDirect() {
    return this.nextSpawnData;
  }

  @Override
  public void initializeSpawnerData(SpawnerType spawnerType, SpawnData spawnData) {
    this.easyNPC$spawnerType = spawnerType != null ? spawnerType : SpawnerType.SINGLE_SPAWNER;

    SpawnerData config = SpawnerData.fromSpawnerType(this.easyNPC$spawnerType);

    this.spawnDelay = config.spawnDelay();
    this.minSpawnDelay = config.minSpawnDelay();
    this.maxSpawnDelay = config.maxSpawnDelay();
    this.spawnCount = config.spawnCount();
    this.maxNearbyEntities = config.maxNearbyEntities();
    this.requiredPlayerRange = config.requiredPlayerRange();
    this.spawnRange = config.spawnRange();

    this.nextSpawnData = SpawnerData.getOrCreateSpawnData(spawnData);
    this.spawnPotentials = SpawnerData.createSpawnPotentials(this.nextSpawnData);
  }

  @Override
  public SpawnerType getSpawnerType() {
    return this.easyNPC$spawnerType;
  }

  @Override
  public int getMaxNearbyEntities() {
    return this.maxNearbyEntities;
  }

  @Override
  public void setMaxNearbyEntities(int value) {
    this.maxNearbyEntities = value;
  }

  @Override
  public int getSpawnDelay() {
    return this.spawnDelay;
  }

  @Override
  public void setSpawnDelay(int value) {
    this.spawnDelay = value;
  }

  @Override
  public int getMinSpawnDelay() {
    return this.minSpawnDelay;
  }

  @Override
  public void setMinSpawnDelay(int value) {
    this.minSpawnDelay = value;
  }

  @Override
  public int getMaxSpawnDelay() {
    return this.maxSpawnDelay;
  }

  @Override
  public void setMaxSpawnDelay(int value) {
    this.maxSpawnDelay = value;
  }

  @Override
  public int getSpawnCount() {
    return this.spawnCount;
  }

  @Override
  public void setSpawnCount(int value) {
    this.spawnCount = value;
  }

  @Override
  public int getRequiredPlayerRange() {
    return this.requiredPlayerRange;
  }

  @Override
  public void setRequiredPlayerRange(int value) {
    this.requiredPlayerRange = value;
  }

  @Override
  public int getSpawnRange() {
    return this.spawnRange;
  }

  @Override
  public void setSpawnRange(int value) {
    this.spawnRange = value;
  }
}
