/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.data.spawner;

import de.markusbordihn.easynpc.config.SpawnerTypeConfig;
import net.minecraft.util.random.SimpleWeightedRandomList;
import net.minecraft.world.level.SpawnData;

public record SpawnerData(
    int spawnDelay,
    int minSpawnDelay,
    int maxSpawnDelay,
    int spawnCount,
    int maxNearbyEntities,
    int requiredPlayerRange,
    int spawnRange) {

  public static SpawnerData fromSpawnerType(SpawnerType spawnerType) {
    SpawnerType type = spawnerType != null ? spawnerType : SpawnerType.SINGLE_SPAWNER;

    return switch (type) {
      case BOSS_SPAWNER ->
          new SpawnerData(
              SpawnerTypeConfig.BOSS_SPAWNER_DELAY,
              SpawnerTypeConfig.BOSS_SPAWNER_MIN_SPAWN_DELAY,
              SpawnerTypeConfig.BOSS_SPAWNER_MAX_SPAWN_DELAY,
              SpawnerTypeConfig.BOSS_SPAWNER_SPAWN_COUNT,
              SpawnerTypeConfig.BOSS_SPAWNER_MAX_NEARBY_ENTITIES,
              SpawnerTypeConfig.BOSS_SPAWNER_REQUIRED_PLAYER_RANGE,
              SpawnerTypeConfig.BOSS_SPAWNER_SPAWN_RANGE);
      case GROUP_SPAWNER ->
          new SpawnerData(
              SpawnerTypeConfig.GROUP_SPAWNER_DELAY,
              SpawnerTypeConfig.GROUP_SPAWNER_MIN_SPAWN_DELAY,
              SpawnerTypeConfig.GROUP_SPAWNER_MAX_SPAWN_DELAY,
              SpawnerTypeConfig.GROUP_SPAWNER_SPAWN_COUNT,
              SpawnerTypeConfig.GROUP_SPAWNER_MAX_NEARBY_ENTITIES,
              SpawnerTypeConfig.GROUP_SPAWNER_REQUIRED_PLAYER_RANGE,
              SpawnerTypeConfig.GROUP_SPAWNER_SPAWN_RANGE);
      case SINGLE_SPAWNER ->
          new SpawnerData(
              SpawnerTypeConfig.SINGLE_SPAWNER_DELAY,
              SpawnerTypeConfig.SINGLE_SPAWNER_MIN_SPAWN_DELAY,
              SpawnerTypeConfig.SINGLE_SPAWNER_MAX_SPAWN_DELAY,
              SpawnerTypeConfig.SINGLE_SPAWNER_SPAWN_COUNT,
              SpawnerTypeConfig.SINGLE_SPAWNER_MAX_NEARBY_ENTITIES,
              SpawnerTypeConfig.SINGLE_SPAWNER_REQUIRED_PLAYER_RANGE,
              SpawnerTypeConfig.SINGLE_SPAWNER_SPAWN_RANGE);
      default -> // DEFAULT_SPAWNER
          new SpawnerData(
              SpawnerTypeConfig.DEFAULT_SPAWNER_DELAY,
              SpawnerTypeConfig.DEFAULT_SPAWNER_MIN_SPAWN_DELAY,
              SpawnerTypeConfig.DEFAULT_SPAWNER_MAX_SPAWN_DELAY,
              SpawnerTypeConfig.DEFAULT_SPAWNER_SPAWN_COUNT,
              SpawnerTypeConfig.DEFAULT_SPAWNER_MAX_NEARBY_ENTITIES,
              SpawnerTypeConfig.DEFAULT_SPAWNER_REQUIRED_PLAYER_RANGE,
              SpawnerTypeConfig.DEFAULT_SPAWNER_SPAWN_RANGE);
    };
  }

  public static SpawnData getOrCreateSpawnData(SpawnData spawnData) {
    return spawnData != null ? spawnData : new SpawnData();
  }

  public static SimpleWeightedRandomList<SpawnData> createSpawnPotentials(SpawnData spawnData) {
    return SimpleWeightedRandomList.single(spawnData);
  }
}
