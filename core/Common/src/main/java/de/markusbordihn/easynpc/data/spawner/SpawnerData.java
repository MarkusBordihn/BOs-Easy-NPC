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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.SpawnerTypeConfig;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.world.level.SpawnData;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnerData {

  public static final String DELAY_TAG = "Delay";
  public static final String MIN_SPAWN_DELAY_TAG = "MinSpawnDelay";
  public static final String MAX_SPAWN_DELAY_TAG = "MaxSpawnDelay";
  public static final String SPAWN_COUNT_TAG = "SpawnCount";
  public static final String MAX_NEARBY_ENTITIES_TAG = "MaxNearbyEntities";
  public static final String REQUIRED_PLAYER_RANGE_TAG = "RequiredPlayerRange";
  public static final String SPAWN_RANGE_TAG = "SpawnRange";
  public static final String SPAWN_DATA_TAG = "SpawnData";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static void setDelay(CompoundTag compoundTag, short spawnDelay) {
    compoundTag.putShort(DELAY_TAG, spawnDelay);
  }

  public static short getDelay(CompoundTag compoundTag) {
    return compoundTag.getShort(DELAY_TAG);
  }

  public static void setMinSpawnDelay(CompoundTag compoundTag, short minSpawnDelay) {
    compoundTag.putShort(MIN_SPAWN_DELAY_TAG, minSpawnDelay);
  }

  public static short getMinSpawnDelay(CompoundTag compoundTag) {
    return compoundTag.getShort(MIN_SPAWN_DELAY_TAG);
  }

  public static boolean hasMinSpawnDelay(CompoundTag compoundTag) {
    return compoundTag.contains(MIN_SPAWN_DELAY_TAG, 99);
  }

  public static void setMaxSpawnDelay(CompoundTag compoundTag, short maxSpawnDelay) {
    compoundTag.putShort(MAX_SPAWN_DELAY_TAG, maxSpawnDelay);
  }

  public static short getMaxSpawnDelay(CompoundTag compoundTag) {
    return compoundTag.getShort(MAX_SPAWN_DELAY_TAG);
  }

  public static void setSpawnCount(CompoundTag compoundTag, short spawnCount) {
    compoundTag.putShort(SPAWN_COUNT_TAG, spawnCount);
  }

  public static short getSpawnCount(CompoundTag compoundTag) {
    return compoundTag.getShort(SPAWN_COUNT_TAG);
  }

  public static void setMaxNearbyEntities(CompoundTag compoundTag, short maxNearbyEntities) {
    compoundTag.putShort(MAX_NEARBY_ENTITIES_TAG, maxNearbyEntities);
  }

  public static short getMaxNearbyEntities(CompoundTag compoundTag) {
    return compoundTag.getShort(MAX_NEARBY_ENTITIES_TAG);
  }

  public static boolean hasMaxNearbyEntities(CompoundTag compoundTag) {
    return compoundTag.contains(MAX_NEARBY_ENTITIES_TAG, 99);
  }

  public static void setRequiredPlayerRange(CompoundTag compoundTag, short requiredPlayerRange) {
    compoundTag.putShort(REQUIRED_PLAYER_RANGE_TAG, requiredPlayerRange);
  }

  public static short getRequiredPlayerRange(CompoundTag compoundTag) {
    return compoundTag.getShort(REQUIRED_PLAYER_RANGE_TAG);
  }

  public static void setSpawnRange(CompoundTag compoundTag, short spawnRange) {
    compoundTag.putShort(SPAWN_RANGE_TAG, spawnRange);
  }

  public static short getSpawnRange(CompoundTag compoundTag) {
    return compoundTag.getShort(SPAWN_RANGE_TAG);
  }

  public static boolean hasSpawnRange(CompoundTag compoundTag) {
    return compoundTag.contains(SPAWN_RANGE_TAG, 99);
  }

  public static SpawnData getSpawnData(CompoundTag compoundTag) {
    if (!hasSpawnData(compoundTag)) {
      return new SpawnData();
    }

    CompoundTag spawnData = compoundTag.getCompound(SPAWN_DATA_TAG);
    return SpawnData.CODEC
        .parse(NbtOps.INSTANCE, spawnData)
        .resultOrPartial((result) -> log.warn("Invalid SpawnData: {}", result))
        .orElseGet(SpawnData::new);
  }

  public static boolean hasSpawnData(CompoundTag compoundTag) {
    return compoundTag.contains(SPAWN_DATA_TAG);
  }

  public static boolean setSpawnerValue(CompoundTag compoundTag, String parameter, short value) {
    switch (parameter) {
      case DELAY_TAG:
        setDelay(compoundTag, value);
        break;
      case MIN_SPAWN_DELAY_TAG:
        setMinSpawnDelay(compoundTag, value);
        break;
      case MAX_SPAWN_DELAY_TAG:
        setMaxSpawnDelay(compoundTag, value);
        break;
      case SPAWN_COUNT_TAG:
        setSpawnCount(compoundTag, value);
        break;
      case MAX_NEARBY_ENTITIES_TAG:
        setMaxNearbyEntities(compoundTag, value);
        break;
      case REQUIRED_PLAYER_RANGE_TAG:
        setRequiredPlayerRange(compoundTag, value);
        break;
      case SPAWN_RANGE_TAG:
        setSpawnRange(compoundTag, value);
        break;
      default:
        return false;
    }
    return true;
  }

  public static void setSpawnData(SpawnerType spawnerType, CompoundTag compoundTag) {
    switch (spawnerType) {
      case BOSS_SPAWNER -> {
        setDelay(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_DELAY);
        setMinSpawnDelay(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_MIN_SPAWN_DELAY);
        setMaxSpawnDelay(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_MAX_SPAWN_DELAY);
        setSpawnCount(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_SPAWN_COUNT);
        setMaxNearbyEntities(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_MAX_NEARBY_ENTITIES);
        setRequiredPlayerRange(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_REQUIRED_PLAYER_RANGE);
        setSpawnRange(compoundTag, SpawnerTypeConfig.BOSS_SPAWNER_SPAWN_RANGE);
      }
      case DEFAULT_SPAWNER -> {
        setDelay(compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_DELAY);
        setMinSpawnDelay(compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_MIN_SPAWN_DELAY);
        setMaxSpawnDelay(compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_MAX_SPAWN_DELAY);
        setSpawnCount(compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_SPAWN_COUNT);
        setMaxNearbyEntities(compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_MAX_NEARBY_ENTITIES);
        setRequiredPlayerRange(
            compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_REQUIRED_PLAYER_RANGE);
        setSpawnRange(compoundTag, SpawnerTypeConfig.DEFAULT_SPAWNER_SPAWN_RANGE);
      }
      case GROUP_SPAWNER -> {
        setDelay(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_DELAY);
        setMinSpawnDelay(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_MIN_SPAWN_DELAY);
        setMaxSpawnDelay(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_MAX_SPAWN_DELAY);
        setSpawnCount(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_SPAWN_COUNT);
        setMaxNearbyEntities(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_MAX_NEARBY_ENTITIES);
        setRequiredPlayerRange(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_REQUIRED_PLAYER_RANGE);
        setSpawnRange(compoundTag, SpawnerTypeConfig.GROUP_SPAWNER_SPAWN_RANGE);
      }
      case SINGLE_SPAWNER -> {
        setDelay(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_DELAY);
        setMinSpawnDelay(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_MIN_SPAWN_DELAY);
        setMaxSpawnDelay(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_MAX_SPAWN_DELAY);
        setSpawnCount(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_SPAWN_COUNT);
        setMaxNearbyEntities(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_MAX_NEARBY_ENTITIES);
        setRequiredPlayerRange(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_REQUIRED_PLAYER_RANGE);
        setSpawnRange(compoundTag, SpawnerTypeConfig.SINGLE_SPAWNER_SPAWN_RANGE);
      }
    }
  }
}
