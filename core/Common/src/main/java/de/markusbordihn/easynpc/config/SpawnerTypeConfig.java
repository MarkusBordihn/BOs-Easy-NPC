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

package de.markusbordihn.easynpc.config;

import java.io.File;
import java.util.Properties;

@SuppressWarnings({"java:S1104", "java:S1444", "java:S3008"})
public class SpawnerTypeConfig extends Config {

  public static final String CONFIG_FILE_NAME = "spawner_type.cfg";
  public static final String CONFIG_FILE_HEADER =
"""
Spawner Type Configuration


""";

  // Boss Spawner
  public static short BOSS_SPAWNER_DELAY = 600;
  public static short BOSS_SPAWNER_MIN_SPAWN_DELAY = 6000;
  public static short BOSS_SPAWNER_MAX_SPAWN_DELAY = 18000;
  public static short BOSS_SPAWNER_SPAWN_COUNT = 1;
  public static short BOSS_SPAWNER_MAX_NEARBY_ENTITIES = 1;
  public static short BOSS_SPAWNER_REQUIRED_PLAYER_RANGE = 32;
  public static short BOSS_SPAWNER_SPAWN_RANGE = 16;

  // Default Spawner
  public static short DEFAULT_SPAWNER_DELAY = 600;
  public static short DEFAULT_SPAWNER_MIN_SPAWN_DELAY = 400;
  public static short DEFAULT_SPAWNER_MAX_SPAWN_DELAY = 1200;
  public static short DEFAULT_SPAWNER_SPAWN_COUNT = 2;
  public static short DEFAULT_SPAWNER_MAX_NEARBY_ENTITIES = 4;
  public static short DEFAULT_SPAWNER_REQUIRED_PLAYER_RANGE = 16;
  public static short DEFAULT_SPAWNER_SPAWN_RANGE = 8;

  // Group Spawner
  public static short GROUP_SPAWNER_DELAY = 600;
  public static short GROUP_SPAWNER_MIN_SPAWN_DELAY = 3000;
  public static short GROUP_SPAWNER_MAX_SPAWN_DELAY = 12000;
  public static short GROUP_SPAWNER_SPAWN_COUNT = 3;
  public static short GROUP_SPAWNER_MAX_NEARBY_ENTITIES = 6;
  public static short GROUP_SPAWNER_REQUIRED_PLAYER_RANGE = 12;
  public static short GROUP_SPAWNER_SPAWN_RANGE = 6;

  // Single Spawner
  public static short SINGLE_SPAWNER_DELAY = 600;
  public static short SINGLE_SPAWNER_MIN_SPAWN_DELAY = 3000;
  public static short SINGLE_SPAWNER_MAX_SPAWN_DELAY = 9000;
  public static short SINGLE_SPAWNER_SPAWN_COUNT = 1;
  public static short SINGLE_SPAWNER_MAX_NEARBY_ENTITIES = 1;
  public static short SINGLE_SPAWNER_REQUIRED_PLAYER_RANGE = 8;
  public static short SINGLE_SPAWNER_SPAWN_RANGE = 4;

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    // Boss Spawner
    BOSS_SPAWNER_DELAY = parseConfigValue(properties, "BossSpawner:Delay", BOSS_SPAWNER_DELAY);
    BOSS_SPAWNER_MIN_SPAWN_DELAY =
        parseConfigValue(properties, "BossSpawner:MinSpawnDelay", BOSS_SPAWNER_MIN_SPAWN_DELAY);
    BOSS_SPAWNER_MAX_SPAWN_DELAY =
        parseConfigValue(properties, "BossSpawner:MaxSpawnDelay", BOSS_SPAWNER_MAX_SPAWN_DELAY);
    BOSS_SPAWNER_SPAWN_COUNT =
        parseConfigValue(properties, "BossSpawner:SpawnCount", BOSS_SPAWNER_SPAWN_COUNT);
    BOSS_SPAWNER_MAX_NEARBY_ENTITIES =
        parseConfigValue(
            properties, "BossSpawner:MaxNearbyEntities", BOSS_SPAWNER_MAX_NEARBY_ENTITIES);
    BOSS_SPAWNER_REQUIRED_PLAYER_RANGE =
        parseConfigValue(
            properties, "BossSpawner:RequiredPlayerRange", BOSS_SPAWNER_REQUIRED_PLAYER_RANGE);
    BOSS_SPAWNER_SPAWN_RANGE =
        parseConfigValue(properties, "BossSpawner:SpawnRange", BOSS_SPAWNER_SPAWN_RANGE);

    // Default Spawner
    DEFAULT_SPAWNER_DELAY =
        parseConfigValue(properties, "DefaultSpawner:Delay", DEFAULT_SPAWNER_DELAY);
    DEFAULT_SPAWNER_MIN_SPAWN_DELAY =
        parseConfigValue(
            properties, "DefaultSpawner:MinSpawnDelay", DEFAULT_SPAWNER_MIN_SPAWN_DELAY);
    DEFAULT_SPAWNER_MAX_SPAWN_DELAY =
        parseConfigValue(
            properties, "DefaultSpawner:MaxSpawnDelay", DEFAULT_SPAWNER_MAX_SPAWN_DELAY);
    DEFAULT_SPAWNER_SPAWN_COUNT =
        parseConfigValue(properties, "DefaultSpawner:SpawnCount", DEFAULT_SPAWNER_SPAWN_COUNT);
    DEFAULT_SPAWNER_MAX_NEARBY_ENTITIES =
        parseConfigValue(
            properties, "DefaultSpawner:MaxNearbyEntities", DEFAULT_SPAWNER_MAX_NEARBY_ENTITIES);
    DEFAULT_SPAWNER_REQUIRED_PLAYER_RANGE =
        parseConfigValue(
            properties,
            "DefaultSpawner:RequiredPlayerRange",
            DEFAULT_SPAWNER_REQUIRED_PLAYER_RANGE);
    DEFAULT_SPAWNER_SPAWN_RANGE =
        parseConfigValue(properties, "DefaultSpawner:SpawnRange", DEFAULT_SPAWNER_SPAWN_RANGE);

    // Group Spawner
    GROUP_SPAWNER_DELAY = parseConfigValue(properties, "GroupSpawner:Delay", GROUP_SPAWNER_DELAY);
    GROUP_SPAWNER_MIN_SPAWN_DELAY =
        parseConfigValue(properties, "GroupSpawner:MinSpawnDelay", GROUP_SPAWNER_MIN_SPAWN_DELAY);
    GROUP_SPAWNER_MAX_SPAWN_DELAY =
        parseConfigValue(properties, "GroupSpawner:MaxSpawnDelay", GROUP_SPAWNER_MAX_SPAWN_DELAY);
    GROUP_SPAWNER_SPAWN_COUNT =
        parseConfigValue(properties, "GroupSpawner:SpawnCount", GROUP_SPAWNER_SPAWN_COUNT);
    GROUP_SPAWNER_MAX_NEARBY_ENTITIES =
        parseConfigValue(
            properties, "GroupSpawner:MaxNearbyEntities", GROUP_SPAWNER_MAX_NEARBY_ENTITIES);
    GROUP_SPAWNER_REQUIRED_PLAYER_RANGE =
        parseConfigValue(
            properties, "GroupSpawner:RequiredPlayerRange", GROUP_SPAWNER_REQUIRED_PLAYER_RANGE);
    GROUP_SPAWNER_SPAWN_RANGE =
        parseConfigValue(properties, "GroupSpawner:SpawnRange", GROUP_SPAWNER_SPAWN_RANGE);

    // Single Spawner
    SINGLE_SPAWNER_DELAY =
        parseConfigValue(properties, "SingleSpawner:Delay", SINGLE_SPAWNER_DELAY);
    SINGLE_SPAWNER_MIN_SPAWN_DELAY =
        parseConfigValue(properties, "SingleSpawner:MinSpawnDelay", SINGLE_SPAWNER_MIN_SPAWN_DELAY);
    SINGLE_SPAWNER_MAX_SPAWN_DELAY =
        parseConfigValue(properties, "SingleSpawner:MaxSpawnDelay", SINGLE_SPAWNER_MAX_SPAWN_DELAY);
    SINGLE_SPAWNER_SPAWN_COUNT =
        parseConfigValue(properties, "SingleSpawner:SpawnCount", SINGLE_SPAWNER_SPAWN_COUNT);
    SINGLE_SPAWNER_MAX_NEARBY_ENTITIES =
        parseConfigValue(
            properties, "SingleSpawner:MaxNearbyEntities", SINGLE_SPAWNER_MAX_NEARBY_ENTITIES);
    SINGLE_SPAWNER_REQUIRED_PLAYER_RANGE =
        parseConfigValue(
            properties, "SingleSpawner:RequiredPlayerRange", SINGLE_SPAWNER_REQUIRED_PLAYER_RANGE);
    SINGLE_SPAWNER_SPAWN_RANGE =
        parseConfigValue(properties, "SingleSpawner:SpawnRange", SINGLE_SPAWNER_SPAWN_RANGE);

    // Update config file if needed
    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }
}
