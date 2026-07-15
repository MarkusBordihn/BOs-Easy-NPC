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

package de.markusbordihn.easynpc.data.spawner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.config.SpawnerTypeConfig;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SpawnerDataTest {

  @Test
  @DisplayName("WORLD_SPAWNER id is world_spawner and serializes consistently")
  void worldSpawnerId() {
    assertEquals("world_spawner", SpawnerType.WORLD_SPAWNER.getId());
    assertEquals("world_spawner", SpawnerType.WORLD_SPAWNER.getSerializedName());
    assertTrue(SpawnerType.WORLD_SPAWNER.getDescriptionId().endsWith("world_spawner"));
  }

  @Test
  @DisplayName("fromSpawnerType maps WORLD_SPAWNER to its configured values")
  void fromSpawnerTypeWorld() {
    SpawnerData data = SpawnerData.fromSpawnerType(SpawnerType.WORLD_SPAWNER);

    assertNotNull(data);
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_DELAY, data.spawnDelay());
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_MIN_SPAWN_DELAY, data.minSpawnDelay());
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_MAX_SPAWN_DELAY, data.maxSpawnDelay());
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_SPAWN_COUNT, data.spawnCount());
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_MAX_NEARBY_ENTITIES, data.maxNearbyEntities());
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_REQUIRED_PLAYER_RANGE, data.requiredPlayerRange());
    assertEquals(SpawnerTypeConfig.WORLD_SPAWNER_SPAWN_RANGE, data.spawnRange());
  }

  @Test
  @DisplayName("Every spawner type resolves to non-null spawner data")
  void allTypesResolve() {
    for (SpawnerType type : SpawnerType.values()) {
      assertNotNull(SpawnerData.fromSpawnerType(type), "Missing SpawnerData for " + type);
    }
  }

  @Test
  @DisplayName("null spawner type falls back to SINGLE_SPAWNER data")
  void nullFallsBackToSingle() {
    SpawnerData nullData = SpawnerData.fromSpawnerType(null);
    SpawnerData singleData = SpawnerData.fromSpawnerType(SpawnerType.SINGLE_SPAWNER);

    assertNotNull(nullData);
    assertEquals(singleData, nullData);
  }
}
