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

package de.markusbordihn.easynpc.access;

import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;

public interface SpawnerAccessHelper {

  void setSpawnDataDirect(Level level, BlockPos blockPos, SpawnData spawnData);

  SpawnData getSpawnDataDirect();

  void initializeSpawnerData(SpawnerType spawnerType, SpawnData spawnData);

  SpawnerType getSpawnerType();

  int getMaxNearbyEntities();

  void setMaxNearbyEntities(int value);

  int getSpawnDelay();

  void setSpawnDelay(int value);

  int getMinSpawnDelay();

  void setMinSpawnDelay(int value);

  int getMaxSpawnDelay();

  void setMaxSpawnDelay(int value);

  int getSpawnCount();

  void setSpawnCount(int value);

  int getRequiredPlayerRange();

  void setRequiredPlayerRange(int value);

  int getSpawnRange();

  void setSpawnRange(int value);
}
