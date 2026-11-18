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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.data.preset.PresetData;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.block.Block;

public class SpawnerBlockEntityTestHelper {

  private static final BlockPos SPAWNER_POSITION = new BlockPos(1, 1, 1);
  private static final String MALFORMED_UUID = "not-a-uuid";
  private static final String SPAWN_DATA_TAG = "SpawnData";
  private static final String SPAWN_DATA_ENTITY_TAG = "entity";

  private SpawnerBlockEntityTestHelper() {}

  public static void assertMalformedUUIDTagsKeepSpawnerLoadable(
      GameTestHelper helper, Block spawnerBlock, EntityType<?> entityType) {
    helper.setBlock(SPAWNER_POSITION, spawnerBlock);
    if (!(helper.getBlockEntity(SPAWNER_POSITION)
        instanceof EasyNPCSpawnerBlockEntity spawnerBlockEntity)) {
      helper.fail("The spawner block must have a spawner block entity");
      return;
    }

    CompoundTag spawnerTag = spawnerBlockEntity.saveWithFullMetadata();
    spawnerTag.putString(EasyNPCSpawnerBlockEntity.UUID_TAG, MALFORMED_UUID);
    spawnerTag.putIntArray(EasyNPCSpawnerBlockEntity.SPAWNER_OWNER_TAG, new int[] {1, 2});
    CompoundTag entityTag = new CompoundTag();
    entityTag.putString(Entity.ID_TAG, EntityType.getKey(entityType).toString());
    entityTag.putString(Entity.UUID_TAG, MALFORMED_UUID);
    entityTag.putString(PresetData.PRESET_UUID_TAG, MALFORMED_UUID);
    CompoundTag spawnDataTag = new CompoundTag();
    spawnDataTag.put(SPAWN_DATA_ENTITY_TAG, entityTag);
    spawnerTag.put(SPAWN_DATA_TAG, spawnDataTag);

    spawnerBlockEntity.load(spawnerTag);

    GameTestHelpers.assertNotNull(
        helper,
        "A malformed spawner UUID must be replaced by a new one",
        spawnerBlockEntity.getSpawnerUUID());
    GameTestHelpers.assertTrue(
        helper,
        "A malformed owner UUID must be read as no owner",
        spawnerBlockEntity.getOwner() == null);
    GameTestHelpers.assertTrue(
        helper,
        "The reloaded spawner must save a valid UUID",
        spawnerBlockEntity.saveWithoutMetadata().hasUUID(EasyNPCSpawnerBlockEntity.UUID_TAG));
  }
}
