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
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.level.storage.ValueInput;

public class BaseEasyNPCSpawner extends BaseSpawner {

  private final SpawnerType spawnerType;
  private boolean isEasyNPC = false;
  private UUID easyNPCPresetUUID;
  private UUID easyNPCUUID;

  public BaseEasyNPCSpawner(SpawnerType spawnerType) {
    super();
    this.spawnerType = spawnerType;
  }

  @Override
  public void setNextSpawnData(Level level, BlockPos blockPos, SpawnData spawnData) {
    super.setNextSpawnData(level, blockPos, spawnData);
    updateEasyNPCData(spawnData.getEntityToSpawn());
  }

  @Override
  public void clientTick(Level level, BlockPos blockPos) {
    if (canSpawnBasedOnConditions(level)) {
      super.clientTick(level, blockPos);
    }
  }

  @Override
  public void serverTick(ServerLevel serverLevel, BlockPos blockPos) {
    if (!canSpawnBasedOnConditions(serverLevel)) {
      return;
    }

    super.serverTick(serverLevel, blockPos);
  }

  private boolean canSpawnBasedOnConditions(Level level) {
    if (!hasEasyNPC()) {
      return false;
    }

    if (easyNPCPresetUUID != null) {
      int entityCount =
          level instanceof ServerLevel serverLevel
              ? LivingEntityManager.getEntityCountByPresetUUID(easyNPCPresetUUID, serverLevel)
              : LivingEntityManager.getEntityCountByPresetUUID(easyNPCPresetUUID);
      int maxNearby = getMaxNearbyEntities();
      if (entityCount >= maxNearby) {
        return false;
      }
      return true;
    }

    if (easyNPCUUID != null) {
      if (level instanceof ServerLevel serverLevel) {
        Entity entity = serverLevel.getEntity(easyNPCUUID);
        if (entity != null && entity.isAlive()) {
          return false;
        }
      } else {
        EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(easyNPCUUID);
        if (easyNPC != null && easyNPC.getLivingEntity().isAlive()) {
          return false;
        }
      }
    }

    return true;
  }

  public boolean hasEasyNPC() {
    return this.isEasyNPC;
  }

  private int getMaxNearbyEntities() {
    TagValueOutput tagValueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);
    save(tagValueOutput);
    return SpawnerData.getMaxNearbyEntities(tagValueOutput.buildResult());
  }

  @Override
  public void load(Level level, BlockPos blockPos, ValueInput valueInput) {
    super.load(level, blockPos, valueInput);

    // Extract entity data from spawn data
    TagValueOutput tagValueOutput = TagValueOutput.createWithoutContext(ProblemReporter.DISCARDING);
    save(tagValueOutput);
    CompoundTag compoundTag = tagValueOutput.buildResult();
    if (SpawnerData.hasSpawnData(compoundTag)) {
      CompoundTag spawnData = compoundTag.getCompoundOrEmpty(SpawnerData.SPAWN_DATA_TAG);
      if (spawnData.contains("entity")) {
        CompoundTag entityData = spawnData.getCompoundOrEmpty("entity");
        updateEasyNPCData(entityData);
      }
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

    if (compoundTag.contains("UUID")) {
      Optional<UUID> uuidOptional = compoundTag.read("UUID", UUIDUtil.CODEC);
      uuidOptional.ifPresent(uuid -> this.easyNPCUUID = uuid);
    }

    if (compoundTag.contains(PresetDataCapable.PRESET_UUID_TAG)) {
      Optional<UUID> presetUuidOptional =
          compoundTag.read(PresetDataCapable.PRESET_UUID_TAG, UUIDUtil.CODEC);
      presetUuidOptional.ifPresent(uuid -> this.easyNPCPresetUUID = uuid);
    }
  }
}
