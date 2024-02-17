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

package de.markusbordihn.easynpc.block.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.block.BaseEasyNPCSpawnerBlock;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetItem;
import java.util.Objects;
import java.util.Random;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.core.BlockPos.MutableBlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.ContainerHelper;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BaseContainerBlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class BaseEasyNPCSpawnerBlockEntity extends BaseContainerBlockEntity {

  public static final String NAME = "easy_npc_spawner_entity";
  public static final int BLOCK_POS_X_DATA = 0;
  public static final int BLOCK_POS_Y_DATA = 1;
  public static final int BLOCK_POS_Z_DATA = 2;
  public static final int SPAWN_RANGE_DATA = 3;
  public static final int REQUIRED_PLAYER_RANGE_DATA = 4;
  public static final int DELAY_DATA = 5;
  public static final int MAX_NEARBY_ENTITIES_DATA = 6;
  public static final int SPAWN_COUNT_DATA = 7;
  public static final int DATA_SIZE = 8;
  public static final String UUID_TAG = "UUID";
  public static final String SPAWN_RANGE_TAG = "SpawnRange";
  public static final String REQUIRED_PLAYER_RANGE_TAG = "RequiredPlayerRange";
  public static final String SPAWNER_OWNER_TAG = "Owner";
  public static final String DELAY_TAG = "Delay";
  public static final String MAX_NEARBY_ENTITIES_TAG = "MaxNearbyEntities";
  public static final String SPAWN_COUNT_TAG = "SpawnCount";
  public static final int SPAWNER_TICK = 20;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected final Random random = new Random();
  protected final NonNullList<ItemStack> items = NonNullList.withSize(8, ItemStack.EMPTY);
  private int spawnerTicker = this.random.nextInt(SPAWNER_TICK);
  private int spawnTicker = 0;
  private int spawnRange = 2;
  private int requiredPlayerRange = 16;
  private UUID owner;
  private UUID spawnerUUID;
  private int delay = 10;
  private int maxNearbyEntities = 1;
  private int numbersPerSpawnInterval = 1;
  protected final ContainerData dataAccess =
      new ContainerData() {

        public int get(int index) {
          return switch (index) {
            case BLOCK_POS_X_DATA -> worldPosition.getX();
            case BLOCK_POS_Y_DATA -> worldPosition.getY();
            case BLOCK_POS_Z_DATA -> worldPosition.getZ();
            case SPAWN_RANGE_DATA -> spawnRange;
            case REQUIRED_PLAYER_RANGE_DATA -> requiredPlayerRange;
            case DELAY_DATA -> delay;
            case MAX_NEARBY_ENTITIES_DATA -> maxNearbyEntities;
            case SPAWN_COUNT_DATA -> numbersPerSpawnInterval;
            default -> 0;
          };
        }

        public void set(int index, int value) {
          switch (index) {
            case SPAWN_RANGE_DATA:
              spawnRange = value;
              break;
            case REQUIRED_PLAYER_RANGE_DATA:
              requiredPlayerRange = value;
              break;
            case DELAY_DATA:
              delay = value;
              break;
            case MAX_NEARBY_ENTITIES_DATA:
              maxNearbyEntities = value;
              break;
            case SPAWN_COUNT_DATA:
              numbersPerSpawnInterval = value;
              break;
            default:
          }
        }

        @Override
        public int getCount() {
          return DATA_SIZE;
        }
      };

  public BaseEasyNPCSpawnerBlockEntity(
      BlockEntityType<?> blockEntityType, BlockPos blockPos, BlockState blockState) {
    super(blockEntityType, blockPos, blockState);
  }

  public static void clientTick(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      BaseEasyNPCSpawnerBlockEntity blockEntity) {
    // Do nothing on client side, so far.
  }

  public static void serverTick(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      BaseEasyNPCSpawnerBlockEntity blockEntity) {
    if (blockEntity.spawnerTicker++ >= SPAWNER_TICK) {
      if (blockEntity.spawnTicker++ >= blockEntity.delay) {
        spawnTick(level, blockPos, blockState, blockEntity);
        blockEntity.spawnTicker = 0;
      }
      blockEntity.spawnerTicker = 0;
    }
  }

  public static void spawnTick(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      BaseEasyNPCSpawnerBlockEntity blockEntity) {
    if (!blockEntity.checkSpawnConditions()) {
      return;
    }
    log.debug("Spawn tick for {}", blockEntity);

    // Check which side of the spawner is free to adjust the spawn block position, if needed.
    // We check the north, south, east and west side of the spawner and if all sides are blocked
    // we will try to spawn the entity one or more blocks above the spawner.
    BlockPos spawnerBlockPos = getFreeSpawnerBlockPos(blockPos, level);

    // Copy preset item stack and set spawner UUID.
    ItemStack presetItemStack = blockEntity.getPresetItemStack().copy();
    EasyNPCPresetItem.setSpawnerUUID(presetItemStack, blockEntity.getSpawnerUUID());

    // Find next free position in x and z direction and spawn entity.
    int numbersPerSpawnInterval = blockEntity.getSpawnCount();
    Iterable<MutableBlockPos> possibleSpawnPositions =
        BlockPos.spiralAround(
            spawnerBlockPos, blockEntity.getSpawnRange(), Direction.NORTH, Direction.EAST);
    for (MutableBlockPos spawnBlockPos : possibleSpawnPositions) {
      AABB aabb = new AABB(spawnBlockPos).inflate(0.1);
      BlockPos targetBlockPos =
          new BlockPos(
              spawnBlockPos.getX() + 0.5f, spawnBlockPos.getY(), spawnBlockPos.getZ() + 0.5f);
      if (level.getBlockState(targetBlockPos.above()).isAir()
          && !(level.getBlockState(targetBlockPos).getBlock() instanceof BaseEasyNPCSpawnerBlock)
          && level.getEntitiesOfClass(LivingEntity.class, aabb).isEmpty()
          && EasyNPCPresetItem.spawnAtPosition(spawnBlockPos, presetItemStack, level)) {

        // Check if we have a numbers per spawn interval and reduce the number of spawns.
        if (blockEntity.getSpawnCount() > 0) {
          numbersPerSpawnInterval -= 1;
          if (numbersPerSpawnInterval <= 0) {
            break;
          }
          log.info(
              "Spawned {} ({} / {}) at {} with in {}",
              presetItemStack,
              numbersPerSpawnInterval,
              blockEntity.getSpawnCount(),
              spawnBlockPos,
              level);
        } else {
          log.info("Spawned {} at {} with in {}", presetItemStack, spawnBlockPos, level);
        }
      }
    }
  }

  public static BlockPos getFreeSpawnerBlockPos(BlockPos blockPos, Level level) {
    for (int yExpand = 0; yExpand < 3; yExpand++) {
      if (level.getBlockState(blockPos.above(yExpand).north()).isAir()
          || level.getBlockState(blockPos.above(yExpand).south()).isAir()
          || level.getBlockState(blockPos.above(yExpand).east()).isAir()
          || level.getBlockState(blockPos.above(yExpand).west()).isAir()) {
        return blockPos.above(yExpand);
      }
    }
    return blockPos;
  }

  public boolean checkSpawnConditions() {
    // Check if present slot is not empty.
    if (getPresetItemStack() == null) {
      return false;
    }

    // Check if player range is greater than 0 and check if any player is in range.
    if (this.requiredPlayerRange > 0) {
      Level level = this.getLevel();
      if (level != null) {
        boolean isPlayerInRange = false;
        for (Player player : level.players()) {
          if (player.distanceToSqr(
                  this.worldPosition.getX(), this.worldPosition.getY(), this.worldPosition.getZ())
              <= this.requiredPlayerRange * this.requiredPlayerRange) {
            isPlayerInRange = true;
            break;
          }
        }
        if (!isPlayerInRange) {
          return false;
        }
      }
    }

    // Check if max nearby entities is greater than 0 and check if any entity is in range.
    if (this.maxNearbyEntities > 0) {
      Level level = this.getLevel();
      if (level != null) {
        AABB aabb = new AABB(this.worldPosition).inflate(this.spawnRange);
        int nearbyEntities = 0;
        for (LivingEntity livingEntity : level.getEntitiesOfClass(LivingEntity.class, aabb)) {
          if (livingEntity instanceof EasyNPC<?> easyNPC
              && easyNPC.getEasyNPCSpawnerData().hasSpawnerUUID()
              && easyNPC.getEasyNPCSpawnerData().getSpawnerUUID().equals(this.spawnerUUID)) {
            nearbyEntities++;
          }
        }
        if (nearbyEntities >= this.maxNearbyEntities) {
          return false;
        }
      }
    }

    return true;
  }

  public ItemStack getPresetItemStack() {
    if (this.items.isEmpty()
        || this.items.get(0).isEmpty() && !EasyNPCPresetItem.hasPreset(this.items.get(0))) {
      return null;
    }
    return this.items.get(0);
  }

  public UUID getSpawnerUUID() {
    return this.spawnerUUID;
  }

  public void setSpawnerUUID(UUID spawnerUUID) {
    this.spawnerUUID = spawnerUUID;
    this.setChanged();
  }

  public UUID getOwner() {
    return this.owner;
  }

  public void setOwner(LivingEntity livingEntity) {
    this.owner = livingEntity.getUUID();
    this.setChanged();
  }

  @Override
  protected Component getDefaultName() {
    return Component.translatable(Constants.CONTAINER_PREFIX + NAME);
  }

  @Override
  protected AbstractContainerMenu createMenu(int windowId, Inventory inventory) {
    return null;
  }

  public int getSpawnRange() {
    return this.dataAccess.get(SPAWN_RANGE_DATA);
  }

  public void setSpawnRange(int spawnRange) {
    this.dataAccess.set(SPAWN_RANGE_DATA, Math.max(0, Math.min(64, spawnRange)));
    this.setChanged();
  }

  public int getRequiredPlayerRange() {
    return this.dataAccess.get(REQUIRED_PLAYER_RANGE_DATA);
  }

  public void setRequiredPlayerRange(int requiredPlayerRange) {
    this.dataAccess.set(REQUIRED_PLAYER_RANGE_DATA, Math.max(0, Math.min(64, requiredPlayerRange)));
    this.setChanged();
  }

  public int getDelay() {
    return this.dataAccess.get(DELAY_DATA);
  }

  public void setDelay(int delay) {
    this.dataAccess.set(DELAY_DATA, Math.max(0, Math.min(3600, delay)));
    this.setChanged();
  }

  public int getMaxNearbyEntities() {
    return this.dataAccess.get(MAX_NEARBY_ENTITIES_DATA);
  }

  public void setMaxNearbyEntities(int maxNearbyEntities) {
    this.dataAccess.set(MAX_NEARBY_ENTITIES_DATA, Math.max(0, Math.min(256, maxNearbyEntities)));
    this.setChanged();
  }

  public int getSpawnCount() {
    return this.dataAccess.get(SPAWN_COUNT_DATA);
  }

  public void setSpawnCount(int spawnCount) {
    this.dataAccess.set(SPAWN_COUNT_DATA, Math.max(0, Math.min(32, spawnCount)));
    this.setChanged();
  }

  @Override
  public int getContainerSize() {
    return this.items.size();
  }

  @Override
  public boolean isEmpty() {
    for (ItemStack itemStack : this.items) {
      if (!itemStack.isEmpty()) {
        return false;
      }
    }
    return true;
  }

  @Override
  public ItemStack getItem(int index) {
    return this.items.get(index);
  }

  @Override
  public ItemStack removeItem(int index, int count) {
    return ContainerHelper.removeItem(this.items, index, count);
  }

  @Override
  public ItemStack removeItemNoUpdate(int index) {
    return ContainerHelper.takeItem(this.items, index);
  }

  @Override
  public void setItem(int index, ItemStack itemStack) {
    ItemStack itemStackFromIndex = this.items.get(index);
    if (itemStack.sameItem(itemStackFromIndex)) {
      return;
    }
    this.items.set(index, itemStack);
  }

  @Override
  public boolean stillValid(Player player) {
    Level level = this.level;
    return (level != null && level.getBlockEntity(this.worldPosition) == this)
        && player.distanceToSqr(
                this.worldPosition.getX() + 0.5D,
                this.worldPosition.getY() + 0.5D,
                this.worldPosition.getZ() + 0.5D)
            <= 64.0D;
  }

  @Override
  public void clearContent() {
    this.items.clear();
  }

  @Override
  public void load(CompoundTag compoundTag) {
    super.load(compoundTag);
    this.spawnerUUID = compoundTag.getUUID(UUID_TAG);
    this.owner = compoundTag.getUUID(SPAWNER_OWNER_TAG);
    this.spawnRange = compoundTag.getInt(SPAWN_RANGE_TAG);
    this.requiredPlayerRange = compoundTag.getInt(REQUIRED_PLAYER_RANGE_TAG);
    this.delay = compoundTag.getInt(DELAY_TAG);
    this.maxNearbyEntities = compoundTag.getInt(MAX_NEARBY_ENTITIES_TAG);
    this.numbersPerSpawnInterval = compoundTag.getInt(SPAWN_COUNT_TAG);

    // Load slot items
    this.items.clear();
    ContainerHelper.loadAllItems(compoundTag, this.items);
  }

  @Override
  public void saveAdditional(CompoundTag compoundTag) {
    super.saveAdditional(compoundTag);
    compoundTag.putUUID(
        UUID_TAG, Objects.requireNonNullElseGet(this.spawnerUUID, UUID::randomUUID));
    if (this.owner != null) {
      compoundTag.putUUID(SPAWNER_OWNER_TAG, this.owner);
    }
    compoundTag.putInt(SPAWN_RANGE_TAG, this.spawnRange);
    compoundTag.putInt(REQUIRED_PLAYER_RANGE_TAG, this.requiredPlayerRange);
    compoundTag.putInt(DELAY_TAG, this.delay);
    compoundTag.putInt(MAX_NEARBY_ENTITIES_TAG, this.maxNearbyEntities);
    compoundTag.putInt(SPAWN_COUNT_TAG, this.numbersPerSpawnInterval);

    // Save slot items
    ContainerHelper.saveAllItems(compoundTag, this.items);
  }
}
