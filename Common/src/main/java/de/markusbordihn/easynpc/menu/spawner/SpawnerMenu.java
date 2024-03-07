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

package de.markusbordihn.easynpc.menu.spawner;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.block.entity.BaseEasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.menu.slots.PresetSlot;
import net.minecraft.core.BlockPos;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerData;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.SimpleContainerData;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnerMenu extends AbstractContainerMenu {
  public static final int PLAYER_SLOT_START = 9;
  public static final int PLAYER_INVENTORY_SLOT_START = PLAYER_SLOT_START;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  public static final int presetItemSlotX = 42;
  public static final int presetItemSlotY = 169;
  protected static final int containerSize = 1;
  private final Container container;
  private final ContainerData data;

  public SpawnerMenu(int windowId, Inventory playerInventory) {
    this(
        windowId,
        playerInventory,
        new SimpleContainer(containerSize),
        new SimpleContainerData(BaseEasyNPCSpawnerBlockEntity.DATA_SIZE));
  }

  public SpawnerMenu(
      final int windowId,
      final Inventory playerInventory,
      final Container container,
      final ContainerData containerData) {
    this(null, windowId, playerInventory, container, containerData);
  }

  public SpawnerMenu(MenuType<?> menuType, final int windowId, final Inventory playerInventory) {
    this(
        menuType,
        windowId,
        playerInventory,
        new SimpleContainer(containerSize),
        new SimpleContainerData(BaseEasyNPCSpawnerBlockEntity.DATA_SIZE));
  }

  public SpawnerMenu(
      MenuType<?> menuType,
      final int windowId,
      final Inventory playerInventory,
      final Container container,
      final ContainerData containerData) {
    super(menuType, windowId);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(container, containerSize);
    checkContainerDataCount(containerData, BaseEasyNPCSpawnerBlockEntity.DATA_SIZE);
    this.container = container;
    this.data = containerData;

    // Spawner Slot
    this.addSlot(new PresetSlot(container, 0, presetItemSlotX, presetItemSlotY));

    // Slot style
    int slotSize = 18;
    int slotSpacing = 8;

    // Player Inventory Slots
    int playerInventoryStartPositionX = 90;
    int playerInventoryStartPositionY = 153;
    for (int inventoryRow = 0; inventoryRow < 3; ++inventoryRow) {
      for (int inventoryColumn = 0; inventoryColumn < 9; ++inventoryColumn) {
        this.addSlot(
            new Slot(
                playerInventory,
                inventoryColumn + inventoryRow * 9 + PLAYER_INVENTORY_SLOT_START,
                playerInventoryStartPositionX + slotSpacing + inventoryColumn * slotSize,
                playerInventoryStartPositionY + inventoryRow * slotSize));
      }
    }

    // Player Hotbar
    int hotbarStartPositionX = 90;
    int hotbarStartPositionY = 213;
    for (int playerInventorySlot = 0; playerInventorySlot < 9; ++playerInventorySlot) {
      this.addSlot(
          new Slot(
              playerInventory,
              playerInventorySlot,
              hotbarStartPositionX + slotSpacing + playerInventorySlot * slotSize,
              hotbarStartPositionY));
    }

    // Define container data
    this.addDataSlots(containerData);
  }

  @Override
  public boolean stillValid(Player player) {
    return player != null && player.isAlive();
  }

  public BlockPos getSpawnerPosition() {
    return new BlockPos(
        this.data.get(BaseEasyNPCSpawnerBlockEntity.BLOCK_POS_X_DATA),
        this.data.get(BaseEasyNPCSpawnerBlockEntity.BLOCK_POS_Y_DATA),
        this.data.get(BaseEasyNPCSpawnerBlockEntity.BLOCK_POS_Z_DATA));
  }

  public int getSpawnRange() {
    return this.data.get(BaseEasyNPCSpawnerBlockEntity.SPAWN_RANGE_DATA);
  }

  public int getDespawnRange() {
    return this.data.get(BaseEasyNPCSpawnerBlockEntity.DESPAWN_RANGE_DATA);
  }

  public int getRequiredPlayerRange() {
    return this.data.get(BaseEasyNPCSpawnerBlockEntity.REQUIRED_PLAYER_RANGE_DATA);
  }

  public int getDelay() {
    return this.data.get(BaseEasyNPCSpawnerBlockEntity.DELAY_DATA);
  }

  public int getMaxNearbyEntities() {
    return this.data.get(BaseEasyNPCSpawnerBlockEntity.MAX_NEARBY_ENTITIES_DATA);
  }

  public int getSpawnCount() {
    return this.data.get(BaseEasyNPCSpawnerBlockEntity.SPAWN_COUNT_DATA);
  }

  public ItemStack getPresetItem() {
    return this.container.getItem(0);
  }

  @Override
  public ItemStack quickMoveStack(Player player, int slotIndex) {
    Slot slot = this.slots.get(slotIndex);
    if (!slot.hasItem()) {
      return ItemStack.EMPTY;
    }

    ItemStack itemStack = slot.getItem();

    // Store changes if itemStack is not empty.
    if (itemStack.isEmpty()) {
      slot.set(ItemStack.EMPTY);
    } else {
      slot.setChanged();
    }

    return ItemStack.EMPTY;
  }
}
