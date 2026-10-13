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

package de.markusbordihn.easynpc.configui.menu.configuration.inventory;

import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;

public class InventoryConfigurationMenu extends ConfigurationMenu {

  public static final int NPC_INVENTORY_START_POSITION_X = 66;
  public static final int NPC_INVENTORY_START_POSITION_Y = 82;
  public static final int PLAYER_INVENTORY_START_POSITION_X = 66;
  public static final int PLAYER_INVENTORY_START_POSITION_Y = 149;
  public static final int HOTBAR_START_POSITION_Y = 209;
  protected static final int SLOT_SIZE = 18;
  protected static final int COLUMNS = 9;

  protected final Container npcInventory;

  public InventoryConfigurationMenu(
      final MenuType<?> menuType, final int containerId, final Inventory playerInventory) {
    this(menuType, containerId, playerInventory, new CompoundTag());
  }

  public InventoryConfigurationMenu(
      final MenuType<?> menuType,
      final int containerId,
      final Inventory playerInventory,
      final CompoundTag data) {
    super(menuType, containerId, playerInventory, data);

    this.npcInventory = this.resolveServerNpcInventoryOrClientPlaceholder();
    checkContainerSize(this.npcInventory, InventoryDataCapable.INVENTORY_SIZE);
    this.npcInventory.startOpen(playerInventory.player);

    int npcInventoryRows = InventoryDataCapable.INVENTORY_SIZE / COLUMNS;
    for (int row = 0; row < npcInventoryRows; ++row) {
      for (int column = 0; column < COLUMNS; ++column) {
        this.addSlot(
            new Slot(
                this.npcInventory,
                column + row * COLUMNS,
                NPC_INVENTORY_START_POSITION_X + column * SLOT_SIZE,
                NPC_INVENTORY_START_POSITION_Y + row * SLOT_SIZE));
      }
    }

    for (int row = 0; row < 3; ++row) {
      for (int column = 0; column < COLUMNS; ++column) {
        this.addSlot(
            new Slot(
                playerInventory,
                column + row * COLUMNS + COLUMNS,
                PLAYER_INVENTORY_START_POSITION_X + column * SLOT_SIZE,
                PLAYER_INVENTORY_START_POSITION_Y + row * SLOT_SIZE));
      }
    }

    for (int column = 0; column < COLUMNS; ++column) {
      this.addSlot(
          new Slot(
              playerInventory,
              column,
              PLAYER_INVENTORY_START_POSITION_X + column * SLOT_SIZE,
              HOTBAR_START_POSITION_Y));
    }
  }

  private Container resolveServerNpcInventoryOrClientPlaceholder() {
    if (!this.level.isClientSide() && this.getEasyNPC() != null) {
      InventoryDataCapable<?> inventoryData = this.getEasyNPC().getEasyNPCInventoryData();
      if (inventoryData != null) {
        return inventoryData.getEasyNPCInventory();
      }
    }

    return new SimpleContainer(InventoryDataCapable.INVENTORY_SIZE);
  }

  @Override
  public boolean stillValid(Player player) {
    return super.stillValid(player)
        && (this.level.isClientSide()
            || (this.getEasyNPC() != null && this.getEasyNPC().getLivingEntity().isAlive()));
  }

  @Override
  public ItemStack quickMoveStack(Player player, int slotIndex) {
    Slot slot = this.slots.get(slotIndex);
    if (!slot.hasItem()) {
      return ItemStack.EMPTY;
    }

    ItemStack itemStack = slot.getItem();
    ItemStack originalItemStack = itemStack.copy();
    int npcInventorySize = this.npcInventory.getContainerSize();
    boolean moved =
        slotIndex < npcInventorySize
            ? this.moveItemStackTo(itemStack, npcInventorySize, this.slots.size(), true)
            : this.moveItemStackTo(itemStack, 0, npcInventorySize, false);
    if (!moved) {
      return ItemStack.EMPTY;
    }

    if (itemStack.isEmpty()) {
      slot.set(ItemStack.EMPTY);
    } else {
      slot.setChanged();
    }
    return originalItemStack;
  }

  @Override
  public void removed(Player player) {
    super.removed(player);
    this.npcInventory.stopOpen(player);
  }
}
