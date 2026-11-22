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

package de.markusbordihn.easynpc.menu.inventory;

import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.menu.EasyNPCMenu;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;

public class NPCInventoryMenu extends EasyNPCMenu {

  public static final int SLOT_SIZE = 18;
  public static final int COLUMNS = 9;
  public static final int SLOTS_LEFT = 8;
  public static final int NPC_SLOTS_TOP = 18;
  private static final int PLAYER_INVENTORY_GAP = 13;
  private static final int HOTBAR_OFFSET = 58;
  private static final double MIN_MAX_DISTANCE = 8.0D;
  private static final double OPEN_DISTANCE_TOLERANCE = 4.0D;

  private final Container npcInventory;
  private final int playerSlotCount;
  private final double maxDistanceSquared;

  public NPCInventoryMenu(
      final MenuType<?> menuType, final int containerId, final Inventory playerInventory) {
    this(menuType, containerId, playerInventory, new CompoundTag());
  }

  public NPCInventoryMenu(
      final MenuType<?> menuType,
      final int containerId,
      final Inventory playerInventory,
      final CompoundTag data) {
    super(menuType, containerId, playerInventory, data);

    this.playerSlotCount =
        AdditionalScreenData.getPlayerSlotCount(
            this.screenData == null ? null : this.screenData.additionalData());
    this.npcInventory = this.resolveServerNpcInventoryOrClientPlaceholder();
    this.maxDistanceSquared = this.getMaxDistanceSquared();
    this.npcInventory.startOpen(this.player);

    for (int slotIndex = 0; slotIndex < this.playerSlotCount; slotIndex++) {
      this.addSlot(
          new Slot(
              this.npcInventory,
              slotIndex,
              SLOTS_LEFT + slotIndex % COLUMNS * SLOT_SIZE,
              NPC_SLOTS_TOP + slotIndex / COLUMNS * SLOT_SIZE));
    }

    int playerInventoryTop =
        NPC_SLOTS_TOP + getRowCount(this.playerSlotCount) * SLOT_SIZE + PLAYER_INVENTORY_GAP;
    for (int row = 0; row < 3; row++) {
      for (int column = 0; column < COLUMNS; column++) {
        this.addSlot(
            new Slot(
                playerInventory,
                column + row * COLUMNS + COLUMNS,
                SLOTS_LEFT + column * SLOT_SIZE,
                playerInventoryTop + row * SLOT_SIZE));
      }
    }
    for (int column = 0; column < COLUMNS; column++) {
      this.addSlot(
          new Slot(
              playerInventory,
              column,
              SLOTS_LEFT + column * SLOT_SIZE,
              playerInventoryTop + HOTBAR_OFFSET));
    }
  }

  public static int getRowCount(int playerSlotCount) {
    return (playerSlotCount + COLUMNS - 1) / COLUMNS;
  }

  public int getPlayerSlotCount() {
    return this.playerSlotCount;
  }

  private Container resolveServerNpcInventoryOrClientPlaceholder() {
    if (!this.level.isClientSide() && this.easyNPC != null) {
      InventoryDataCapable<?> inventoryData = this.easyNPC.getEasyNPCInventoryData();
      if (inventoryData != null) {
        return inventoryData.getEasyNPCInventory();
      }
    }

    return new SimpleContainer(InventoryDataCapable.INVENTORY_SIZE);
  }

  private double getMaxDistanceSquared() {
    if (this.level.isClientSide() || this.easyNPC == null) {
      return 0;
    }

    double maxDistance =
        Math.max(
            MIN_MAX_DISTANCE,
            Math.sqrt(this.player.distanceToSqr(this.easyNPC.getLivingEntity()))
                + OPEN_DISTANCE_TOLERANCE);
    return maxDistance * maxDistance;
  }

  @Override
  public boolean stillValid(Player player) {
    if (this.level.isClientSide()) {
      return super.stillValid(player);
    }
    if (this.easyNPC == null || this.easyNPC.getEasyNPCInventoryData() == null) {
      return false;
    }

    LivingEntity livingEntity = this.easyNPC.getLivingEntity();
    return super.stillValid(player)
        && !player.isSpectator()
        && livingEntity.isAlive()
        && player.distanceToSqr(livingEntity) <= this.maxDistanceSquared;
  }

  @Override
  public ItemStack quickMoveStack(Player player, int slotIndex) {
    Slot slot = this.slots.get(slotIndex);
    if (!slot.hasItem()) {
      return ItemStack.EMPTY;
    }

    ItemStack itemStack = slot.getItem();
    ItemStack originalItemStack = itemStack.copy();
    boolean moved =
        slotIndex < this.playerSlotCount
            ? this.moveItemStackTo(itemStack, this.playerSlotCount, this.slots.size(), true)
            : this.moveItemStackTo(itemStack, 0, this.playerSlotCount, false);
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
