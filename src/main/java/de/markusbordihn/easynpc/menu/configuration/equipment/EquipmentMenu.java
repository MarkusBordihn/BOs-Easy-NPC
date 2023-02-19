/**
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.menu.configuration.equipment;

import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.slots.ArmorSlot;
import de.markusbordihn.easynpc.menu.slots.DummySlot;
import de.markusbordihn.easynpc.menu.slots.HandSlot;

public class EquipmentMenu extends ConfigurationMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Defining basic layout options
  protected static int armorContainerSize = 4;
  protected static int equipmentContainerSize = 8;
  protected static int handContainerSize = 2;
  protected static int inventoryContainerSize = 16;
  protected static int slotSize = 18;

  // Define containers
  protected final Container armorContainer;
  protected final Container equipmentContainer;
  protected final Container handContainer;
  protected final Container inventoryContainer;

  // Cache
  protected final Level level;
  protected final Player player;

  public EquipmentMenu(int windowId, Inventory playerInventory, UUID uuid) {
    this(ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU.get(), windowId, playerInventory, uuid);
  }

  public EquipmentMenu(int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID());
  }

  public EquipmentMenu(final MenuType<?> menuType, int windowId, Inventory inventory,
      UUID uuid) {
    this(menuType, windowId, inventory, new SimpleContainer(armorContainerSize),
        new SimpleContainer(equipmentContainerSize), new SimpleContainer(handContainerSize),
        new SimpleContainer(inventoryContainerSize), uuid);
  }

  public EquipmentMenu(final MenuType<?> menuType, final int windowId,
      final Inventory playerInventory, final Container armorContainer,
      final Container equipmentContainer, final Container handContainer,
      final Container inventoryContainer, UUID uuid) {
    super(menuType, windowId, playerInventory, uuid);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(armorContainer, armorContainerSize);
    checkContainerSize(equipmentContainer, equipmentContainerSize);
    checkContainerSize(handContainer, handContainerSize);
    checkContainerSize(inventoryContainer, inventoryContainerSize);

    // Container
    this.armorContainer = armorContainer;
    this.equipmentContainer = equipmentContainer;
    this.handContainer = handContainer;
    this.inventoryContainer = inventoryContainer;

    // Other
    this.player = playerInventory.player;
    this.level = this.player.getLevel();

    log.debug("Open Configuration menu for {}: {}", this.uuid, this.entity);

    // Player Companion Amor Slots (left / slot: 3 - 0)
    int playerCompanionEquipmentLeftStartPositionY = 17;
    int playerCompanionEquipmentLeftStartPositionX = 6;
    for (int armorSlot = 3; armorSlot >= 0; armorSlot--) {
      this.addSlot(new ArmorSlot(this, this.armorContainer, 3 - armorSlot,
          playerCompanionEquipmentLeftStartPositionX,
          playerCompanionEquipmentLeftStartPositionY + armorSlot * slotSize));
    }

    // Player Companion Equipment Slots (right / slot: 4 - 7)
    int playerCompanionEquipmentRightStartPositionY = 17;
    int playerCompanionEquipmentRightStartPositionX = 96;
    for (int equipmentSlot = 0; equipmentSlot < 4; ++equipmentSlot) {
      this.addSlot(new DummySlot(this.equipmentContainer, equipmentSlot + 4,
          playerCompanionEquipmentRightStartPositionX,
          playerCompanionEquipmentRightStartPositionY + equipmentSlot * slotSize));
    }

    // Player Companion Main Hand Slot (left / bottom: 0)
    int playerCompanionMainHandStartPositionY = 89;
    int playerCompanionMainHandStartPositionX = 6;
    this.addSlot(new HandSlot(this, this.handContainer, EquipmentSlot.MAINHAND.getIndex(),
        playerCompanionMainHandStartPositionX, playerCompanionMainHandStartPositionY));

    // Player Companion Off Hand Slot (right / bottom: 1)
    int playerCompanionOffHandStartPositionY = 89;
    int playerCompanionOffHandStartPositionX = 96;
    this.addSlot(new HandSlot(this, this.handContainer, EquipmentSlot.OFFHAND.getIndex(),
        playerCompanionOffHandStartPositionX, playerCompanionOffHandStartPositionY));

    // Player Inventory Slots
    int playerInventoryStartPositionY = 151;
    int playerInventoryStartPositionX = 6;
    for (int inventoryRow = 0; inventoryRow < 3; ++inventoryRow) {
      for (int inventoryColumn = 0; inventoryColumn < 9; ++inventoryColumn) {
        this.addSlot(new Slot(playerInventory, inventoryColumn + inventoryRow * 9 + 9,
            playerInventoryStartPositionX + inventoryColumn * slotSize,
            playerInventoryStartPositionY + inventoryRow * slotSize));
      }
    }

    // Player Hotbar Slots
    int hotbarStartPositionY = 209;
    int hotbarStartPositionX = 6;
    for (int playerInventorySlot = 0; playerInventorySlot < 9; ++playerInventorySlot) {
      this.addSlot(new Slot(playerInventory, playerInventorySlot,
          hotbarStartPositionX + playerInventorySlot * slotSize, hotbarStartPositionY));
    }
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
