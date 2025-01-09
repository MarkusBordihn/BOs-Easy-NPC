/*
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

import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.handler.EquipmentHandler;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.equipment.slot.ArmorSlot;
import de.markusbordihn.easynpc.menu.configuration.equipment.slot.HandSlot;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;

public class EquipmentConfigurationMenu extends ConfigurationMenu {

  // Defining basic layout options
  protected static final int ARMOR_CONTAINER_SIZE = 4;
  protected static final int HAND_CONTAINER_SIZE = 2;
  protected static final int SLOT_SIZE = 18;

  // Define containers
  protected final Container armorContainer;
  protected final Container handContainer;

  public EquipmentConfigurationMenu(
      final MenuType<?> menuType, final int containerId, final Inventory playerInventory) {
    this(menuType, containerId, playerInventory, new CompoundTag());
  }

  public EquipmentConfigurationMenu(
      final MenuType<?> menuType,
      int containerId,
      final Inventory playerInventory,
      final CompoundTag data) {
    this(
        menuType,
        containerId,
        playerInventory,
        new SimpleContainer(ARMOR_CONTAINER_SIZE),
        new SimpleContainer(HAND_CONTAINER_SIZE),
        data);
  }

  public EquipmentConfigurationMenu(
      final MenuType<?> menuType,
      final int windowId,
      final Inventory playerInventory,
      final Container armorContainer,
      final Container handContainer,
      final CompoundTag data) {
    super(menuType, windowId, playerInventory, data);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(armorContainer, ARMOR_CONTAINER_SIZE);
    checkContainerSize(handContainer, HAND_CONTAINER_SIZE);

    // Container
    this.armorContainer = armorContainer;
    this.handContainer = handContainer;

    // Update containers, if needed.
    this.loadHand();

    // Player Companion Amor Slots (left / slot: 3 - 0)
    ModelData<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    if (modelData == null || modelData.canUseArmor()) {
      this.loadArmor();
      int playerCompanionEquipmentLeftStartPositionY = 44;
      int playerCompanionEquipmentLeftStartPositionX = 98;
      for (int armorSlot = 3; armorSlot >= 0; armorSlot--) {
        this.addSlot(
            new ArmorSlot(
                this,
                this.armorContainer,
                3 - armorSlot,
                playerCompanionEquipmentLeftStartPositionX,
                playerCompanionEquipmentLeftStartPositionY + armorSlot * SLOT_SIZE));
      }
    }

    // Player Companion Main Hand Slot (left / bottom: 0)
    if (modelData == null || modelData.canUseMainHand()) {
      int playerCompanionMainHandStartPositionY = 119;
      int playerCompanionMainHandStartPositionX = 98;
      this.addSlot(
          new HandSlot(
              this,
              this.handContainer,
              EquipmentSlot.MAINHAND.getIndex(),
              playerCompanionMainHandStartPositionX,
              playerCompanionMainHandStartPositionY));
    }

    // Player Companion Off Hand Slot (right / bottom: 1)
    if (modelData == null || modelData.canUseOffHand()) {
      int playerCompanionOffHandStartPositionY = 119;
      int playerCompanionOffHandStartPositionX = 178;
      this.addSlot(
          new HandSlot(
              this,
              this.handContainer,
              EquipmentSlot.OFFHAND.getIndex(),
              playerCompanionOffHandStartPositionX,
              playerCompanionOffHandStartPositionY));
    }

    // Player Inventory Slots
    int playerInventoryStartPositionY = 149;
    int playerInventoryStartPositionX = 66;
    for (int inventoryRow = 0; inventoryRow < 3; ++inventoryRow) {
      for (int inventoryColumn = 0; inventoryColumn < 9; ++inventoryColumn) {
        this.addSlot(
            new Slot(
                playerInventory,
                inventoryColumn + inventoryRow * 9 + 9,
                playerInventoryStartPositionX + inventoryColumn * SLOT_SIZE,
                playerInventoryStartPositionY + inventoryRow * SLOT_SIZE));
      }
    }

    // Player Hotbar Slots
    int hotbarStartPositionY = 209;
    int hotbarStartPositionX = 66;
    for (int playerInventorySlot = 0; playerInventorySlot < 9; ++playerInventorySlot) {
      this.addSlot(
          new Slot(
              playerInventory,
              playerInventorySlot,
              hotbarStartPositionX + playerInventorySlot * SLOT_SIZE,
              hotbarStartPositionY));
    }
  }

  public void loadHand() {
    if (this.level.isClientSide) {
      return;
    }
    log.debug("Load hand {}", this.getEasyNPC().getLivingEntity().getHandSlots());
    this.handContainer.setItem(
        0, this.getEasyNPC().getLivingEntity().getItemInHand(InteractionHand.MAIN_HAND));
    this.handContainer.setItem(
        1, this.getEasyNPC().getLivingEntity().getItemInHand(InteractionHand.OFF_HAND));
    this.handContainer.setChanged();
  }

  public void setHandChanged(InteractionHand hand, ItemStack itemStack) {
    if (this.level.isClientSide) {
      return;
    }
    EquipmentHandler.setHandSlotItem(this.getEasyNPC(), hand, itemStack);
  }

  public void loadArmor() {
    if (this.level.isClientSide) {
      return;
    }
    log.debug("Load armor {}", this.getEasyNPC().getLivingEntity().getArmorSlots());
    this.armorContainer.setItem(
        0, this.getEasyNPC().getLivingEntity().getItemBySlot(EquipmentSlot.FEET));
    this.armorContainer.setItem(
        1, this.getEasyNPC().getLivingEntity().getItemBySlot(EquipmentSlot.LEGS));
    this.armorContainer.setItem(
        2, this.getEasyNPC().getLivingEntity().getItemBySlot(EquipmentSlot.CHEST));
    this.armorContainer.setItem(
        3, this.getEasyNPC().getLivingEntity().getItemBySlot(EquipmentSlot.HEAD));
    this.armorContainer.setChanged();
  }

  public void setArmorChanged(EquipmentSlot equipmentSlot, int slot, ItemStack itemStack) {
    if (this.level.isClientSide) {
      return;
    }
    EquipmentHandler.setArmorSlotItem(this.getEasyNPC(), equipmentSlot, itemStack);
  }
}
