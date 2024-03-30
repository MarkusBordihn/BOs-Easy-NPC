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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EquipmentConfigurationMenu extends ConfigurationMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  // Defining basic layout options
  protected static final int ARMOR_CONTAINER_SIZE = 4;
  protected static final int HAND_CONTAINER_SIZE = 2;
  protected static final int SLOT_SIZE = 18;

  // Define containers
  protected final Container armorContainer;
  protected final Container handContainer;

  public EquipmentConfigurationMenu(int windowId, Inventory playerInventory, UUID uuid) {
    this(ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU.get(), windowId, playerInventory, uuid);
  }

  public EquipmentConfigurationMenu(int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID());
  }

  public EquipmentConfigurationMenu(
      final MenuType<?> menuType, int windowId, Inventory inventory, UUID uuid) {
    this(
        menuType,
        windowId,
        inventory,
        new SimpleContainer(ARMOR_CONTAINER_SIZE),
        new SimpleContainer(HAND_CONTAINER_SIZE),
        uuid);
  }

  public EquipmentConfigurationMenu(
      final MenuType<?> menuType,
      final int windowId,
      final Inventory playerInventory,
      final Container armorContainer,
      final Container handContainer,
      UUID uuid) {
    super(menuType, windowId, playerInventory, uuid);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(armorContainer, ARMOR_CONTAINER_SIZE);
    checkContainerSize(handContainer, HAND_CONTAINER_SIZE);

    // Container
    this.armorContainer = armorContainer;
    this.handContainer = handContainer;

    // Update containers, if needed.
    this.loadHand();

    // Model data
    ModelData<?> modelData = this.getEasyNPC().getEasyNPCModelData();

    // Player Companion Amor Slots (left / slot: 3 - 0)
    if (modelData.canUseArmor()) {
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
    if (modelData.canUseMainHand()) {
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
    if (modelData.canUseOffHand()) {
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

  public static MenuProvider getMenuProvider(UUID uuid, Entity entity) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return Component.literal("Equipment for " + entity.getName().getString());
      }

      @Override
      public AbstractContainerMenu createMenu(
          int windowId, Inventory inventory, Player serverPlayer) {
        return new EquipmentConfigurationMenu(windowId, inventory, uuid);
      }
    };
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
    log.debug("Hand changed {} {} ...", hand, itemStack);
    this.getEasyNPC().getLivingEntity().setItemInHand(hand, itemStack);
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
    log.debug("Armor changed {} {} {} ...", equipmentSlot, slot, itemStack);
    this.getEasyNPC().getLivingEntity().setItemSlot(equipmentSlot, itemStack);
  }
}
