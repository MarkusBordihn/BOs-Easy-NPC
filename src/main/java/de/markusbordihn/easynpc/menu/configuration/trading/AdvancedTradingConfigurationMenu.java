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

package de.markusbordihn.easynpc.menu.configuration.trading;

import java.util.UUID;

import javax.annotation.Nullable;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.Container;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

import de.markusbordihn.easynpc.menu.ModMenuTypes;

public class AdvancedTradingConfigurationMenu extends TradingConfigurationMenu {

  // Defining basic layout options
  public static final int TRADING_OFFERS = 25;
  public static final int TRADING_OFFERS_PER_PAGE = 5;
  public static final int TRADING_START_POSITION_Y = 60;
  public static final int TRADING_START_POSITION_X = 30;
  public static final int TRADING_SLOT_SIZE = 18;
  public static final int SLOT_SIZE = 18;

  // Define containers
  protected static int tradingContainerSize = TRADING_OFFERS * 3;
  protected final Container tradingContainer;

  public AdvancedTradingConfigurationMenu(int windowId, Inventory playerInventory, UUID uuid,
      int pageIndex) {
    this(ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU.get(), windowId, playerInventory, uuid,
        pageIndex);
  }

  public AdvancedTradingConfigurationMenu(int windowId, Inventory playerInventory,
      FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID(), data.readInt());
  }

  public AdvancedTradingConfigurationMenu(final MenuType<?> menuType, int windowId,
      Inventory inventory, UUID uuid, int pageIndex) {
    this(menuType, windowId, inventory, new SimpleContainer(tradingContainerSize), uuid, pageIndex);
  }

  public AdvancedTradingConfigurationMenu(final MenuType<?> menuType, final int windowId,
      final Inventory playerInventory, final Container tradingContainer, UUID uuid, int pageIndex) {
    super(menuType, windowId, playerInventory, uuid, pageIndex);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(tradingContainer, tradingContainerSize);

    // Container
    this.tradingContainer = tradingContainer;

    // Restructure Container from merchant offers.
    if (!this.level.isClientSide) {
      MerchantOffers merchantOffers = this.entity.getTradingOffers();
      if (merchantOffers != null) {
        for (int tradingOffer = 0; tradingOffer < TRADING_OFFERS
            && tradingOffer < merchantOffers.size(); tradingOffer++) {
          MerchantOffer merchantOffer = merchantOffers.get(tradingOffer);
          this.tradingContainer.setItem((tradingOffer * 3) + 0, merchantOffer.getBaseCostA());
          this.tradingContainer.setItem((tradingOffer * 3) + 1, merchantOffer.getCostB());
          this.tradingContainer.setItem((tradingOffer * 3) + 2, merchantOffer.getResult());
        }
      }
    }

    // Trading offers Slots
    int tradingPage = this.getPageIndex();
    int slotPositionX = TRADING_START_POSITION_X;
    int slotPositionY = TRADING_START_POSITION_Y;

    // Only display a limited amount of trading slots per page.
    for (int tradingOffer = (tradingPage
        * TRADING_OFFERS_PER_PAGE); tradingOffer < ((tradingPage + 1) * TRADING_OFFERS_PER_PAGE)
            && tradingOffer < TRADING_OFFERS; tradingOffer++) {

      // Item A Slot
      this.addSlot(new ItemASlot(this, tradingContainer, (tradingOffer * 3) + 0, slotPositionX,
          slotPositionY));

      // Item B Slot
      this.addSlot(new ItemBSlot(this, tradingContainer, (tradingOffer * 3) + 1,
          slotPositionX + TRADING_SLOT_SIZE + TRADING_SLOT_SIZE, slotPositionY));

      // Result Slot
      this.addSlot(new ItemResultSlot(this, tradingContainer, (tradingOffer * 3) + 2,
          slotPositionX + ((TRADING_SLOT_SIZE + TRADING_SLOT_SIZE + 5) * 2), slotPositionY));

      slotPositionY += TRADING_SLOT_SIZE + 1;
    }

    // Player Inventory Slots
    int playerInventoryStartPositionY = 159;
    int playerInventoryStartPositionX = 80;
    for (int inventoryRow = 0; inventoryRow < 3; ++inventoryRow) {
      for (int inventoryColumn = 0; inventoryColumn < 9; ++inventoryColumn) {
        this.addSlot(new Slot(playerInventory, inventoryColumn + inventoryRow * 9 + 9,
            playerInventoryStartPositionX + inventoryColumn * SLOT_SIZE,
            playerInventoryStartPositionY + inventoryRow * SLOT_SIZE));
      }
    }

    // Player Hotbar Slots
    int hotbarStartPositionY = 215;
    int hotbarStartPositionX = 80;
    for (int playerInventorySlot = 0; playerInventorySlot < 9; ++playerInventorySlot) {
      this.addSlot(new Slot(playerInventory, playerInventorySlot,
          hotbarStartPositionX + playerInventorySlot * SLOT_SIZE, hotbarStartPositionY));
    }
  }

  @Override
  public void setTradingChanged() {
    if (this.level.isClientSide) {
      return;
    }
    this.entity.setAdvancedTradingOffers(tradingContainer);
  }

  public int getMaxPages() {
    return TRADING_OFFERS / TRADING_OFFERS_PER_PAGE;
  }

  public static MenuProvider getMenuProvider(UUID uuid, Entity entity, int pageIndex) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Advanced trades for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new AdvancedTradingConfigurationMenu(windowId, inventory, uuid, pageIndex);
      }
    };
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
