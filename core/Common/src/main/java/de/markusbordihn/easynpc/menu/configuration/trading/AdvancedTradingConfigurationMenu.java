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

package de.markusbordihn.easynpc.menu.configuration.trading;

import de.markusbordihn.easynpc.data.trading.TradingSettings;
import de.markusbordihn.easynpc.menu.configuration.trading.slot.ItemASlot;
import de.markusbordihn.easynpc.menu.configuration.trading.slot.ItemBSlot;
import de.markusbordihn.easynpc.menu.configuration.trading.slot.ItemResultSlot;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

public class AdvancedTradingConfigurationMenu extends TradingConfigurationMenu {

  public static final int SLOT_SIZE = 18;
  // Defining basic layout options
  public static final int TRADING_OFFERS_PER_PAGE = 5;
  public static final int TRADING_SLOT_SIZE = 18;
  public static final int TRADING_START_POSITION_X = 30;
  public static final int TRADING_START_POSITION_Y = 40;
  // Define containers
  protected static final int TRADING_CONTAINER_SIZE = TradingSettings.ADVANCED_TRADING_OFFERS * 3;
  protected final Container tradingContainer;

  public AdvancedTradingConfigurationMenu(
      final MenuType<?> menuType, final int containerId, final Inventory playerInventory) {
    this(menuType, containerId, playerInventory, new CompoundTag());
  }

  public AdvancedTradingConfigurationMenu(
      final MenuType<?> menuType,
      int containerId,
      final Inventory playerInventory,
      final CompoundTag data) {
    this(menuType, containerId, playerInventory, new SimpleContainer(TRADING_CONTAINER_SIZE), data);
  }

  public AdvancedTradingConfigurationMenu(
      final MenuType<?> menuType,
      final int windowId,
      final Inventory playerInventory,
      final Container tradingContainer,
      final CompoundTag data) {
    super(menuType, windowId, playerInventory, data);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(tradingContainer, TRADING_CONTAINER_SIZE);

    // Container
    this.tradingContainer = tradingContainer;

    // Restructure Container from merchant offers.
    if (!this.level.isClientSide) {
      MerchantOffers merchantOffers = this.getEasyNPC().getEasyNPCTradingData().getTradingOffers();
      if (merchantOffers != null) {
        for (int tradingOffer = 0;
            tradingOffer < TradingSettings.ADVANCED_TRADING_OFFERS
                && tradingOffer < merchantOffers.size();
            tradingOffer++) {
          MerchantOffer merchantOffer = merchantOffers.get(tradingOffer);
          this.tradingContainer.setItem((tradingOffer * 3), merchantOffer.getBaseCostA());
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
    for (int tradingOffer = (tradingPage * TRADING_OFFERS_PER_PAGE);
        tradingOffer < ((tradingPage + 1) * TRADING_OFFERS_PER_PAGE)
            && tradingOffer < TradingSettings.ADVANCED_TRADING_OFFERS;
        tradingOffer++) {

      // Item A Slot
      this.addSlot(
          new ItemASlot(this, tradingContainer, (tradingOffer * 3), slotPositionX, slotPositionY));

      // Item B Slot
      this.addSlot(
          new ItemBSlot(
              this,
              tradingContainer,
              (tradingOffer * 3) + 1,
              slotPositionX + TRADING_SLOT_SIZE + TRADING_SLOT_SIZE,
              slotPositionY));

      // Result Slot
      this.addSlot(
          new ItemResultSlot(
              this,
              tradingContainer,
              (tradingOffer * 3) + 2,
              slotPositionX + ((TRADING_SLOT_SIZE + TRADING_SLOT_SIZE + 5) * 2),
              slotPositionY));

      slotPositionY += TRADING_SLOT_SIZE + 1;
    }

    // Player Inventory Slots
    int playerInventoryStartPositionY = 159;
    int playerInventoryStartPositionX = 80;
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
    int hotbarStartPositionY = 215;
    int hotbarStartPositionX = 80;
    for (int playerInventorySlot = 0; playerInventorySlot < 9; ++playerInventorySlot) {
      this.addSlot(
          new Slot(
              playerInventory,
              playerInventorySlot,
              hotbarStartPositionX + playerInventorySlot * SLOT_SIZE,
              hotbarStartPositionY));
    }
  }

  @Override
  public void setTradingChanged() {
    if (this.level.isClientSide) {
      return;
    }
    this.getEasyNPC().getEasyNPCTradingData().setAdvancedTradingOffers(tradingContainer);
  }

  public int getMaxPages() {
    return TradingSettings.ADVANCED_TRADING_OFFERS / TRADING_OFFERS_PER_PAGE;
  }
}
