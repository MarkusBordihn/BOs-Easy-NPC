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
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.world.Container;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

public class BasicTradingConfigurationMenu extends TradingConfigurationMenu {

  // Defining basic layout options
  public static final int TRADING_START_POSITION_Y = 40;
  public static final int TRADING_START_POSITION_X = 30;
  public static final int TRADING_START_POSITION_SECOND_ROW_X = TRADING_START_POSITION_X + 160;
  public static final int TRADING_SLOT_SIZE = 18;
  public static final int SLOT_SIZE = 18;

  // Define containers
  protected static final int tradingContainerSize = TradingSettings.BASIC_TRADING_OFFERS * 3;
  protected final Container tradingContainer;

  public BasicTradingConfigurationMenu(int windowId, Inventory playerInventory, UUID uuid) {
    this(ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU.get(), windowId, playerInventory, uuid);
  }

  public BasicTradingConfigurationMenu(
      int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(windowId, playerInventory, data.readUUID());
  }

  public BasicTradingConfigurationMenu(
      final MenuType<?> menuType, int windowId, Inventory inventory, UUID uuid) {
    this(menuType, windowId, inventory, new SimpleContainer(tradingContainerSize), uuid);
  }

  public BasicTradingConfigurationMenu(
      final MenuType<?> menuType,
      final int windowId,
      final Inventory playerInventory,
      final Container tradingContainer,
      UUID uuid) {
    super(menuType, windowId, playerInventory, uuid, 0);

    // Make sure the passed container matched the expected sizes
    checkContainerSize(tradingContainer, tradingContainerSize);

    // Container
    this.tradingContainer = tradingContainer;

    // Restructure Container from merchant offers.
    if (!this.level.isClientSide) {
      MerchantOffers merchantOffers = this.getEasyNPC().getEasyNPCTradingData().getTradingOffers();
      if (merchantOffers != null) {
        for (int tradingOffer = 0;
            tradingOffer < TradingSettings.BASIC_TRADING_OFFERS
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
    int slotPositionX = TRADING_START_POSITION_X;
    int slotPositionY = TRADING_START_POSITION_Y;
    for (int tradingOffer = 0;
        tradingOffer < TradingSettings.BASIC_TRADING_OFFERS;
        tradingOffer++) {
      // Position for Second row
      if (tradingOffer == 6) {
        slotPositionX = TRADING_START_POSITION_SECOND_ROW_X;
        slotPositionY = TRADING_START_POSITION_Y;
      }

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
    int playerInventoryStartPositionX = 8;
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
    int hotbarStartPositionX = 8;
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
        return Component.literal("Basic Trades for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(
          int windowId, Inventory inventory, Player serverPlayer) {
        return new BasicTradingConfigurationMenu(windowId, inventory, uuid);
      }
    };
  }

  @Override
  public void setTradingChanged() {
    if (this.level.isClientSide) {
      return;
    }
    this.getEasyNPC().getEasyNPCTradingData().setBasicTradingOffers(tradingContainer);
  }
}
