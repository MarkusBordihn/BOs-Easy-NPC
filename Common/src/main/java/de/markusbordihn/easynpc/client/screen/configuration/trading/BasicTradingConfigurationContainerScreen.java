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

package de.markusbordihn.easynpc.client.screen.configuration.trading;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.PositiveNumberField;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.trading.TradingSettings;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.utils.ValueUtils;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class BasicTradingConfigurationContainerScreen<T extends ConfigurationMenu>
    extends TradingConfigurationContainerScreen<T> {

  protected EditBox resetsEveryMinEditBox;
  protected EditBox maxUsesEditBox;
  protected EditBox rewardExpEditBox;

  public BasicTradingConfigurationContainerScreen(
      T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private void onResetsEveryMinEditBoxChanged(String text) {
    if (ValueUtils.isPositiveNumericValueOrZero(text) && !text.isEmpty()) {
      NetworkMessageHandlerManager.getServerHandler()
          .setBasicTradingResetsEveryMin(this.getEasyNPCUUID(), Integer.parseInt(text));
    }
  }

  private void onMaxUsesEditBoxChanged(String text) {
    if (ValueUtils.isPositiveNumericValueOrZero(text) && !text.isEmpty()) {
      NetworkMessageHandlerManager.getServerHandler()
          .setBasicTradingMaxUses(this.getEasyNPCUUID(), Integer.parseInt(text));
    }
  }

  private void onRewardExpEditBoxChanged(String text) {
    if (ValueUtils.isPositiveNumericValueOrZero(text) && !text.isEmpty()) {
      NetworkMessageHandlerManager.getServerHandler()
          .setBasicTradingRewardExp(this.getEasyNPCUUID(), Integer.parseInt(text));
    }
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicTradesButton.active = false;

    // Trading Data
    TradingData<?> tradingData = this.getEasyNPC().getEasyNPCTradingData();

    // Reset Every Min Edit Box
    this.resetsEveryMinEditBox =
        new TextField(this.font, this.contentLeftPos + 166, this.contentTopPos + 142, 32);
    this.resetsEveryMinEditBox.setMaxLength(3);
    this.resetsEveryMinEditBox.setValue(tradingData.getTradingResetsEveryMin() + "");
    this.resetsEveryMinEditBox.setResponder(this::onResetsEveryMinEditBoxChanged);
    this.resetsEveryMinEditBox.setFilter(ValueUtils::isPositiveNumericValueOrZero);
    this.addRenderableWidget(this.resetsEveryMinEditBox);

    // Max Uses Edit Box
    this.maxUsesEditBox =
        new PositiveNumberField(this.font, this.contentLeftPos + 166, this.contentTopPos + 165, 32);
    this.maxUsesEditBox.setMaxLength(4);
    this.maxUsesEditBox.setValue(tradingData.getBasicTradingMaxUses() + "");
    this.maxUsesEditBox.setResponder(this::onMaxUsesEditBoxChanged);
    this.maxUsesEditBox.setFilter(ValueUtils::isPositiveNumericValueOrZero);
    this.addRenderableWidget(this.maxUsesEditBox);

    // Experience Edit Box
    this.rewardExpEditBox =
        new TextField(this.font, this.contentLeftPos + 166, this.contentTopPos + 188, 32);
    this.rewardExpEditBox.setMaxLength(3);
    this.rewardExpEditBox.setValue(tradingData.getBasicTradingRewardExp() + "");
    this.rewardExpEditBox.setResponder(this::onRewardExpEditBoxChanged);
    this.rewardExpEditBox.setFilter(ValueUtils::isPositiveNumericValueOrZero);
    this.addRenderableWidget(this.rewardExpEditBox);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    this.renderTooltip(guiGraphics, x, y);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Render Trading Slots
    int slotPositionX = this.leftPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_X - 1;
    int slotPositionY = this.topPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_Y - 1;
    for (int tradingOffer = 0;
        tradingOffer < TradingSettings.BASIC_TRADING_OFFERS;
        tradingOffer++) {

      // Position for Second row
      if (tradingOffer == 6) {
        slotPositionX =
            this.leftPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_SECOND_ROW_X - 1;
        slotPositionY = this.topPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_Y - 1;
      }

      // Offer Label
      Text.drawString(
          guiGraphics,
          this.font,
          (tradingOffer < 9 ? " " : "") + (tradingOffer + 1) + ".",
          slotPositionX - 15,
          slotPositionY + 5,
          0x404040);

      // Item A Slot
      int itemASlotLeftPosition = slotPositionX;
      int itemASlotTopPosition = slotPositionY;
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_INVENTORY,
          itemASlotLeftPosition,
          itemASlotTopPosition,
          7,
          7,
          18,
          18);

      // "+" Label
      Text.drawString(
          guiGraphics,
          this.font,
          "+",
          itemASlotLeftPosition + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 6,
          itemASlotTopPosition + 5,
          0x404040);

      // Item B Slot
      int itemBSlotLeftPosition =
          slotPositionX
              + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE
              + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE;
      int itemBSlotTopPosition = slotPositionY;
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_INVENTORY,
          itemBSlotLeftPosition,
          itemBSlotTopPosition,
          7,
          7,
          18,
          18);

      // "=" Label
      Text.drawString(
          guiGraphics,
          this.font,
          "=",
          itemBSlotLeftPosition + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 12,
          itemBSlotTopPosition + 5,
          0x404040);

      // Result Slot
      Graphics.blit(
          guiGraphics,
          Constants.TEXTURE_INVENTORY,
          slotPositionX
              + ((BasicTradingConfigurationMenu.TRADING_SLOT_SIZE
                      + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE
                      + 5)
                  * 2),
          slotPositionY,
          7,
          7,
          18,
          18);

      slotPositionY += BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 1;
    }

    // Player Inventory Slots
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos,
        this.contentTopPos + 135,
        7,
        83,
        162,
        54);

    // Player Hotbar Slots
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos,
        this.contentTopPos + 191,
        7,
        141,
        162,
        18);

    // Render Reset Every Min Label
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "trading.minutes_for_trade_reset",
        this.contentLeftPos + 202,
        this.resetsEveryMinEditBox.getY() + 3,
        0x404040);

    // Render Max Uses Label
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "trading.max_uses_per_trade",
        this.contentLeftPos + 202,
        this.maxUsesEditBox.getY() + 3,
        0x404040);

    // Render Reward Exp Label
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "trading.rewarded_exp_per_trade",
        this.contentLeftPos + 202,
        this.rewardExpEditBox.getY() + 3,
        0x404040);
  }
}
