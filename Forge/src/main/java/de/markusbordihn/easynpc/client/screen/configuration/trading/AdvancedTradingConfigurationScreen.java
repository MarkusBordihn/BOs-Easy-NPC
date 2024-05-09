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

import com.mojang.blaze3d.systems.RenderSystem;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.PositiveNumberField;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import java.util.HashMap;
import java.util.Objects;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class AdvancedTradingConfigurationScreen
    extends TradingConfigurationScreen<AdvancedTradingConfigurationMenu> {

  protected static final int NAVIGATION_TICKER = 20;
  protected static final int TRADING_OFFERS_TICKER = 20;
  protected final HashMap<Integer, EditBox> maxUsesEditBoxes = new HashMap<>();
  protected final HashMap<Integer, EditBox> rewardExpEditBoxes = new HashMap<>();
  protected final HashMap<Integer, EditBox> priceMultiplierEditBoxes = new HashMap<>();
  protected final HashMap<Integer, EditBox> demandEditBoxes = new HashMap<>();
  protected final TradingData<?> tradingData;
  protected Button previousPageButton;
  protected Button nextPageButton;
  protected EditBox resetsEveryMinEditBox;
  private int navigationTicker = NAVIGATION_TICKER;
  private int tradingOffersTicker = TRADING_OFFERS_TICKER;

  public AdvancedTradingConfigurationScreen(
      AdvancedTradingConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.tradingData = this.easyNPC.getEasyNPCTradingData();
  }

  private void onResetsEveryMinEditBoxChanged(String text) {
    if (isNumericValue(text) && !text.isEmpty()) {
      ServerNetworkMessageHandler.setAdvancedTradingResetsEveryMin(uuid, Integer.parseInt(text));
    }
  }

  private void onMaxUsesEditBoxChanged(int offerIndex, String value, String formerValue) {
    if (!Objects.equals(value, formerValue)
        && offerIndex >= 0
        && isPositiveNumericValue(value)
        && !value.isEmpty()) {
      ServerNetworkMessageHandler.setAdvancedTradingMaxUses(
          uuid, offerIndex, Integer.parseInt(value));
    }
  }

  private void onRewardExpEditBoxChanged(int offerIndex, String value, String formerValue) {
    if (!Objects.equals(value, formerValue)
        && offerIndex >= 0
        && isNumericValue(value)
        && !value.isEmpty()) {
      ServerNetworkMessageHandler.setAdvancedTradingRewardExp(
          uuid, offerIndex, Integer.parseInt(value));
    }
  }

  private void onPriceMultiplierEditBoxChanged(int offerIndex, String value, String formerValue) {
    if (!Objects.equals(value, formerValue)
        && offerIndex >= 0
        && isFloatValue(value)
        && !value.isEmpty()) {
      ServerNetworkMessageHandler.setAdvancedTradingPriceMultiplier(
          uuid, offerIndex, Float.parseFloat(value));
    }
  }

  private void onDemandEditBoxChanged(int offerIndex, String value, String formerValue) {
    if (!Objects.equals(value, formerValue)
        && offerIndex >= 0
        && isNumericValue(value)
        && !value.isEmpty()) {
      ServerNetworkMessageHandler.setAdvancedTradingDemand(
          uuid, offerIndex, Integer.parseInt(value));
    }
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.advancedTradesButton.active = false;

    // Reset Every Min Edit Box
    this.resetsEveryMinEditBox =
        new TextField(this.font, this.contentLeftPos + 250, this.contentTopPos + 172, 32);
    this.resetsEveryMinEditBox.setMaxLength(3);
    this.resetsEveryMinEditBox.setValue(this.tradingData.getTradingResetsEveryMin() + "");
    this.resetsEveryMinEditBox.setResponder(this::onResetsEveryMinEditBoxChanged);
    this.resetsEveryMinEditBox.setFilter(TradingConfigurationScreen::isNumericValue);
    this.addRenderableWidget(this.resetsEveryMinEditBox);

    // Adding trading edit boxes for advanced trading configuration
    int editBoxPositionX = this.contentLeftPos + 140;
    int editBoxPositionY = this.topPos + AdvancedTradingConfigurationMenu.TRADING_START_POSITION_Y;
    MerchantOffers merchantOffers = this.tradingData.getTradingOffers();
    for (int tradingOffer = 0;
        tradingOffer < AdvancedTradingConfigurationMenu.TRADING_OFFERS_PER_PAGE;
        tradingOffer++) {

      int tradingOfferIndex =
          (this.menu.getPageIndex() * AdvancedTradingConfigurationMenu.TRADING_OFFERS_PER_PAGE)
              + tradingOffer;
      MerchantOffer merchantOffer =
          merchantOffers.size() > tradingOfferIndex
              ? merchantOffers.get(tradingOfferIndex)
              : new MerchantOffer(ItemStack.EMPTY, ItemStack.EMPTY, ItemStack.EMPTY, 0, 0, 0, 0);
      boolean hasValidOffer =
          (!merchantOffer.getBaseCostA().isEmpty() || !merchantOffer.getCostB().isEmpty())
              && !merchantOffer.getResult().isEmpty();
      log.info("Trading Offer {} : {}", tradingOfferIndex, merchantOffer.createTag());

      // Max Uses Edit Box
      EditBox maxUsesEditBox =
          new PositiveNumberField(this.font, editBoxPositionX, editBoxPositionY, 32);
      maxUsesEditBox.setMaxLength(4);
      maxUsesEditBox.setValue(
          merchantOffer.getMaxUses() > 0 ? merchantOffer.getMaxUses() + "" : "16");
      maxUsesEditBox.setResponder(
          text ->
              onMaxUsesEditBoxChanged(tradingOfferIndex, text, merchantOffer.getMaxUses() + ""));
      maxUsesEditBox.setEditable(hasValidOffer);
      maxUsesEditBoxes.put(tradingOfferIndex, maxUsesEditBox);
      this.addRenderableWidget(maxUsesEditBox);

      // Reward Exp Edit Box
      EditBox rewardExpEditBox =
          new TextField(this.font, editBoxPositionX + 45, editBoxPositionY, 26);
      rewardExpEditBox.setMaxLength(3);
      rewardExpEditBox.setValue(merchantOffer.getXp() >= 0 ? merchantOffer.getXp() + "" : "0");
      rewardExpEditBox.setResponder(
          text -> onRewardExpEditBoxChanged(tradingOfferIndex, text, merchantOffer.getXp() + ""));
      rewardExpEditBox.setFilter(TradingConfigurationScreen::isNumericValue);
      rewardExpEditBox.setEditable(hasValidOffer);
      rewardExpEditBoxes.put(tradingOfferIndex, rewardExpEditBox);
      this.addRenderableWidget(rewardExpEditBox);

      // Price Multiplier Edit Box
      EditBox priceMultiplierEditBox =
          new TextField(this.font, editBoxPositionX + 85, editBoxPositionY, 32);
      priceMultiplierEditBox.setMaxLength(4);
      priceMultiplierEditBox.setValue(
          merchantOffer.getPriceMultiplier() >= 0 ? merchantOffer.getPriceMultiplier() + "" : "0");
      priceMultiplierEditBox.setResponder(
          text ->
              onPriceMultiplierEditBoxChanged(
                  tradingOfferIndex, text, merchantOffer.getPriceMultiplier() + ""));
      priceMultiplierEditBox.setFilter(TradingConfigurationScreen::isFloatValue);
      priceMultiplierEditBox.setEditable(hasValidOffer);
      priceMultiplierEditBoxes.put(tradingOfferIndex, priceMultiplierEditBox);
      this.addRenderableWidget(priceMultiplierEditBox);

      // Demand Edit Box
      EditBox demandEditBox =
          new TextField(this.font, editBoxPositionX + 135, editBoxPositionY, 20);
      demandEditBox.setMaxLength(2);
      demandEditBox.setValue(merchantOffer.getDemand() >= 0 ? merchantOffer.getDemand() + "" : "0");
      demandEditBox.setResponder(
          text -> onDemandEditBoxChanged(tradingOfferIndex, text, merchantOffer.getDemand() + ""));
      demandEditBox.setFilter(TradingConfigurationScreen::isNumericValue);
      demandEditBox.setEditable(hasValidOffer);
      demandEditBoxes.put(tradingOfferIndex, demandEditBox);
      this.addRenderableWidget(demandEditBox);

      editBoxPositionY += 19;
    }

    // Page navigation Buttons
    this.previousPageButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos,
                this.contentTopPos + 115,
                70,
                "previous_page",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.ADVANCED_TRADING, this.menu.getPageIndex() - 1)));
    this.nextPageButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 235,
                this.contentTopPos + 115,
                70,
                "next_page",
                onPress ->
                    ServerNetworkMessageHandler.openConfiguration(
                        uuid, ConfigurationType.ADVANCED_TRADING, this.menu.getPageIndex() + 1)));
  }

  @Override
  public void containerTick() {
    super.containerTick();

    // Control page navigation buttons
    if (this.navigationTicker++ > NAVIGATION_TICKER) {
      if (this.previousPageButton != null) {
        this.previousPageButton.active = this.menu.getPageIndex() > 0;
      }
      if (this.nextPageButton != null) {
        this.nextPageButton.active = this.menu.getPageIndex() < this.menu.getMaxPages() - 1;
      }
      this.navigationTicker = 0;
    }

    // Validate trading offers
    if (this.tradingOffersTicker++ > TRADING_OFFERS_TICKER) {
      for (int tradingOffer = 0;
          tradingOffer < AdvancedTradingConfigurationMenu.TRADING_OFFERS_PER_PAGE;
          tradingOffer++) {

        int tradingOfferIndex =
            (this.menu.getPageIndex() * AdvancedTradingConfigurationMenu.TRADING_OFFERS_PER_PAGE)
                + tradingOffer;
        MerchantOffers merchantOffers = this.tradingData.getTradingOffers();
        MerchantOffer merchantOffer =
            merchantOffers.size() > tradingOfferIndex
                ? merchantOffers.get(tradingOfferIndex)
                : new MerchantOffer(ItemStack.EMPTY, ItemStack.EMPTY, ItemStack.EMPTY, 16, 0, 0, 0);
        boolean hasValidOffer =
            (!merchantOffer.getBaseCostA().isEmpty() || !merchantOffer.getCostB().isEmpty())
                && !merchantOffer.getResult().isEmpty();

        // Max Uses Edit Box
        EditBox maxUsesEditBox = maxUsesEditBoxes.get(tradingOfferIndex);
        if (maxUsesEditBox != null) {
          maxUsesEditBox.setEditable(hasValidOffer);
        }

        // Reward Exp Edit Box
        EditBox rewardExpEditBox = rewardExpEditBoxes.get(tradingOfferIndex);
        if (rewardExpEditBox != null) {
          rewardExpEditBox.setEditable(hasValidOffer);
        }

        // Price Multiplier Edit Box
        EditBox priceMultiplierEditBox = priceMultiplierEditBoxes.get(tradingOfferIndex);
        if (priceMultiplierEditBox != null) {
          priceMultiplierEditBox.setEditable(hasValidOffer);
        }

        // Demand Edit Box
        EditBox demandEditBox = demandEditBoxes.get(tradingOfferIndex);
        if (demandEditBox != null) {
          demandEditBox.setEditable(hasValidOffer);
        }
      }
      this.tradingOffersTicker = 0;
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    this.renderTooltip(guiGraphics, x, y);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);

    // Render Legend (without translation / not enough space for translation)
    int legendTopPositionY = this.contentTopPos + 2;
    Text.drawString(
        guiGraphics, this.font, "Cost A", this.leftPos + 18, legendTopPositionY, 0xA04040);
    Text.drawString(
        guiGraphics, this.font, "Cost B", this.leftPos + 60, legendTopPositionY, 0x40A040);
    Text.drawString(
        guiGraphics, this.font, "Result", this.leftPos + 102, legendTopPositionY, 0x4040A0);
    Text.drawString(
        guiGraphics, this.font, "Max Uses", this.leftPos + 140, legendTopPositionY, 0x004040);
    Text.drawString(guiGraphics, this.font, "XP", this.leftPos + 198, legendTopPositionY, 0x400040);
    Text.drawString(
        guiGraphics, this.font, "Multiplier", this.leftPos + 225, legendTopPositionY, 0x404040);
    Text.drawString(
        guiGraphics, this.font, "Demand", this.leftPos + 275, legendTopPositionY, 0x404000);

    // Render Trading Slots
    int slotPositionX =
        this.leftPos + AdvancedTradingConfigurationMenu.TRADING_START_POSITION_X - 1;
    int slotPositionY = this.topPos + AdvancedTradingConfigurationMenu.TRADING_START_POSITION_Y - 1;
    for (int tradingOffer = 0;
        tradingOffer < AdvancedTradingConfigurationMenu.TRADING_OFFERS_PER_PAGE;
        tradingOffer++) {

      // Offer Label
      int tradingOfferIndex =
          (this.menu.getPageIndex() * AdvancedTradingConfigurationMenu.TRADING_OFFERS_PER_PAGE)
              + tradingOffer;
      Text.drawString(
          guiGraphics,
          this.font,
          (tradingOfferIndex < 9 ? " " : "") + (tradingOfferIndex + 1) + ".",
          slotPositionX - 15,
          slotPositionY + 5,
          0x404040);

      // Item A Slot
      int itemASlotTopPosition = slotPositionY;
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);
      guiGraphics.blit(
          Constants.TEXTURE_INVENTORY, slotPositionX, itemASlotTopPosition, 7, 7, 18, 18);

      // "+" Label
      Text.drawString(
          guiGraphics,
          this.font,
          "+",
          slotPositionX + AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE + 6,
          itemASlotTopPosition + 5,
          0x404040);

      // Item B Slot
      int itemBSlotLeftPosition =
          slotPositionX
              + AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE
              + AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE;
      int itemBSlotTopPosition = slotPositionY;
      guiGraphics.blit(
          Constants.TEXTURE_INVENTORY, itemBSlotLeftPosition, itemBSlotTopPosition, 7, 7, 18, 18);

      // "=" Label
      Text.drawString(
          guiGraphics,
          this.font,
          "=",
          itemBSlotLeftPosition + AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE + 12,
          itemBSlotTopPosition + 5,
          0x404040);

      // Result Slot
      guiGraphics.blit(
          Constants.TEXTURE_INVENTORY,
          slotPositionX
              + ((AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE
                      + AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE
                      + 5)
                  * 2),
          slotPositionY,
          7,
          7,
          18,
          18);

      slotPositionY += AdvancedTradingConfigurationMenu.TRADING_SLOT_SIZE + 1;
    }

    // Player Inventory Slots
    guiGraphics.blit(
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos + 72,
        this.contentTopPos + 115,
        7,
        83,
        162,
        54);

    // Player Hotbar Slots
    guiGraphics.blit(
        Constants.TEXTURE_INVENTORY,
        this.contentLeftPos + 72,
        this.contentTopPos + 171,
        7,
        141,
        162,
        18);

    // Render Reset Every Min Label
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "trading.minutes_for_trade_reset",
        this.contentLeftPos + 236,
        this.contentTopPos + 160,
        0x404040);
  }
}
