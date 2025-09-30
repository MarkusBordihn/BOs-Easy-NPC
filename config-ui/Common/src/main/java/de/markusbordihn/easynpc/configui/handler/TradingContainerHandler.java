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

package de.markusbordihn.easynpc.configui.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.trading.TradingSettings;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import java.util.Optional;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TradingContainerHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TradingContainerHandler() {}

  public static void setAdvancedTradingOffers(
      TradingDataCapable<?> tradingData, Container container) {

    // Update trading offers with container items.
    MerchantOffers merchantOffers = new MerchantOffers();
    int merchantOfferIndex = 0;
    for (int tradingOffer = 0;
        tradingOffer < TradingSettings.ADVANCED_TRADING_OFFERS;
        tradingOffer++) {

      // Check if we have a valid trading offer.
      ItemStack itemA = container.getItem(tradingOffer * 3);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);
      if (!isValidTradingOffer(itemA, itemB, itemResult)) {
        continue;
      }

      // Calculate item costs based on item A and item B.
      ItemCost itemCostA = getItemCost(itemA);
      Optional<ItemCost> itemCostB = getOptionalItemCost(itemB);

      // Check if we have existing trading offers and use them as base for the new trading offers.
      MerchantOffers existingMerchantOffers = tradingData.getTradingOffers();
      MerchantOffer existingMerchantOffer =
          existingMerchantOffers != null && existingMerchantOffers.size() > tradingOffer
              ? existingMerchantOffers.get(tradingOffer)
              : null;
      if (existingMerchantOffer != null) {
        merchantOffers.add(
            merchantOfferIndex++,
            new MerchantOffer(
                itemCostA,
                itemCostB,
                itemResult,
                existingMerchantOffer.getUses(),
                existingMerchantOffer.getMaxUses(),
                existingMerchantOffer.getXp(),
                existingMerchantOffer.getPriceMultiplier(),
                existingMerchantOffer.getDemand()));
      } else {
        merchantOffers.add(
            merchantOfferIndex++, new MerchantOffer(itemCostA, itemCostB, itemResult, 64, 1, 1.0F));
      }
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      tradingData.getTradingDataSet().setType(TradingType.ADVANCED);
      tradingData.setTradingOffers(merchantOffers);
    }
  }

  public static void setBasicTradingOffers(TradingDataCapable<?> tradingData, Container container) {

    // Create new trading offers based on the container and number of trading offers.
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int tradingOffer = 0;
        tradingOffer < TradingSettings.BASIC_TRADING_OFFERS;
        tradingOffer++) {

      // Check if we have a valid trading offer.
      ItemStack itemA = container.getItem(tradingOffer * 3);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);
      if (!isValidTradingOffer(itemA, itemB, itemResult)) {
        continue;
      }

      // Calculate item costs based on item A and item B.
      ItemCost itemCostA = getItemCost(itemA);
      Optional<ItemCost> itemCostB = getOptionalItemCost(itemB);

      MerchantOffer merchantOffer =
          new MerchantOffer(
              itemCostA,
              itemCostB,
              itemResult,
              tradingData.getTradingDataSet().getMaxUses(),
              tradingData.getTradingDataSet().getRewardedXP(),
              1.0F);
      merchantOffers.add(merchantOffer);
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      tradingData.getTradingDataSet().setType(TradingType.BASIC);
      tradingData.setTradingOffers(merchantOffers);
    }
  }

  private static boolean isValidTradingOffer(
      ItemStack itemA, ItemStack itemB, ItemStack itemResult) {
    if (itemResult == null || (itemA == null && itemB == null)) {
      return false;
    }
    return ((itemA != null && !itemA.isEmpty()) || (itemB != null && !itemB.isEmpty()))
        && !itemResult.isEmpty();
  }

  private static ItemCost getItemCost(ItemStack itemStack) {
    return new ItemCost(
        itemStack.isEmpty() ? ItemStack.EMPTY.getItem() : itemStack.getItem(),
        itemStack.getCount());
  }

  private static Optional<ItemCost> getOptionalItemCost(ItemStack itemStack) {
    return itemStack.isEmpty() ? Optional.empty() : Optional.of(getItemCost(itemStack));
  }
}
