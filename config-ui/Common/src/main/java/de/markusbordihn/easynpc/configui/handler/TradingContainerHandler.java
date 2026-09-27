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
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TradingContainerHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TradingContainerHandler() {}

  public static void setAdvancedTradingOffers(
      TradingDataCapable<?> tradingData, Container container) {
    MerchantOffers existingMerchantOffers = tradingData.getTradingOffers();
    MerchantOffers merchantOffers = new MerchantOffers();
    int merchantOfferIndex = 0;
    for (int offerIndex = 0; offerIndex < TradingSettings.ADVANCED_TRADING_OFFERS; offerIndex++) {
      ItemStack itemA = container.getItem(offerIndex * 3);
      ItemStack itemB = container.getItem(offerIndex * 3 + 1);
      ItemStack itemResult = container.getItem(offerIndex * 3 + 2);
      if (!isValidTradingOffer(itemA, itemB, itemResult)) {
        continue;
      }

      MerchantOffer existingMerchantOffer =
          existingMerchantOffers != null && existingMerchantOffers.size() > offerIndex
              ? existingMerchantOffers.get(offerIndex)
              : null;
      if (existingMerchantOffer != null) {
        merchantOffers.add(
            merchantOfferIndex++,
            new MerchantOffer(
                itemA,
                itemB,
                itemResult,
                existingMerchantOffer.getUses(),
                existingMerchantOffer.getMaxUses(),
                existingMerchantOffer.getXp(),
                existingMerchantOffer.getPriceMultiplier(),
                existingMerchantOffer.getDemand()));
      } else {
        merchantOffers.add(
            merchantOfferIndex++, new MerchantOffer(itemA, itemB, itemResult, 64, 1, 1.0F));
      }
    }

    if (!merchantOffers.isEmpty()) {
      tradingData.getTradingDataSet().setType(TradingType.ADVANCED);
      tradingData.setTradingOffers(merchantOffers);
    }
  }

  public static void setBasicTradingOffers(TradingDataCapable<?> tradingData, Container container) {
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int offerIndex = 0; offerIndex < TradingSettings.BASIC_TRADING_OFFERS; offerIndex++) {
      ItemStack itemA = container.getItem(offerIndex * 3);
      ItemStack itemB = container.getItem(offerIndex * 3 + 1);
      ItemStack itemResult = container.getItem(offerIndex * 3 + 2);
      if (!isValidTradingOffer(itemA, itemB, itemResult)) {
        continue;
      }

      MerchantOffer merchantOffer =
          new MerchantOffer(
              itemA,
              itemB,
              itemResult,
              tradingData.getTradingDataSet().getMaxUses(),
              tradingData.getTradingDataSet().getRewardedXP(),
              1.0F);
      merchantOffers.add(merchantOffer);
    }

    if (!merchantOffers.isEmpty()) {
      tradingData.getTradingDataSet().setType(TradingType.BASIC);
      tradingData.setTradingOffers(merchantOffers);
    }
  }

  private static boolean isValidTradingOffer(
      ItemStack itemA, ItemStack itemB, ItemStack itemResult) {
    return (!itemA.isEmpty() || !itemB.isEmpty()) && !itemResult.isEmpty();
  }
}
