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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TradingOfferHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TradingOfferHandler() {}

  public static void setAdvancedTradingMaxUses(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, int maxUses) {
    MerchantOffers merchantOffers = tradingData.getTradingOffers();
    if (merchantOffers == null
        || merchantOffers.isEmpty()
        || merchantOffers.size() <= tradingOfferIndex) {
      return;
    }
    MerchantOffer merchantOffer = merchantOffers.get(tradingOfferIndex);
    if (merchantOffer == null) {
      return;
    }
    merchantOffers.set(
        tradingOfferIndex,
        new MerchantOffer(
            merchantOffer.getBaseCostA(),
            merchantOffer.getCostB(),
            merchantOffer.getResult(),
            0,
            maxUses,
            merchantOffer.getXp(),
            merchantOffer.getPriceMultiplier(),
            merchantOffer.getDemand()));
    tradingData.setTradingOffers(merchantOffers);
  }

  public static void setAdvancedTradingXp(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, int xp) {
    MerchantOffers merchantOffers = tradingData.getTradingOffers();
    if (merchantOffers == null
        || merchantOffers.isEmpty()
        || merchantOffers.size() <= tradingOfferIndex) {
      return;
    }
    MerchantOffer merchantOffer = merchantOffers.get(tradingOfferIndex);
    if (merchantOffer == null) {
      return;
    }
    merchantOffers.set(
        tradingOfferIndex,
        new MerchantOffer(
            merchantOffer.getBaseCostA(),
            merchantOffer.getCostB(),
            merchantOffer.getResult(),
            merchantOffer.getUses(),
            merchantOffer.getMaxUses(),
            xp,
            merchantOffer.getPriceMultiplier(),
            merchantOffer.getDemand()));
    tradingData.setTradingOffers(merchantOffers);
  }

  public static void setAdvancedTradingPriceMultiplier(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, float priceMultiplier) {
    MerchantOffers merchantOffers = tradingData.getTradingOffers();
    if (merchantOffers == null
        || merchantOffers.isEmpty()
        || merchantOffers.size() <= tradingOfferIndex) {
      return;
    }
    MerchantOffer merchantOffer = merchantOffers.get(tradingOfferIndex);
    if (merchantOffer == null) {
      return;
    }
    merchantOffers.set(
        tradingOfferIndex,
        new MerchantOffer(
            merchantOffer.getBaseCostA(),
            merchantOffer.getCostB(),
            merchantOffer.getResult(),
            merchantOffer.getUses(),
            merchantOffer.getMaxUses(),
            merchantOffer.getXp(),
            priceMultiplier,
            merchantOffer.getDemand()));
    tradingData.setTradingOffers(merchantOffers);
  }

  public static void setAdvancedTradingDemand(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, int demand) {
    MerchantOffers merchantOffers = tradingData.getTradingOffers();
    if (merchantOffers == null
        || merchantOffers.isEmpty()
        || merchantOffers.size() <= tradingOfferIndex) {
      return;
    }
    MerchantOffer merchantOffer = merchantOffers.get(tradingOfferIndex);
    if (merchantOffer == null) {
      return;
    }
    merchantOffers.set(
        tradingOfferIndex,
        new MerchantOffer(
            merchantOffer.getBaseCostA(),
            merchantOffer.getCostB(),
            merchantOffer.getResult(),
            merchantOffer.getUses(),
            merchantOffer.getMaxUses(),
            merchantOffer.getXp(),
            merchantOffer.getPriceMultiplier(),
            demand));
    tradingData.setTradingOffers(merchantOffers);
  }

  public static void updateBasicTradingOffers(TradingDataCapable<?> tradingData) {
    if (tradingData
        .getTradingDataSet()
        .isType(de.markusbordihn.easynpc.data.trading.TradingType.BASIC)) {
      return;
    }

    MerchantOffers merchantOffers = tradingData.getTradingOffers();
    if (merchantOffers == null || merchantOffers.isEmpty()) {
      return;
    }

    // Update trading offers
    MerchantOffers newMerchantOffers = new MerchantOffers();
    for (MerchantOffer merchantOffer : merchantOffers) {
      if (!isValidTradingOffer(
          merchantOffer.getBaseCostA(), merchantOffer.getCostB(), merchantOffer.getResult())) {
        continue;
      }
      MerchantOffer newMerchantOffer =
          new MerchantOffer(
              merchantOffer.getBaseCostA(),
              merchantOffer.getCostB(),
              merchantOffer.getResult(),
              tradingData.getTradingDataSet().getMaxUses(),
              tradingData.getTradingDataSet().getRewardedXP(),
              merchantOffer.getPriceMultiplier());
      newMerchantOffers.add(newMerchantOffer);
    }

    // Update trading offers
    tradingData.setTradingOffers(newMerchantOffers);
  }

  private static boolean isValidTradingOffer(
      net.minecraft.world.item.ItemStack itemA,
      net.minecraft.world.item.ItemStack itemB,
      net.minecraft.world.item.ItemStack itemResult) {
    if (itemResult == null || (itemA == null && itemB == null)) {
      return false;
    }
    return ((itemA != null && !itemA.isEmpty()) || (itemB != null && !itemB.isEmpty()))
        && !itemResult.isEmpty();
  }
}
