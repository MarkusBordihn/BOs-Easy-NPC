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
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import java.util.Optional;
import java.util.function.Function;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class TradingOfferHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TradingOfferHandler() {}

  private static void updateTradingOffer(
      TradingDataCapable<?> tradingData,
      int tradingOfferIndex,
      Function<MerchantOffer, MerchantOffer> updater) {
    MerchantOffers merchantOffers = tradingData.getTradingOffers();
    if (merchantOffers == null
        || merchantOffers.isEmpty()
        || merchantOffers.size() <= tradingOfferIndex
        || tradingOfferIndex < 0) {
      return;
    }
    MerchantOffer merchantOffer = merchantOffers.get(tradingOfferIndex);
    if (merchantOffer == null) {
      return;
    }
    merchantOffers.set(tradingOfferIndex, updater.apply(merchantOffer));
    tradingData.setTradingOffers(merchantOffers);
  }

  public static void setAdvancedTradingMaxUses(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, int maxUses) {
    updateTradingOffer(
        tradingData,
        tradingOfferIndex,
        offer ->
            new MerchantOffer(
                getItemCost(offer.getBaseCostA()),
                getOptionalItemCost(offer.getCostB()),
                offer.getResult(),
                0,
                maxUses,
                offer.getXp(),
                offer.getPriceMultiplier(),
                offer.getDemand()));
  }

  public static void setAdvancedTradingXp(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, int xp) {
    updateTradingOffer(
        tradingData,
        tradingOfferIndex,
        offer ->
            new MerchantOffer(
                getItemCost(offer.getBaseCostA()),
                getOptionalItemCost(offer.getCostB()),
                offer.getResult(),
                offer.getUses(),
                offer.getMaxUses(),
                xp,
                offer.getPriceMultiplier(),
                offer.getDemand()));
  }

  public static void setAdvancedTradingPriceMultiplier(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, float priceMultiplier) {
    updateTradingOffer(
        tradingData,
        tradingOfferIndex,
        offer ->
            new MerchantOffer(
                getItemCost(offer.getBaseCostA()),
                getOptionalItemCost(offer.getCostB()),
                offer.getResult(),
                offer.getUses(),
                offer.getMaxUses(),
                offer.getXp(),
                priceMultiplier,
                offer.getDemand()));
  }

  public static void setAdvancedTradingDemand(
      TradingDataCapable<?> tradingData, int tradingOfferIndex, int demand) {
    updateTradingOffer(
        tradingData,
        tradingOfferIndex,
        offer ->
            new MerchantOffer(
                getItemCost(offer.getBaseCostA()),
                getOptionalItemCost(offer.getCostB()),
                offer.getResult(),
                offer.getUses(),
                offer.getMaxUses(),
                offer.getXp(),
                offer.getPriceMultiplier(),
                demand));
  }

  public static void updateBasicTradingOffers(TradingDataCapable<?> tradingData) {
    if (tradingData.getTradingDataSet().isType(TradingType.BASIC)) {
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
              getItemCost(merchantOffer.getBaseCostA()),
              getOptionalItemCost(merchantOffer.getCostB()),
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
