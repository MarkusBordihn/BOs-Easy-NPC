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

package de.markusbordihn.easynpc.entity.data;

import de.markusbordihn.easynpc.data.entity.CustomDataSerializers;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import de.markusbordihn.easynpc.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

public interface EntityTradingData extends EntityDataInterface {

  // Synced entity data
  EntityDataAccessor<CompoundTag> DATA_TRADING_INVENTORY =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.COMPOUND_TAG);
  EntityDataAccessor<MerchantOffers> DATA_MERCHANT_OFFERS =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.MERCHANT_OFFERS);
  EntityDataAccessor<TradingType> DATA_TRADING_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.TRADING_TYPE);
  EntityDataAccessor<Integer> DATA_TRADING_RESETS_EVERY_MIN =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.INT);
  EntityDataAccessor<Integer> DATA_TRADING_BASIC_MAX_USES =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.INT);
  EntityDataAccessor<Integer> DATA_TRADING_BASIC_REWARDED_XP =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.INT);

  // CompoundTags
  String DATA_TRADING_INVENTORY_TAG = "Inventory";
  String DATA_TRADING_OFFERS_TAG = "Offers";
  String DATA_TRADING_RECIPES_TAG = "Recipes";
  String DATA_TRADING_TYPE_TAG = "TradingType";
  String DATA_TRADING_RESETS_EVERY_MIN_TAG = "ResetsEveryMin";
  String DATA_TRADING_BASIC_MAX_USES_TAG = "BasicMaxUses";
  String DATA_TRADING_BASIC_REWARDED_XP_TAG = "BasicRewardedXP";

  void updateTradesData();

  default MerchantOffers getTradingOffers() {
    return getEntityData(DATA_MERCHANT_OFFERS);
  }

  default void setTradingOffers(MerchantOffers merchantOffers) {
    // Force update and client sync because of weak change detection.
    setEntityData(DATA_MERCHANT_OFFERS, new MerchantOffers());
    setEntityData(DATA_MERCHANT_OFFERS, merchantOffers);
    this.updateTradesData();
  }

  default void setAdvancedTradingOffers(Container container) {

    // Update trading offers with container items.
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int tradingOffer = 0;
        tradingOffer < AdvancedTradingConfigurationMenu.TRADING_OFFERS;
        tradingOffer++) {
      ItemStack itemA = container.getItem(tradingOffer * 3);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);

      // Check if we have existing trading offers and use them as base for the new trading offers.
      MerchantOffers existingMerchantOffers = this.getTradingOffers();
      MerchantOffer existingMerchantOffer =
          existingMerchantOffers != null && existingMerchantOffers.size() > tradingOffer
              ? existingMerchantOffers.get(tradingOffer)
              : null;
      if (existingMerchantOffer != null) {
        merchantOffers.add(
            tradingOffer,
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
        merchantOffers.add(tradingOffer, new MerchantOffer(itemA, itemB, itemResult, 64, 1, 1.0F));
      }
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      this.setTradingType(TradingType.ADVANCED);
      this.setTradingOffers(merchantOffers);
    }
  }

  default void setBasicTradingOffers(Container container) {

    // Create new trading offers based on the container and number of trading offers.
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int tradingOffer = 0;
        tradingOffer < BasicTradingConfigurationMenu.TRADING_OFFERS;
        tradingOffer++) {
      ItemStack itemA = container.getItem(tradingOffer * 3);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);

      MerchantOffer merchantOffer =
          new MerchantOffer(
              itemA,
              itemB,
              itemResult,
              this.getBasicTradingMaxUses(),
              this.getBasicTradingRewardExp(),
              1.0F);
      merchantOffers.add(merchantOffer);
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      this.setTradingType(TradingType.BASIC);
      this.setTradingOffers(merchantOffers);
    }
  }

  default void updateBasicTradingOffers() {
    if (this.getTradingType() != TradingType.BASIC) {
      return;
    }

    MerchantOffers merchantOffers = this.getTradingOffers();
    if (merchantOffers == null || merchantOffers.isEmpty()) {
      return;
    }

    // Update trading offers
    MerchantOffers newMerchantOffers = new MerchantOffers();
    for (MerchantOffer merchantOffer : merchantOffers) {
      MerchantOffer newMerchantOffer =
          new MerchantOffer(
              merchantOffer.getBaseCostA(),
              merchantOffer.getCostB(),
              merchantOffer.getResult(),
              this.getBasicTradingMaxUses(),
              this.getBasicTradingRewardExp(),
              merchantOffer.getPriceMultiplier());
      newMerchantOffers.add(newMerchantOffer);
    }

    // Update trading offers
    this.setTradingOffers(newMerchantOffers);
  }

  default void resetTradingOffers() {
    MerchantOffers merchantOffers = this.getTradingOffers();
    if (merchantOffers == null || merchantOffers.isEmpty()) {
      return;
    }

    // Reset trading offers
    for (MerchantOffer merchantOffer : merchantOffers) {
      merchantOffer.resetUses();
    }

    // Update trading offers
    this.setTradingOffers(merchantOffers);
  }

  default CompoundTag getTradingInventory() {
    return getEntityData(DATA_TRADING_INVENTORY);
  }

  default void setTradingInventory(CompoundTag tradingInventory) {
    setEntityData(DATA_TRADING_INVENTORY, tradingInventory);
  }

  default TradingType getTradingType() {
    return getEntityData(DATA_TRADING_TYPE);
  }

  default void setTradingType(TradingType tradingType) {
    setEntityData(DATA_TRADING_TYPE, tradingType);
  }

  default boolean hasTrading() {
    return ((getTradingType() == TradingType.BASIC || getTradingType() == TradingType.ADVANCED)
        && getEntityData(DATA_MERCHANT_OFFERS) != null
        && !getEntityData(DATA_MERCHANT_OFFERS).isEmpty())
        || getTradingType() == TradingType.CUSTOM;
  }

  default void setAdvancedTradingMaxUses(int tradingOfferIndex, int maxUses) {
    MerchantOffers merchantOffers = getTradingOffers();
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
    this.setTradingOffers(merchantOffers);
  }

  default void setAdvancedTradingXp(int tradingOfferIndex, int xp) {
    MerchantOffers merchantOffers = getTradingOffers();
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
    this.setTradingOffers(merchantOffers);
  }

  default void setAdvancedTradingPriceMultiplier(int tradingOfferIndex, float priceMultiplier) {
    MerchantOffers merchantOffers = getTradingOffers();
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
    this.setTradingOffers(merchantOffers);
  }

  default void setAdvancedTradingDemand(int tradingOfferIndex, int demand) {
    MerchantOffers merchantOffers = getTradingOffers();
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
    this.setTradingOffers(merchantOffers);
  }

  default int getBasicTradingMaxUses() {
    return getEntityData(DATA_TRADING_BASIC_MAX_USES);
  }

  default void setBasicTradingMaxUses(int maxUses) {
    setEntityData(DATA_TRADING_BASIC_MAX_USES, maxUses);
  }

  default int getBasicTradingRewardExp() {
    return getEntityData(DATA_TRADING_BASIC_REWARDED_XP);
  }

  default void setBasicTradingRewardExp(int rewardExp) {
    setEntityData(DATA_TRADING_BASIC_REWARDED_XP, rewardExp);
  }

  default int getTradingResetsEveryMin() {
    return getEntityData(DATA_TRADING_RESETS_EVERY_MIN);
  }

  default void setTradingResetsEveryMin(int resetsEveryMin) {
    setEntityData(DATA_TRADING_RESETS_EVERY_MIN, resetsEveryMin);
  }

  default void defineSynchedTradingData() {
    defineEntityData(DATA_TRADING_INVENTORY, new CompoundTag());
    defineEntityData(DATA_MERCHANT_OFFERS, new MerchantOffers());
    defineEntityData(DATA_TRADING_TYPE, TradingType.NONE);
    defineEntityData(DATA_TRADING_RESETS_EVERY_MIN, 0);
    defineEntityData(DATA_TRADING_BASIC_MAX_USES, 64);
    defineEntityData(DATA_TRADING_BASIC_REWARDED_XP, 1);
  }

  default void addAdditionalTradingData(CompoundTag compoundTag) {
    CompoundTag tradingTag = new CompoundTag();

    tradingTag.put(DATA_TRADING_INVENTORY_TAG, getTradingInventory());
    tradingTag.put(DATA_TRADING_RECIPES_TAG, getTradingOffers().createTag());
    tradingTag.putString(DATA_TRADING_TYPE_TAG, getTradingType().name());
    tradingTag.putInt(DATA_TRADING_RESETS_EVERY_MIN_TAG, getTradingResetsEveryMin());
    tradingTag.putInt(DATA_TRADING_BASIC_MAX_USES_TAG, getBasicTradingMaxUses());
    tradingTag.putInt(DATA_TRADING_BASIC_REWARDED_XP_TAG, getBasicTradingRewardExp());

    compoundTag.put(DATA_TRADING_OFFERS_TAG, tradingTag);
  }

  default void readAdditionalTradingData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_TRADING_OFFERS_TAG)) {
      return;
    }
    CompoundTag tradingTag = compoundTag.getCompound(DATA_TRADING_OFFERS_TAG);

    String tradingType = tradingTag.getString(DATA_TRADING_TYPE_TAG);
    if (tradingType != null && !tradingType.isEmpty()) {
      this.setTradingType(TradingType.get(tradingType));
    }

    if (tradingTag.contains(DATA_TRADING_RESETS_EVERY_MIN_TAG)) {
      this.setTradingResetsEveryMin(tradingTag.getInt(DATA_TRADING_RESETS_EVERY_MIN_TAG));
    }

    if (tradingTag.contains(DATA_TRADING_BASIC_MAX_USES_TAG)) {
      this.setBasicTradingMaxUses(tradingTag.getInt(DATA_TRADING_BASIC_MAX_USES_TAG));
    }

    if (tradingTag.contains(DATA_TRADING_BASIC_REWARDED_XP_TAG)) {
      this.setBasicTradingRewardExp(tradingTag.getInt(DATA_TRADING_BASIC_REWARDED_XP_TAG));
    }

    if (tradingTag.contains(DATA_TRADING_RECIPES_TAG)) {
      MerchantOffers merchantOffers =
          new MerchantOffers(tradingTag.getCompound(DATA_TRADING_RECIPES_TAG));
      if (!merchantOffers.isEmpty()) {
        this.setTradingOffers(merchantOffers);
      }
      return;
    }

    if (tradingTag.contains(DATA_TRADING_INVENTORY_TAG)) {
      setTradingInventory(tradingTag.getCompound(DATA_TRADING_INVENTORY_TAG));
    }
  }
}
