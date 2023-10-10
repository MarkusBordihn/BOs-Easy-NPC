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

package de.markusbordihn.easynpc.entity.data;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;

public interface EntityTradingData extends EntityDataInterface {

  // Synced entity data
  public static final EntityDataAccessor<CompoundTag> DATA_TRADING_INVENTORY =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.COMPOUND_TAG);
  public static final EntityDataAccessor<MerchantOffers> DATA_MERCHANT_OFFERS =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.MERCHANT_OFFERS);
  public static final EntityDataAccessor<TradingType> DATA_TRADING_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.TRADING_TYPE);

  // CompoundTags
  public static final String DATA_TRADING_INVENTORY_TAG = "Inventory";
  public static final String DATA_TRADING_OFFERS_TAG = "Offers";
  public static final String DATA_TRADING_RECIPES_TAG = "Recipes";
  public static final String DATA_TRADING_TYPE_TAG = "TradingType";

  default MerchantOffers getTradingOffers() {
    return getEntityData(DATA_MERCHANT_OFFERS);
  }

  default void setTradingOffers(MerchantOffers merchantOffers) {
    setEntityData(DATA_MERCHANT_OFFERS, merchantOffers);
  }

  default void setBasicTradingOffers(Container container) {

    // Create new trading offers based on the container and number of trading offers.
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int tradingOffer =
        0; tradingOffer < BasicTradingConfigurationMenu.TRADING_OFFERS; tradingOffer++) {
      ItemStack itemA = container.getItem(tradingOffer * 3 + 0);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);

      // Skip empty offers
      if ((itemA.isEmpty() && itemB.isEmpty()) || itemResult.isEmpty()) {
        continue;
      }
      MerchantOffer merchantOffer = new MerchantOffer(itemA, itemB, itemResult, 16, 64, 2.0F);
      merchantOffers.add(merchantOffer);
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      this.setTradingOffers(merchantOffers);
      this.setTradingType(TradingType.BASIC);
      this.updateTradesData();
    }
  }

  default void updateTradesData() {

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
    return (getTradingType() == TradingType.BASIC && !getEntityData(DATA_MERCHANT_OFFERS).isEmpty())
        || (getTradingType() == TradingType.ADVANCED
            && !getEntityData(DATA_TRADING_INVENTORY).isEmpty())
        || getTradingType() == TradingType.CUSTOM;
  }

  default void defineSynchedTradingData() {
    defineEntityData(DATA_TRADING_INVENTORY, new CompoundTag());
    defineEntityData(DATA_MERCHANT_OFFERS, new MerchantOffers());
    defineEntityData(DATA_TRADING_TYPE, TradingType.NONE);
  }

  default void addAdditionalTradingData(CompoundTag compoundTag) {
    CompoundTag tradingTag = new CompoundTag();

    tradingTag.put(DATA_TRADING_INVENTORY_TAG, getTradingInventory());
    tradingTag.put(DATA_TRADING_RECIPES_TAG, getTradingOffers().createTag());
    tradingTag.putString(DATA_TRADING_TYPE_TAG, getTradingType().name());

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
