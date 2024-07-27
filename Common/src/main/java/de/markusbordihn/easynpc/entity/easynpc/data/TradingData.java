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

package de.markusbordihn.easynpc.entity.easynpc.data;

import com.mojang.serialization.DataResult;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.trading.TradingSettings;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.EnumMap;
import java.util.Optional;
import net.minecraft.Util;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

public interface TradingData<E extends PathfinderMob> extends EasyNPC<E>, Merchant {

  String DATA_TRADING_TAG = "TradingData";
  String DATA_TRADING_BASIC_MAX_USES_TAG = "BasicMaxUses";
  String DATA_TRADING_BASIC_REWARDED_XP_TAG = "BasicRewardedXP";
  String DATA_OFFERS_TAG = "Offers";
  String DATA_TRADING_RESETS_EVERY_MIN_TAG = "ResetsEveryMin";
  String DATA_TRADING_TYPE_TAG = "TradingType";

  static void registerSyncedTradingData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Trading Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.TRADING_INVENTORY,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.COMPOUND_TAG));
    map.put(
        SynchedDataIndex.TRADING_MERCHANT_OFFERS,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.MERCHANT_OFFERS));
    map.put(
        SynchedDataIndex.TRADING_TYPE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.TRADING_TYPE));
    map.put(
        SynchedDataIndex.TRADING_RESETS_EVERY_MIN,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.INT));
    map.put(
        SynchedDataIndex.TRADING_BASIC_MAX_USES,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.INT));
    map.put(
        SynchedDataIndex.TRADING_BASIC_REWARDED_XP,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.INT));
  }

  private static ItemCost getItemCost(ItemStack itemStack) {
    return new ItemCost(
        itemStack.isEmpty() ? ItemStack.EMPTY.getItem() : itemStack.getItem(),
        itemStack.getCount());
  }

  private static Optional<ItemCost> getOptionalItemCost(ItemStack itemStack) {
    return itemStack.isEmpty() ? Optional.empty() : Optional.of(getItemCost(itemStack));
  }

  void updateTradesData();

  Player getTradingPlayer();

  void setTradingPlayer(Player player);

  MerchantOffers getOffers();

  @Override
  default void overrideOffers(MerchantOffers merchantOffers) {
    /* Method is not used */
  }

  void notifyTrade(MerchantOffer merchantOffer);

  void notifyTradeUpdated(ItemStack itemStack);

  @Override
  default int getVillagerXp() {
    return 0;
  }

  @Override
  default void overrideXp(int experience) {
    /* Method is not used */
  }

  @Override
  default boolean showProgressBar() {
    return true;
  }

  @Override
  default SoundEvent getNotifyTradeSound() {
    return SoundEvents.VILLAGER_YES;
  }

  @Override
  default boolean isClientSide() {
    return this.getLevel() != null && this.getLevel().isClientSide();
  }

  default void setAdvancedTradingOffers(Container container) {

    log.debug("Setting advanced trading offers for {} with container {}", this, container);
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int tradingOffer = 0;
        tradingOffer < TradingSettings.ADVANCED_TRADING_OFFERS;
        tradingOffer++) {

      // Check if we have a valid trading offer.
      ItemStack itemA = container.getItem(tradingOffer * 3);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);
      if ((itemA.isEmpty() && itemB.isEmpty()) || itemResult.isEmpty()) {
        continue;
      }

      // Calculate item costs based on item A and item B.
      ItemCost itemCostA = getItemCost(itemA);
      Optional<ItemCost> itemCostB = getOptionalItemCost(itemB);

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
            tradingOffer, new MerchantOffer(itemCostA, itemCostB, itemResult, 64, 1, 1.0F));
      }
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      this.setTradingType(TradingType.ADVANCED);
      this.setTradingOffers(merchantOffers);
    }
  }

  default void setBasicTradingOffers(Container container) {

    log.debug("Setting basic trading offers for {} with container {}", this, container);
    MerchantOffers merchantOffers = new MerchantOffers();
    for (int tradingOffer = 0;
        tradingOffer < TradingSettings.BASIC_TRADING_OFFERS;
        tradingOffer++) {

      // Check if we have a valid trading offer.
      ItemStack itemA = container.getItem(tradingOffer * 3);
      ItemStack itemB = container.getItem(tradingOffer * 3 + 1);
      ItemStack itemResult = container.getItem(tradingOffer * 3 + 2);
      if ((itemA.isEmpty() && itemB.isEmpty()) || itemResult.isEmpty()) {
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
              getItemCost(merchantOffer.getBaseCostA()),
              getOptionalItemCost(merchantOffer.getCostB()),
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

  default MerchantOffers getTradingOffers() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS);
  }

  default void setTradingOffers(MerchantOffers merchantOffers) {
    // Force update and client sync because of weak change detection.
    setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, new MerchantOffers());
    setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, merchantOffers);
    this.updateTradesData();
  }

  default TradingType getTradingType() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_TYPE);
  }

  default void setTradingType(TradingType tradingType) {
    setSynchedEntityData(SynchedDataIndex.TRADING_TYPE, tradingType);
  }

  default boolean hasTrading() {
    return ((getTradingType() == TradingType.BASIC || getTradingType() == TradingType.ADVANCED)
            && getTradingOffers() != null
            && !getTradingOffers().isEmpty())
        || getTradingType() == TradingType.CUSTOM;
  }

  default void stopTrading() {
    Merchant merchant = this.getMerchant();
    if (merchant != null) {
      merchant.setTradingPlayer(null);
    }
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
            getItemCost(merchantOffer.getBaseCostA()),
            getOptionalItemCost(merchantOffer.getCostB()),
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
            getItemCost(merchantOffer.getBaseCostA()),
            getOptionalItemCost(merchantOffer.getCostB()),
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
            getItemCost(merchantOffer.getBaseCostA()),
            getOptionalItemCost(merchantOffer.getCostB()),
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
            getItemCost(merchantOffer.getBaseCostA()),
            getOptionalItemCost(merchantOffer.getCostB()),
            merchantOffer.getResult(),
            merchantOffer.getUses(),
            merchantOffer.getMaxUses(),
            merchantOffer.getXp(),
            merchantOffer.getPriceMultiplier(),
            demand));
    this.setTradingOffers(merchantOffers);
  }

  default int getBasicTradingMaxUses() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_BASIC_MAX_USES);
  }

  default void setBasicTradingMaxUses(int maxUses) {
    setSynchedEntityData(SynchedDataIndex.TRADING_BASIC_MAX_USES, maxUses);
  }

  default int getBasicTradingRewardExp() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_BASIC_REWARDED_XP);
  }

  default void setBasicTradingRewardExp(int rewardExp) {
    setSynchedEntityData(SynchedDataIndex.TRADING_BASIC_REWARDED_XP, rewardExp);
  }

  default int getTradingResetsEveryMin() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_RESETS_EVERY_MIN);
  }

  default void setTradingResetsEveryMin(int resetsEveryMin) {
    setSynchedEntityData(SynchedDataIndex.TRADING_RESETS_EVERY_MIN, resetsEveryMin);
  }

  default InteractionResult openTradingScreen(ServerPlayer serverPlayer) {
    if (!this.isClientSide()) {
      Merchant merchant = this.getMerchant();
      if (merchant == null) {
        log.error(
            "No merchant found for {} with {} from {}",
            this,
            this.getTradingOffers(),
            serverPlayer);
        return InteractionResult.PASS;
      }
      if (merchant.getTradingPlayer() != null && merchant.getTradingPlayer() != serverPlayer) {
        log.warn(
            "Unable to open trading screen for {} with {} from {}, {} is still trading.",
            this,
            merchant.getOffers(),
            serverPlayer,
            merchant.getTradingPlayer());
        serverPlayer.sendSystemMessage(
            Component.translatable(
                Constants.TEXT_PREFIX + "trading.busy", merchant.getTradingPlayer()));
        return InteractionResult.PASS;
      }
      log.debug(
          "Open trading screen for {} with {} from {}", this, merchant.getOffers(), serverPlayer);
      merchant.setTradingPlayer(serverPlayer);
      merchant.openTradingScreen(
          serverPlayer,
          this.getEntity().getCustomName() != null
              ? this.getEntity().getCustomName()
              : Component.translatable(Constants.TEXT_PREFIX + "trading"),
          Entity.BASE_TICKS_REQUIRED_TO_FREEZE);
    }
    return InteractionResult.sidedSuccess(this.isClientSide());
  }

  default void defineSynchedTradingData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_INVENTORY, new CompoundTag());
    defineSynchedEntityData(
        builder, SynchedDataIndex.TRADING_MERCHANT_OFFERS, new MerchantOffers());
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_TYPE, TradingType.NONE);
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_RESETS_EVERY_MIN, 0);
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_BASIC_MAX_USES, 64);
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_BASIC_REWARDED_XP, 1);
  }

  default void addAdditionalTradingData(CompoundTag compoundTag, HolderLookup.Provider provider) {
    // Add trading data to the compound tag.
    CompoundTag tradingDataTag = new CompoundTag();
    tradingDataTag.putString(DATA_TRADING_TYPE_TAG, getTradingType().name());
    tradingDataTag.putInt(DATA_TRADING_RESETS_EVERY_MIN_TAG, getTradingResetsEveryMin());
    tradingDataTag.putInt(DATA_TRADING_BASIC_MAX_USES_TAG, getBasicTradingMaxUses());
    tradingDataTag.putInt(DATA_TRADING_BASIC_REWARDED_XP_TAG, getBasicTradingRewardExp());
    compoundTag.put(DATA_TRADING_TAG, tradingDataTag);

    // Add trading offers to the compound tag.
    MerchantOffers merchantOffers = getTradingOffers();
    if (merchantOffers != null && !merchantOffers.isEmpty()) {
      compoundTag.put(
          DATA_OFFERS_TAG,
          MerchantOffers.CODEC
              .encodeStart(provider.createSerializationContext(NbtOps.INSTANCE), merchantOffers)
              .getOrThrow());
    }
  }

  default void readAdditionalTradingData(CompoundTag compoundTag, HolderLookup.Provider provider) {

    // Read trading data from the compound tag.
    if (!compoundTag.contains(DATA_TRADING_TAG)) {
      return;
    }
    CompoundTag tradingDataTag = compoundTag.getCompound(DATA_TRADING_TAG);
    String tradingType = tradingDataTag.getString(DATA_TRADING_TYPE_TAG);
    if (!tradingType.isEmpty()) {
      this.setTradingType(TradingType.get(tradingType));
    }
    if (tradingDataTag.contains(DATA_TRADING_RESETS_EVERY_MIN_TAG)) {
      this.setTradingResetsEveryMin(tradingDataTag.getInt(DATA_TRADING_RESETS_EVERY_MIN_TAG));
    }
    if (tradingDataTag.contains(DATA_TRADING_BASIC_MAX_USES_TAG)) {
      this.setBasicTradingMaxUses(tradingDataTag.getInt(DATA_TRADING_BASIC_MAX_USES_TAG));
    }
    if (tradingDataTag.contains(DATA_TRADING_BASIC_REWARDED_XP_TAG)) {
      this.setBasicTradingRewardExp(tradingDataTag.getInt(DATA_TRADING_BASIC_REWARDED_XP_TAG));
    }

    // Read trading offers from the compound tag.
    if (!compoundTag.contains(DATA_OFFERS_TAG)) {
      return;
    }
    DataResult<MerchantOffers> dataResult =
        MerchantOffers.CODEC.parse(
            provider.createSerializationContext(NbtOps.INSTANCE), compoundTag.get(DATA_OFFERS_TAG));
    dataResult
        .resultOrPartial(Util.prefix("Failed to load offers: ", log::warn))
        .ifPresent(this::setTradingOffers);
  }
}
