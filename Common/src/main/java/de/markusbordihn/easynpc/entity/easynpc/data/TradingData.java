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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import de.markusbordihn.easynpc.data.trading.TradingSettings;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

public interface TradingData<E extends PathfinderMob> extends EasyNPC<E>, Merchant {

  String DATA_TRADING_INVENTORY_TAG = "Inventory";
  String DATA_TRADING_OFFERS_TAG = "Offers";
  String DATA_TRADING_RECIPES_TAG = "Recipes";
  String DATA_TRADING_DATA_TAG = "TradingData";

  EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, MerchantOffers value) {
          buffer.writeNbt(value.createTag());
        }

        public MerchantOffers read(FriendlyByteBuf buffer) {
          CompoundTag compoundTag = buffer.readNbt();
          return compoundTag != null ? new MerchantOffers(compoundTag) : new MerchantOffers();
        }

        public MerchantOffers copy(MerchantOffers value) {
          return value;
        }
      };
  EntityDataSerializer<TradingDataSet> TRADING_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, TradingDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public TradingDataSet read(FriendlyByteBuf buffer) {
          return new TradingDataSet(buffer.readNbt());
        }

        public TradingDataSet copy(TradingDataSet value) {
          return value;
        }
      };

  static void registerSyncedTradingData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Trading Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.TRADING_DATA_SET,
        SynchedEntityData.defineId(entityClass, TRADING_DATA_SET));
    map.put(
        SynchedDataIndex.TRADING_INVENTORY,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.COMPOUND_TAG));
    map.put(
        SynchedDataIndex.TRADING_MERCHANT_OFFERS,
        SynchedEntityData.defineId(entityClass, MERCHANT_OFFERS));
  }

  static void registerTradingDataSerializer() {
    EntityDataSerializers.registerSerializer(TRADING_DATA_SET);
    EntityDataSerializers.registerSerializer(MERCHANT_OFFERS);
  }

  @Override
  Player getTradingPlayer();

  @Override
  void setTradingPlayer(Player player);

  MerchantOffers getMerchantTradingOffers();

  void setMerchantTradingOffers(MerchantOffers merchantOffers);

  default MerchantOffers getOffers() {
    if (this.getMerchantTradingOffers() == null) {
      this.updateMerchantTradingOffers();
    }
    return this.getMerchantTradingOffers();
  }

  default void updateMerchantTradingOffers() {
    MerchantOffers merchantOffers = new MerchantOffers();
    TradingDataSet tradingDataSet = this.getTradingDataSet();
    if (tradingDataSet.isType(TradingType.BASIC) || tradingDataSet.isType(TradingType.ADVANCED)) {
      // Create a copy of the offers to avoid side effects.
      merchantOffers = new MerchantOffers(this.getTradingOffers().createTag());
    }
    if (!merchantOffers.isEmpty()) {
      // Filter out offers which are missing item a, item b or result item.
      merchantOffers.removeIf(
          merchantOffer ->
              (merchantOffer.getBaseCostA().isEmpty() && merchantOffer.getCostB().isEmpty())
                  || merchantOffer.getResult().isEmpty());
    }
    this.setMerchantTradingOffers(merchantOffers);
  }

  @Override
  default void overrideOffers(MerchantOffers merchantOffers) {
    /* Method is not used */
  }

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

      // Check if we have existing trading offers and use them as base for the new trading offers.
      MerchantOffers existingMerchantOffers = this.getTradingOffers();
      MerchantOffer existingMerchantOffer =
          existingMerchantOffers != null && existingMerchantOffers.size() > tradingOffer
              ? existingMerchantOffers.get(tradingOffer)
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

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      getTradingDataSet().setType(TradingType.ADVANCED);
      this.setTradingOffers(merchantOffers);
    }
  }

  default void setBasicTradingOffers(Container container) {

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

      MerchantOffer merchantOffer =
          new MerchantOffer(
              itemA,
              itemB,
              itemResult,
              getTradingDataSet().getMaxUses(),
              getTradingDataSet().getRewardedXP(),
              1.0F);
      merchantOffers.add(merchantOffer);
    }

    // Set trading offers if we have any
    if (!merchantOffers.isEmpty()) {
      getTradingDataSet().setType(TradingType.BASIC);
      this.setTradingOffers(merchantOffers);
    }
  }

  default void updateBasicTradingOffers() {
    if (getTradingDataSet().isType(TradingType.BASIC)) {
      return;
    }

    MerchantOffers merchantOffers = this.getTradingOffers();
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
              getTradingDataSet().getMaxUses(),
              getTradingDataSet().getRewardedXP(),
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

    log.debug("Reset trading offers {} for {}", merchantOffers, this);

    // Reset trading offers
    for (MerchantOffer merchantOffer : merchantOffers) {
      merchantOffer.resetUses();
    }

    // Update trading offers
    this.setTradingOffers(merchantOffers);

    // Update last reset time
    this.getTradingDataSet().setLastReset(System.currentTimeMillis());
  }

  default MerchantOffers getTradingOffers() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS);
  }

  default void setTradingOffers(MerchantOffers merchantOffers) {
    // Force update and client sync because of weak change detection.
    setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, new MerchantOffers());
    setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, merchantOffers);
    this.updateMerchantTradingOffers();
  }

  default CompoundTag getTradingInventory() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_INVENTORY);
  }

  default void setTradingInventory(CompoundTag tradingInventory) {
    setSynchedEntityData(SynchedDataIndex.TRADING_INVENTORY, tradingInventory);
  }

  default void notifyTrade(MerchantOffer merchantOffer) {
    merchantOffer.increaseUses();
    this.getMob().ambientSoundTime = -this.getMob().getAmbientSoundInterval();
    this.rewardTradeXp(merchantOffer);
    if (getTradingPlayer() instanceof ServerPlayer serverPlayer) {
      log.debug("Trade {} with {} for {}", merchantOffer, serverPlayer, this);
    }
  }

  default void notifyTradeUpdated(ItemStack itemStack) {
    if (!this.isClientSide()
        && this.getMob().ambientSoundTime > -this.getMob().getAmbientSoundInterval() + 20) {
      this.getMob().ambientSoundTime = -this.getMob().getAmbientSoundInterval();
      SoundData<E> soundData = getEasyNPCSoundData();
      soundData.playDefaultTradeUpdatedSound(!itemStack.isEmpty());
    }
  }

  default void rewardTradeXp(MerchantOffer merchantOffer) {
    if (merchantOffer.shouldRewardExp() && merchantOffer.getXp() > 0) {
      LivingEntity livingEntity = this.getLivingEntity();
      int tradeExperience = 3 + livingEntity.getRandom().nextInt(merchantOffer.getXp());
      livingEntity
          .level()
          .addFreshEntity(
              new ExperienceOrb(
                  livingEntity.level(),
                  livingEntity.getX(),
                  livingEntity.getY() + 0.5D,
                  livingEntity.getZ(),
                  tradeExperience));
    }
  }

  default boolean hasTradingData() {
    TradingType tradingType = getTradingDataSet().getType();
    return ((tradingType == TradingType.BASIC || tradingType == TradingType.ADVANCED)
            && getTradingOffers() != null
            && !getTradingOffers().isEmpty())
        || tradingType == TradingType.CUSTOM;
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

  default TradingDataSet getTradingDataSet() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_DATA_SET);
  }

  default void setTradingDataSet(TradingDataSet tradingDataSet) {
    setSynchedEntityData(SynchedDataIndex.TRADING_DATA_SET, tradingDataSet);
  }

  default boolean isValidTradingOffer(ItemStack itemA, ItemStack itemB, ItemStack itemResult) {
    if (itemResult == null || (itemA == null && itemB == null)) {
      return false;
    }
    return ((itemA != null && !itemA.isEmpty()) || (itemB != null && !itemB.isEmpty()))
        && !itemResult.isEmpty();
  }

  default InteractionResult openTradingScreen(ServerPlayer serverPlayer) {
    if (this.isClientSide()) {
      return InteractionResult.SUCCESS;
    }

    // Make sure we have a valid merchant.
    Merchant merchant = this.getMerchant();
    if (merchant == null) {
      log.error(
          "No merchant found for {} with {} from {}", this, this.getTradingOffers(), serverPlayer);
      return InteractionResult.FAIL;
    }

    // Verify that we have trading offers.
    MerchantOffers merchantOffers = merchant.getOffers();
    if (merchantOffers.isEmpty()) {
      log.error(
          "No trading offers found for {} with {} from {}", this, merchantOffers, serverPlayer);
      return InteractionResult.FAIL;
    }

    // Check if player is already trading.
    if (merchant.getTradingPlayer() != null && merchant.getTradingPlayer() != serverPlayer) {
      log.warn(
          "Unable to open trading screen for {} with {} from {}, {} is still trading.",
          this,
          merchantOffers,
          serverPlayer,
          merchant.getTradingPlayer());
      serverPlayer.sendSystemMessage(
          Component.translatable(
              Constants.TEXT_PREFIX + "trading.busy", merchant.getTradingPlayer()));
      return InteractionResult.FAIL;
    }

    // Check if trades should be reset.
    if (this.getTradingDataSet().getResetsEveryMin() > 0) {
      long currentTime = System.currentTimeMillis();
      long resetTimeInMillis = this.getTradingDataSet().getResetsEveryMin() * 60L * 1000L;
      if (currentTime - this.getTradingDataSet().getLastReset() > resetTimeInMillis) {
        this.resetTradingOffers();
      }
    }

    // Open trading screen for the player.
    log.debug("Open trading screen for {} with {} from {}", this, merchantOffers, serverPlayer);
    merchant.setTradingPlayer(serverPlayer);
    merchant.openTradingScreen(
        serverPlayer,
        this.getEntity().getCustomName() != null
            ? this.getEntity().getCustomName()
            : Component.translatable(Constants.TEXT_PREFIX + "trading"),
        Entity.BASE_TICKS_REQUIRED_TO_FREEZE);

    return InteractionResult.CONSUME;
  }

  default void defineSynchedTradingData() {
    defineSynchedEntityData(SynchedDataIndex.TRADING_DATA_SET, new TradingDataSet());
    defineSynchedEntityData(SynchedDataIndex.TRADING_INVENTORY, new CompoundTag());
    defineSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, new MerchantOffers());
  }

  default void addAdditionalTradingData(CompoundTag compoundTag) {
    // Save custom trading data set
    CompoundTag tradingDataTag = new CompoundTag();
    TradingDataSet tradingDataSet = this.getTradingDataSet();
    if (tradingDataSet != null) {
      tradingDataSet.save(tradingDataTag);
    }
    compoundTag.put(DATA_TRADING_DATA_TAG, tradingDataTag);

    // Store vanilla trading data
    CompoundTag tradingOffersTag = new CompoundTag();
    tradingOffersTag.put(DATA_TRADING_INVENTORY_TAG, getTradingInventory());
    tradingOffersTag.put(DATA_TRADING_RECIPES_TAG, getTradingOffers().createTag());
    compoundTag.put(DATA_TRADING_OFFERS_TAG, tradingOffersTag);
  }

  default void readAdditionalTradingData(CompoundTag compoundTag) {

    // Load custom trading data set
    CompoundTag tradingDataTag = compoundTag.getCompound(DATA_TRADING_DATA_TAG);
    if (tradingDataTag.contains(TradingDataSet.DATA_TRADING_DATA_SET_TAG)) {
      TradingDataSet tradingDataSet = new TradingDataSet(tradingDataTag);
      this.setTradingDataSet(tradingDataSet);
    }

    // Load vanilla trading data
    CompoundTag tradingOffersTag = compoundTag.getCompound(DATA_TRADING_OFFERS_TAG);
    if (tradingOffersTag.contains(DATA_TRADING_RECIPES_TAG)) {
      MerchantOffers merchantOffers =
          new MerchantOffers(tradingOffersTag.getCompound(DATA_TRADING_RECIPES_TAG));
      if (!merchantOffers.isEmpty()) {
        this.setTradingOffers(merchantOffers);
      }
      return;
    }
    if (tradingOffersTag.contains(DATA_TRADING_INVENTORY_TAG)) {
      setTradingInventory(tradingOffersTag.getCompound(DATA_TRADING_INVENTORY_TAG));
    }
  }
}
