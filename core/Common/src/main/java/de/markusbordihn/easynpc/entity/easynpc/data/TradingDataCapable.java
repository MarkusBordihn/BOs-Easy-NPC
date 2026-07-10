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

import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.TradingUtils;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;

public interface TradingDataCapable<E extends Mob> extends EasyNPC<E>, Merchant {

  String DATA_OFFERS_TAG = "Offers";
  String DATA_TRADING_DATA_TAG = "TradingData";

  Player getTradingPlayer();

  void setTradingPlayer(Player player);

  MerchantOffers getMerchantTradingOffers();

  void setMerchantTradingOffers(MerchantOffers merchantOffers);

  void rewardTradeXp(MerchantOffer merchantOffer);

  void stopTrading();

  @Override
  default MerchantOffers getOffers() {
    if (this.getMerchantTradingOffers() == null) {
      this.updateMerchantTradingOffers();
    }
    return this.getMerchantTradingOffers();
  }

  default void updateMerchantTradingOffers() {
    TradingDataSet tradingDataSet = this.getTradingDataSet();
    MerchantOffers merchantOffers = new MerchantOffers();
    if (tradingDataSet.isType(TradingType.BASIC)
        || tradingDataSet.isType(TradingType.ADVANCED)
        || tradingDataSet.isType(TradingType.CUSTOM)) {
      merchantOffers = TradingUtils.sanitizeTradingOffers(this.getTradingOffers().copy());
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
  default boolean isClientSideInstance() {
    return this.getEntityLevel() != null && this.getEntityLevel().isClientSide();
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

    // Update last reset time
    this.getTradingDataSet().setLastReset(System.currentTimeMillis());
  }

  default boolean resetExpiredTradingOffers() {
    TradingDataSet tradingDataSet = this.getTradingDataSet();
    if (tradingDataSet == null || tradingDataSet.getResetsEveryMin() <= 0) {
      return false;
    }

    MerchantOffers merchantOffers = this.getTradingOffers();
    if (merchantOffers == null || merchantOffers.isEmpty()) {
      return false;
    }

    long resetTimeInMillis = tradingDataSet.getResetsEveryMin() * 60L * 1000L;
    if (System.currentTimeMillis() - tradingDataSet.getLastReset() <= resetTimeInMillis) {
      return false;
    }

    this.resetTradingOffers();
    return true;
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

  default void notifyTrade(MerchantOffer merchantOffer) {
    merchantOffer.increaseUses();
    this.getMob().ambientSoundTime = -this.getMob().getAmbientSoundInterval();
    this.rewardTradeXp(merchantOffer);
    MerchantOffers updatedOffers = this.getMerchantTradingOffers();
    if (updatedOffers != null && !updatedOffers.isEmpty()) {
      this.setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, new MerchantOffers());
      this.setSynchedEntityData(SynchedDataIndex.TRADING_MERCHANT_OFFERS, updatedOffers);
    }
    if (this.getTradingPlayer() instanceof ServerPlayer serverPlayer) {
      log.debug("Trade {} with {} for {}", merchantOffer, serverPlayer, this);
      ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
      if (actionEventData != null) {
        actionEventData.handleActionEvent(ActionEventType.ON_TRADE, serverPlayer);
      }
      int offerIndex =
          this.getTradingOffers() != null ? this.getTradingOffers().indexOf(merchantOffer) : -1;
      if (offerIndex >= 0 && this.getTradingDataSet().hasOfferAction(offerIndex)) {
        var actionHandler = this.getEasyNPCActionHandler();
        if (actionHandler != null) {
          actionHandler.executeActions(
              this.getTradingDataSet().getOfferAction(offerIndex), serverPlayer);
        }
      }
    }
  }

  default void notifyTradeUpdated(ItemStack itemStack) {
    if (!this.isClientSideInstance()
        && this.getMob().ambientSoundTime > -this.getMob().getAmbientSoundInterval() + 20) {
      this.getMob().ambientSoundTime = -this.getMob().getAmbientSoundInterval();
      SoundDataCapable<E> soundData = getEasyNPCSoundData();
      soundData.playDefaultTradeUpdatedSound(!itemStack.isEmpty());
    }
  }

  default void rewardMerchantTradeXp(MerchantOffer merchantOffer) {
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

  default void stopMerchantTrading() {
    Merchant merchant = this.getMerchant();
    if (merchant != null) {
      merchant.setTradingPlayer(null);
    }
  }

  default TradingDataSet getTradingDataSet() {
    return getSynchedEntityData(SynchedDataIndex.TRADING_DATA_SET);
  }

  default void setTradingDataSet(TradingDataSet tradingDataSet) {
    setSynchedEntityData(SynchedDataIndex.TRADING_DATA_SET, tradingDataSet);
  }

  default void updateTradingDataSet() {
    TradingDataSet currentTradingDataSet = getTradingDataSet();
    setTradingDataSet(new TradingDataSet());
    setTradingDataSet(currentTradingDataSet);
  }

  default boolean isValidTradingOffer(ItemStack itemA, ItemStack itemB, ItemStack itemResult) {
    if (itemResult == null || (itemA == null && itemB == null)) {
      return false;
    }
    return ((itemA != null && !itemA.isEmpty()) || (itemB != null && !itemB.isEmpty()))
        && !itemResult.isEmpty();
  }

  default void openTradingScreen(ServerPlayer serverPlayer) {
    if (this.isClientSideInstance()) {
      return;
    }

    // Make sure we have a valid merchant.
    Merchant merchant = this.getMerchant();
    if (merchant == null) {
      log.error(
          "No merchant found for {} with {} from {}", this, this.getTradingOffers(), serverPlayer);
      return;
    }

    this.resetExpiredTradingOffers();

    MerchantOffers merchantOffers = merchant.getOffers();
    if (merchantOffers.isEmpty()) {
      log.error(
          "No trading offers found for {} with {} from {}", this, merchantOffers, serverPlayer);
      return;
    }

    // Check if player is already trading.
    if (merchant.getTradingPlayer() != null && merchant.getTradingPlayer() != serverPlayer) {
      log.warn(
          "Unable to open trading screen for {} with {} from {}, {} is still trading.",
          this,
          merchantOffers,
          serverPlayer,
          merchant.getTradingPlayer());
      serverPlayer.closeContainer();
      serverPlayer.sendSystemMessage(
          TextComponent.getTranslatedText(
              "trading.busy", this.getLivingEntity(), merchant.getTradingPlayer()));
      return;
    }

    merchant.setTradingPlayer(serverPlayer);
    merchant.openTradingScreen(
        serverPlayer,
        this.getEntity().getCustomName() != null
            ? this.getEntity().getCustomName()
            : TextComponent.getTranslatedText("trading"),
        Entity.BASE_TICKS_REQUIRED_TO_FREEZE);
  }

  default void defineSynchedTradingData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_DATA_SET, new TradingDataSet());
    defineSynchedEntityData(builder, SynchedDataIndex.TRADING_INVENTORY, new CompoundTag());
    defineSynchedEntityData(
        builder, SynchedDataIndex.TRADING_MERCHANT_OFFERS, new MerchantOffers());
  }

  default void addAdditionalTradingData(CompoundTag compoundTag, HolderLookup.Provider provider) {
    TradingDataSet tradingDataSet = this.getTradingDataSet();

    // Only save trading config when trading is enabled or per-offer actions are configured.
    if (tradingDataSet != null
        && (!tradingDataSet.isType(TradingType.NONE)
            || !tradingDataSet.getOfferActions().isEmpty())) {
      CompoundTag tradingDataTag = new CompoundTag();
      tradingDataSet.save(tradingDataTag);
      compoundTag.put(DATA_TRADING_DATA_TAG, tradingDataTag);
    }

    MerchantOffers merchantOffers = getTradingOffers();
    if (merchantOffers != null && !merchantOffers.isEmpty()) {
      compoundTag.put(
          DATA_OFFERS_TAG,
          MerchantOffers.CODEC
              .encodeStart(
                  provider.createSerializationContext(NbtOps.INSTANCE),
                  TradingUtils.sanitizeTradingOffers(merchantOffers.copy()))
              .getOrThrow());
    }
  }

  default void readAdditionalTradingData(CompoundTag compoundTag, HolderLookup.Provider provider) {

    // Load custom trading data set
    CompoundTag tradingDataTag = compoundTag.getCompound(DATA_TRADING_DATA_TAG);
    if (tradingDataTag.contains(TradingDataSet.DATA_TRADING_DATA_SET_TAG)) {
      try {
        TradingDataSet tradingDataSet = new TradingDataSet(tradingDataTag);
        this.setTradingDataSet(tradingDataSet);
      } catch (Exception e) {
        log.warn(
            "Failed to load trading data set for {} ({}): {}",
            this,
            this.getEntity().getUUID(),
            e.getMessage());
      }
    }

    // Load vanilla trading data with legacy migration and per-entry fallback
    if (compoundTag.contains(DATA_OFFERS_TAG)) {
      MerchantOffers offers =
          TradingUtils.parseMerchantOffers(
              compoundTag,
              DATA_OFFERS_TAG,
              provider,
              this + " (" + this.getEntity().getUUID() + ")");
      if (offers != null) {
        log.debug("Loading trading offers {} for {}", offers, this);
        this.setTradingOffers(offers);
      }
    }
  }
}
