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

package de.markusbordihn.easynpc.data.trading;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.MerchantMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SafeMerchantData<E extends PathfinderMob> implements Merchant {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final boolean DEFAULT_CAN_RESTOCK = false;
  private static final boolean DEFAULT_SHOW_PROGRESS_BAR = true;
  private static final int DEFAULT_VILLAGER_LEVEL = 0;
  private static final int DEFAULT_VILLAGER_XP = 0;

  private final TradingDataCapable<E> tradingData;

  public SafeMerchantData(TradingDataCapable<E> tradingData) {
    this.tradingData = tradingData;
  }

  @Override
  public Player getTradingPlayer() {
    return tradingData.getTradingPlayer();
  }

  @Override
  public void setTradingPlayer(Player player) {
    tradingData.setTradingPlayer(player);
  }

  @Override
  public MerchantOffers getOffers() {
    return tradingData.getOffers();
  }

  @Override
  public void overrideOffers(MerchantOffers offers) {
    tradingData.overrideOffers(offers);
  }

  @Override
  public void notifyTrade(MerchantOffer offer) {
    tradingData.notifyTrade(offer);
  }

  @Override
  public void notifyTradeUpdated(ItemStack stack) {
    tradingData.notifyTradeUpdated(stack);
  }

  @Override
  public int getVillagerXp() {
    try {
      return tradingData.getVillagerXp();
    } catch (AbstractMethodError e) {
      return DEFAULT_VILLAGER_XP;
    }
  }

  @Override
  public void overrideXp(int xp) {
    try {
      tradingData.overrideXp(xp);
    } catch (AbstractMethodError e) {
      // Silently ignore if method is not implemented
    }
  }

  @Override
  public boolean showProgressBar() {
    try {
      return tradingData.showProgressBar();
    } catch (AbstractMethodError e) {
      return DEFAULT_SHOW_PROGRESS_BAR;
    }
  }

  @Override
  public SoundEvent getNotifyTradeSound() {
    try {
      return tradingData.getNotifyTradeSound();
    } catch (AbstractMethodError e) {
      return SoundEvents.VILLAGER_TRADE;
    }
  }

  @Override
  public boolean isClientSide() {
    return tradingData.isClientSideInstance();
  }

  @Override
  public void openTradingScreen(Player player, Component name, int containerId) {
    if (player instanceof ServerPlayer serverPlayer) {
      MerchantOffers offers = this.getOffers();
      if (offers == null || offers.isEmpty()) {
        log.error(
            "No trading offers available for {} with name {}",
            serverPlayer.getName().getString(),
            name.getString());
        return;
      }

      log.debug(
          "Opening trading screen for {} with {} offers",
          serverPlayer.getName().getString(),
          offers.size());
      serverPlayer.openMenu(
          new SimpleMenuProvider(
              (id, inventory, p) -> new MerchantMenu(id, inventory, this), name));

      serverPlayer.sendMerchantOffers(
          serverPlayer.containerMenu.containerId,
          offers,
          this.getVillagerXp(),
          DEFAULT_VILLAGER_LEVEL,
          this.showProgressBar(),
          DEFAULT_CAN_RESTOCK);
    }
  }
}
