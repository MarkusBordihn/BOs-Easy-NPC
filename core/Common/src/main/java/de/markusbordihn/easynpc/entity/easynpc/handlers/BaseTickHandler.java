package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.handler.PauseManager;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Mob;

public interface BaseTickHandler<E extends Mob> extends EasyNPC<E> {

  int BASE_TICK = 16;
  int TRADING_BASE_TICK = Math.round((20f / BASE_TICK) * 60) - 10;
  int ATTRIBUTE_BASE_TICK = Math.round((20f / BASE_TICK) * 20);

  default void handleBaseTick() {
    this.getProfiler().push("npcBaseTick");

    if (PauseManager.isPaused(this)) {
      PauseManager.enforcePause(this);
      this.getProfiler().pop();
      return;
    }

    PendingActionHandler<E> pendingActionHandler = this.getEasyNPCPendingActionHandler();
    if (pendingActionHandler != null) {
      pendingActionHandler.tickPendingActions();
    }

    TickerDataCapable<E> tickerData = this.getEasyNPCTickerData();
    if (tickerData.checkAndIncreaseTicker(TickerType.BASE_TICK, BASE_TICK)) {
      Entity entity = this.getEntity();

      if (tickerData.checkAndIncreaseTicker(TickerType.ATTRIBUTE_BASE_TICK, ATTRIBUTE_BASE_TICK)) {
        AttributeHandler<E> attributeHandler = this.getEasyNPCAttributeHandler();
        attributeHandler.checkAttributeActions();

        // The navigation type can be derived from a data pack, which may load after the entity.
        NavigationDataCapable<E> navigationData = this.getEasyNPCNavigationData();
        if (navigationData != null && !entity.level().isClientSide()) {
          navigationData.refreshNavigationIfChanged();
        }
        tickerData.resetTicker(TickerType.ATTRIBUTE_BASE_TICK);
      }

      ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
      actionHandler.checkDistanceActions();

      actionHandler.checkSpawnAction();
      actionHandler.checkIntervalActions();
      actionHandler.checkEnvironmentActions();

      if (tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, TRADING_BASE_TICK)) {
        TradingDataCapable<E> tradingData = this.getEasyNPCTradingData();
        if (tradingData != null && tradingData.hasTradingData()) {
          tradingData.resetExpiredTradingOffers();
          actionHandler.checkTradingActions();
        }
        tickerData.resetTicker(TickerType.TRADING_BASE_TICK);
      }

      if (entity.isInWater()
          && this.getEasyNPCAttributeData()
              .getEntityAttributes()
              .getEnvironmentalAttributes()
              .canBreatheUnderwater()) {
        entity.setAirSupply(300);
      }

      tickerData.resetTicker(TickerType.BASE_TICK);
    }

    this.getProfiler().pop();
  }
}
