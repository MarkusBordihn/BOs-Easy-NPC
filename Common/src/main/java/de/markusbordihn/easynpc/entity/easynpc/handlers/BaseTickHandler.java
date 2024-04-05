package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import net.minecraft.world.entity.PathfinderMob;

public interface BaseTickHandler<E extends PathfinderMob> extends EasyNPC<E> {

  int BASE_TICK = 16;
  int TRADING_BASE_TICK = Math.round((20f / BASE_TICK) * 60) - 10;

  default void handleBaseTick() {
    this.getProfiler().push("npcBaseTick");

    TickerData<E> tickerData = this.getEasyNPCTickerData();
    if (tickerData.checkAndIncreaseTicker(TickerType.BASE_TICK, BASE_TICK)) {
      ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();

      // Check distance for additional actions.
      actionHandler.checkDistanceActions();

      // Check if we have a trading inventory and update it.
      if (tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, TRADING_BASE_TICK)) {
        TradingData<E> tradingData = this.getEasyNPCTradingData();
        if (tradingData.hasTrading()) {
          actionHandler.checkTradingActions();
        }
        tickerData.resetTicker(TickerType.TRADING_BASE_TICK);
      }

      tickerData.resetTicker(TickerType.BASE_TICK);
    }

    this.getProfiler().pop();
  }
}
