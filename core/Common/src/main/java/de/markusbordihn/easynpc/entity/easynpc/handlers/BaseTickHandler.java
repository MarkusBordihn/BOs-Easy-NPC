package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface BaseTickHandler<E extends PathfinderMob> extends EasyNPC<E> {

  int BASE_TICK = 16;
  int TRADING_BASE_TICK = Math.round((20f / BASE_TICK) * 60) - 10;
  int ATTRIBUTE_BASE_TICK = Math.round((20f / BASE_TICK) * 20);

  default void handleBaseTick() {
    this.getProfiler().push("npcBaseTick");

    TickerData<E> tickerData = this.getEasyNPCTickerData();
    if (tickerData.checkAndIncreaseTicker(TickerType.BASE_TICK, BASE_TICK)) {
      Entity entity = this.getEntity();

      // Check for attribute relevant actions.
      if (tickerData.checkAndIncreaseTicker(TickerType.ATTRIBUTE_BASE_TICK, ATTRIBUTE_BASE_TICK)) {
        AttributeHandler<E> attributeHandler = this.getEasyNPCAttributeHandler();
        attributeHandler.checkAttributeActions();
        tickerData.resetTicker(TickerType.ATTRIBUTE_BASE_TICK);
      }

      // Check distance for additional actions.
      ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
      actionHandler.checkDistanceActions();

      // Check if we have a trading inventory and update it.
      if (tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, TRADING_BASE_TICK)) {
        TradingData<E> tradingData = this.getEasyNPCTradingData();
        if (tradingData.hasTradingData()) {
          actionHandler.checkTradingActions();
        }
        tickerData.resetTicker(TickerType.TRADING_BASE_TICK);
      }

      // Check if entity can breathe underwater.
      if (entity.isInLiquid()
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
