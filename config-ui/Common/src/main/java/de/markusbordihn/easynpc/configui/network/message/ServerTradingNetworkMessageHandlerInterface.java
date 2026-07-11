/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.network.message;

import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeAdvancedTradingMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeBasicTradingMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeProfessionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeTradingTypeMessage;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.data.trading.TradingValueType;
import java.util.UUID;

public interface ServerTradingNetworkMessageHandlerInterface {

  default void changeProfession(UUID uuid, Profession profession) {
    if (uuid != null && profession != null) {
      NetworkHandlerManager.sendMessageToServer(new ChangeProfessionMessage(uuid, profession));
    }
  }

  default void changeTradingType(UUID uuid, TradingType tradingType) {
    if (uuid != null && tradingType != null) {
      NetworkHandlerManager.sendMessageToServer(new ChangeTradingTypeMessage(uuid, tradingType));
    }
  }

  default void setAdvancedTradingResetsEveryMin(UUID uuid, int resetsEveryMin) {
    if (uuid != null && resetsEveryMin >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeAdvancedTradingMessage(
              uuid, 9999, TradingValueType.RESET_TRADING_EVERY_MIN, resetsEveryMin));
    }
  }

  default void setAdvancedTradingMaxUses(UUID uuid, int tradingOfferIndex, int maxUses) {
    if (uuid != null && maxUses > 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeAdvancedTradingMessage(
              uuid, tradingOfferIndex, TradingValueType.MAX_USES, maxUses));
    }
  }

  default void setAdvancedTradingRewardExp(UUID uuid, int tradingOfferIndex, int xp) {
    if (uuid != null && xp >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeAdvancedTradingMessage(
              uuid, tradingOfferIndex, TradingValueType.REWARD_EXP, xp));
    }
  }

  default void setAdvancedTradingPriceMultiplier(
      UUID uuid, int tradingOfferIndex, float priceMultiplier) {
    if (uuid != null && priceMultiplier >= 0.0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeAdvancedTradingMessage(
              uuid, tradingOfferIndex, TradingValueType.PRICE_MULTIPLIER, priceMultiplier));
    }
  }

  default void setAdvancedTradingDemand(UUID uuid, int tradingOfferIndex, int demand) {
    if (uuid != null && demand >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeAdvancedTradingMessage(
              uuid, tradingOfferIndex, TradingValueType.DEMAND, demand));
    }
  }

  default void setBasicTradingMaxUses(UUID uuid, int maxUses) {
    if (uuid != null && maxUses > 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeBasicTradingMessage(uuid, TradingValueType.MAX_USES, maxUses));
    }
  }

  default void setBasicTradingRewardExp(UUID uuid, int rewardExp) {
    if (uuid != null && rewardExp >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeBasicTradingMessage(uuid, TradingValueType.REWARD_EXP, rewardExp));
    }
  }

  default void setBasicTradingResetsEveryMin(UUID uuid, int resetsEveryMin) {
    if (uuid != null && resetsEveryMin >= 0) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeBasicTradingMessage(
              uuid, TradingValueType.RESET_TRADING_EVERY_MIN, resetsEveryMin));
    }
  }

  default void resetTradingOffers(UUID uuid) {
    if (uuid != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeBasicTradingMessage(uuid, TradingValueType.LAST_TRADING_RESET, 0));
    }
  }
}
