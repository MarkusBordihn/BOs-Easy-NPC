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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.phys.Vec3;

public class TradingActionTestHelper {

  private static final ResourceLocation TRADE_EVENT_STATE =
      StateIdentifier.parse("gametest_on_trade");
  private static final ResourceLocation OFFER_ACTION_STATE =
      StateIdentifier.parse("gametest_offer_action");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 2);

  private TradingActionTestHelper() {}

  public static void assertTradeActionsTrigger(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
    GameTestHelpers.assertNotNull(helper, "NPC must support trading", tradingData);

    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(
        ActionEventType.ON_TRADE, stateAction("increase gametest_on_trade 1"));
    easyNPC.getEasyNPCActionEventData().setActionEventSet(actionEventSet);

    MerchantOffers merchantOffers = new MerchantOffers();
    merchantOffers.add(
        new MerchantOffer(new ItemCost(Items.DIAMOND), new ItemStack(Items.EMERALD), 10, 0, 1.0f));
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    tradingData
        .getTradingDataSet()
        .setOfferAction(0, stateAction("increase gametest_offer_action 1"));
    tradingData.setTradingOffers(merchantOffers);

    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    tradingData.setTradingPlayer(serverPlayer);

    tradingData.notifyTrade(tradingData.getOffers().get(0));

    GameTestHelpers.assertEquals(
        helper,
        "The \"On Trade\" action must run once after a completed trade",
        1,
        easyNPC.getEasyNPCStateData().getStateNumber(TRADE_EVENT_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "The action of the traded offer must run once after a completed trade",
        1,
        easyNPC.getEasyNPCStateData().getStateNumber(OFFER_ACTION_STATE));
  }

  private static ActionDataSet stateAction(String command) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.NPC_STATE, command));
    return actionDataSet;
  }
}
