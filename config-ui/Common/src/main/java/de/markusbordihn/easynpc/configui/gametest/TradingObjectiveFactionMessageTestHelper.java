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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.gametest.ServerMessageAssertions.SurvivalOwnerAccess;
import de.markusbordihn.easynpc.configui.network.message.server.AddOrUpdateObjectiveMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeAdvancedTradingMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeBasicTradingMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeFactionColorMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeFactionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeFactionRelationMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeTradingTypeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.CreateFactionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveFactionEntryMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveObjectiveMessage;
import de.markusbordihn.easynpc.data.faction.FactionDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.data.trading.TradingDataSet;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.data.trading.TradingValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.handler.FactionHandler;
import de.markusbordihn.easynpc.handler.ObjectiveHandler;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Scoreboard;
import net.minecraft.world.scores.TeamColor;

public final class TradingObjectiveFactionMessageTestHelper {

  private static final int INITIAL_MAX_USES = 64;
  private static final float INITIAL_PRICE_MULTIPLIER = 0.05f;
  private static final int CHANGED_MAX_USES = 16;
  private static final int CHANGED_RESET_INTERVAL_MINUTES = 30;
  private static final float CHANGED_PRICE_MULTIPLIER = 0.2f;
  private static final ObjectiveType TEST_OBJECTIVE_TYPE = ObjectiveType.PANIC;
  private static final String TEST_FACTION_PREFIX = "config_ui_message_test_";
  private static final TeamColor CHANGED_FACTION_COLOR = TeamColor.GOLD;

  private TradingObjectiveFactionMessageTestHelper() {}

  public static void assertTradingTypeChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeTradingTypeMessage(uuid, TradingType.BASIC),
        ChangeTradingTypeMessage::create,
        easyNPC -> tradingDataSet(easyNPC).isType(TradingType.BASIC),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertBasicTradingMaxUsesChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC -> setupTrading(easyNPC, TradingType.BASIC),
        uuid -> new ChangeBasicTradingMessage(uuid, TradingValueType.MAX_USES, CHANGED_MAX_USES),
        ChangeBasicTradingMessage::create,
        easyNPC ->
            tradingDataSet(easyNPC).getMaxUses() == CHANGED_MAX_USES
                && firstTradingOffer(easyNPC).getMaxUses() == CHANGED_MAX_USES,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertBasicTradingResetIntervalChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC -> setupTrading(easyNPC, TradingType.BASIC),
        uuid ->
            new ChangeBasicTradingMessage(
                uuid, TradingValueType.RESET_TRADING_EVERY_MIN, CHANGED_RESET_INTERVAL_MINUTES),
        ChangeBasicTradingMessage::create,
        easyNPC -> tradingDataSet(easyNPC).getResetsEveryMin() == CHANGED_RESET_INTERVAL_MINUTES,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertAdvancedTradingPriceMultiplierChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC -> setupTrading(easyNPC, TradingType.ADVANCED),
        uuid ->
            new ChangeAdvancedTradingMessage(
                uuid, 0, TradingValueType.PRICE_MULTIPLIER, CHANGED_PRICE_MULTIPLIER),
        ChangeAdvancedTradingMessage::create,
        easyNPC -> firstTradingOffer(easyNPC).getPriceMultiplier() == CHANGED_PRICE_MULTIPLIER,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertObjectiveAddition(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new AddOrUpdateObjectiveMessage(uuid, new ObjectiveDataEntry(TEST_OBJECTIVE_TYPE)),
        AddOrUpdateObjectiveMessage::create,
        TradingObjectiveFactionMessageTestHelper::hasTestObjective,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertObjectiveRemoval(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC ->
            ObjectiveHandler.addOrUpdateCustomObjective(
                easyNPC, new ObjectiveDataEntry(TEST_OBJECTIVE_TYPE)),
        uuid -> new RemoveObjectiveMessage(uuid, new ObjectiveDataEntry(TEST_OBJECTIVE_TYPE)),
        RemoveObjectiveMessage::create,
        easyNPC -> !hasTestObjective(easyNPC),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertFactionAssignment(GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        TradingObjectiveFactionMessageTestHelper::createFaction,
        uuid -> new ChangeFactionMessage(uuid, factionName(uuid)),
        ChangeFactionMessage::create,
        easyNPC ->
            factionName(easyNPC.getEntityUUID())
                .equals(easyNPC.getEasyNPCFactionData().getFactionName()),
        SurvivalOwnerAccess.DENIED);
    removeTestFactions(helper);
  }

  public static void assertFactionUnassignment(GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC -> FactionHandler.setFaction(easyNPC, factionName(easyNPC.getEntityUUID())),
        uuid -> new ChangeFactionMessage(uuid, ""),
        ChangeFactionMessage::create,
        easyNPC -> !easyNPC.getEasyNPCFactionData().hasFactionName(),
        SurvivalOwnerAccess.DENIED);
    removeTestFactions(helper);
  }

  public static void assertFactionCreation(GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new CreateFactionMessage(uuid, factionName(uuid)),
        CreateFactionMessage::create,
        easyNPC -> FactionData.get().hasFaction(factionName(easyNPC.getEntityUUID())),
        SurvivalOwnerAccess.DENIED);
    removeTestFactions(helper);
  }

  public static void assertFactionColorChange(GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        TradingObjectiveFactionMessageTestHelper::createFaction,
        uuid ->
            new ChangeFactionColorMessage(uuid, factionName(uuid), CHANGED_FACTION_COLOR.getSerializedName()),
        ChangeFactionColorMessage::create,
        TradingObjectiveFactionMessageTestHelper::hasChangedFactionColor,
        SurvivalOwnerAccess.DENIED);
    removeTestFactions(helper);
  }

  public static void assertFactionRelationChange(GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        TradingObjectiveFactionMessageTestHelper::createFactionPair,
        uuid ->
            new ChangeFactionRelationMessage(
                uuid, factionName(uuid), hostileFactionName(uuid), true, true),
        ChangeFactionRelationMessage::create,
        TradingObjectiveFactionMessageTestHelper::isMutuallyHostile,
        SurvivalOwnerAccess.DENIED);
    removeTestFactions(helper);
  }

  public static void assertFactionEntryRemoval(GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        TradingObjectiveFactionMessageTestHelper::createFaction,
        uuid -> new RemoveFactionEntryMessage(uuid, factionName(uuid)),
        RemoveFactionEntryMessage::create,
        easyNPC -> !FactionData.get().hasFaction(factionName(easyNPC.getEntityUUID())),
        SurvivalOwnerAccess.DENIED);
    removeTestFactions(helper);
  }

  private static void setupTrading(EasyNPC<?> easyNPC, TradingType tradingType) {
    MerchantOffers merchantOffers = new MerchantOffers();
    merchantOffers.add(
        new MerchantOffer(
            new ItemCost(Items.EMERALD),
            new ItemStack(Items.DIAMOND),
            INITIAL_MAX_USES,
            0,
            INITIAL_PRICE_MULTIPLIER));
    TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
    tradingData.getTradingDataSet().setType(tradingType);
    tradingData.setTradingOffers(merchantOffers);
  }

  private static TradingDataSet tradingDataSet(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCTradingData().getTradingDataSet();
  }

  private static MerchantOffer firstTradingOffer(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCTradingData().getTradingOffers().get(0);
  }

  private static boolean hasTestObjective(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCObjectiveData().hasObjective(TEST_OBJECTIVE_TYPE);
  }

  private static String factionName(UUID uuid) {
    return TEST_FACTION_PREFIX + uuid;
  }

  private static String hostileFactionName(UUID uuid) {
    return TEST_FACTION_PREFIX + "hostile_" + uuid;
  }

  private static void createFaction(EasyNPC<?> easyNPC) {
    FactionData.get().createFaction(factionName(easyNPC.getEntityUUID()));
  }

  private static void createFactionPair(EasyNPC<?> easyNPC) {
    UUID uuid = easyNPC.getEntityUUID();
    FactionData.get().createFaction(factionName(uuid));
    FactionData.get().createFaction(hostileFactionName(uuid));
  }

  private static boolean hasChangedFactionColor(EasyNPC<?> easyNPC) {
    FactionDataEntry faction = FactionData.get().getFaction(factionName(easyNPC.getEntityUUID()));
    return faction != null && faction.getColor() == CHANGED_FACTION_COLOR;
  }

  private static boolean isMutuallyHostile(EasyNPC<?> easyNPC) {
    UUID uuid = easyNPC.getEntityUUID();
    return FactionData.get().isHostile(factionName(uuid), hostileFactionName(uuid))
        && FactionData.get().isHostile(hostileFactionName(uuid), factionName(uuid));
  }

  private static void removeTestFactions(GameTestHelper helper) {
    Scoreboard scoreboard = helper.getLevel().getScoreboard();
    for (String factionName : FactionData.get().getFactionNames()) {
      if (!factionName.startsWith(TEST_FACTION_PREFIX)) {
        continue;
      }
      FactionData.get().removeFaction(factionName);
      PlayerTeam team = scoreboard.getPlayerTeam(factionName);
      if (team != null) {
        scoreboard.removePlayerTeam(team);
      }
    }
  }
}
