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

import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.handler.TradingOfferHandler;
import de.markusbordihn.easynpc.utils.TradingUtils;
import java.util.List;
import net.minecraft.core.component.DataComponents;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.chat.Component;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.ItemLore;
import net.minecraft.world.item.trading.ItemCost;
import net.minecraft.world.item.trading.MerchantOffer;
import net.minecraft.world.item.trading.MerchantOffers;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.phys.Vec3;

public class TradingPersistenceTestHelper {

  private TradingPersistenceTestHelper() {}

  public static void assertTradeUsePersistence(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));

    TradingDataCapable<?> tradingData = npc.getEasyNPCTradingData();
    if (tradingData == null) {
      helper.fail("NPC has no TradingDataCapable");
      return;
    }

    // Configure a one-off trade (maxUses=1) to match the reported bug scenario.
    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(new ItemCost(Items.DIAMOND), new ItemStack(Items.EMERALD), 1, 0, 1.0f));
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    tradingData.setTradingOffers(offers);

    // Simulate a player completing the trade (uses the UI copy, same as in real gameplay).
    MerchantOffers currentOffers = tradingData.getMerchantTradingOffers();
    if (currentOffers == null || currentOffers.isEmpty()) {
      helper.fail("No offers available after setup");
      return;
    }
    tradingData.notifyTrade(currentOffers.get(0));

    // Synced data must reflect the updated uses count before any NBT save.
    MerchantOffers syncedOffers = tradingData.getTradingOffers();
    if (syncedOffers == null || syncedOffers.isEmpty()) {
      helper.fail("Synced offers null after notifyTrade");
      return;
    }
    if (syncedOffers.get(0).getUses() != 1) {
      helper.fail(
          "Expected uses=1 in synced data after trade, got " + syncedOffers.get(0).getUses());
      return;
    }

    // Simulate chunk unload / world rejoin via NBT round-trip.
    TagValueOutput valueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
    tradingData.addAdditionalTradingData(valueOutput);
    ValueInput valueInput =
        TagValueInput.create(
            ProblemReporter.DISCARDING,
            helper.getLevel().registryAccess(),
            valueOutput.buildResult());
    tradingData.readAdditionalTradingData(valueInput);

    // Uses must survive the round-trip.
    MerchantOffers reloadedOffers = tradingData.getTradingOffers();
    if (reloadedOffers == null || reloadedOffers.isEmpty()) {
      helper.fail("Offers null after NBT round-trip");
      return;
    }
    if (reloadedOffers.get(0).getUses() != 1) {
      helper.fail(
          "Trade uses reset after NBT round-trip: expected 1, got "
              + reloadedOffers.get(0).getUses());
    }
  }

  public static void assertAdvancedMaxUsesPreservesCurrentUses(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));

    TradingDataCapable<?> tradingData = npc.getEasyNPCTradingData();
    if (tradingData == null) {
      helper.fail("NPC has no TradingDataCapable");
      return;
    }

    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(new ItemCost(Items.DIAMOND), new ItemStack(Items.EMERALD), 10, 5, 1.0f));
    tradingData.getTradingDataSet().setType(TradingType.ADVANCED);
    tradingData.setTradingOffers(offers);

    tradingData.notifyTrade(tradingData.getMerchantTradingOffers().get(0));

    if (tradingData.getTradingOffers().get(0).getUses() != 1) {
      helper.fail("Setup: expected uses=1 after trade");
      return;
    }

    TradingOfferHandler.setAdvancedTradingMaxUses(tradingData, 0, 5);

    MerchantOffer updated = tradingData.getTradingOffers().get(0);
    if (updated.getUses() != 1) {
      helper.fail(
          "Bug 1 regression: setAdvancedTradingMaxUses reset uses to "
              + updated.getUses()
              + ", expected 1");
      return;
    }
    if (updated.getMaxUses() != 5) {
      helper.fail(
          "setAdvancedTradingMaxUses did not update maxUses: expected 5, got "
              + updated.getMaxUses());
    }
  }

  public static void assertModifiedTradeItemPersistence(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    TradingDataCapable<?> tradingData = npc.getEasyNPCTradingData();
    if (tradingData == null) {
      helper.fail("NPC has no TradingDataCapable");
      return;
    }

    ItemStack questItem = new ItemStack(Items.SUGAR);
    questItem.set(DataComponents.CUSTOM_NAME, Component.literal("Lethal Bio-Reagent"));
    questItem.set(
        DataComponents.LORE, new ItemLore(List.of(Component.literal("It singes at your skin..."))));
    questItem.remove(DataComponents.ATTRIBUTE_MODIFIERS);

    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(
            TradingUtils.getItemCost(questItem), new ItemStack(Items.EMERALD), 10, 0, 1.0F));
    tradingData.getTradingDataSet().setType(TradingType.ADVANCED);
    tradingData.setTradingOffers(offers);
    ItemStack originalCost = tradingData.getTradingOffers().get(0).getBaseCostA().copy();

    TagValueOutput valueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
    tradingData.addAdditionalTradingData(valueOutput);
    ValueInput valueInput =
        TagValueInput.create(
            ProblemReporter.DISCARDING,
            helper.getLevel().registryAccess(),
            valueOutput.buildResult());
    tradingData.readAdditionalTradingData(valueInput);
    TradingOfferHandler.setAdvancedTradingMaxUses(tradingData, 0, 5);

    MerchantOffer reloadedOffer = tradingData.getTradingOffers().get(0);
    if (!reloadedOffer.satisfiedBy(questItem, ItemStack.EMPTY)) {
      helper.fail("Modified quest item no longer matches after NBT round-trip and offer rebuild");
      return;
    }
    if (reloadedOffer.satisfiedBy(new ItemStack(Items.SUGAR), ItemStack.EMPTY)) {
      helper.fail("Plain sugar unexpectedly matches the modified quest item trade");
      return;
    }
    if (!originalCost
        .getComponentsPatch()
        .equals(reloadedOffer.getBaseCostA().getComponentsPatch())) {
      helper.fail("Trade item components changed after NBT round-trip and offer rebuild");
    }
  }

  public static void assertBasicTradingOffersUpdateAppliesCorrectFields(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));

    TradingDataCapable<?> tradingData = npc.getEasyNPCTradingData();
    if (tradingData == null) {
      helper.fail("NPC has no TradingDataCapable");
      return;
    }

    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(new ItemCost(Items.DIAMOND), new ItemStack(Items.EMERALD), 10, 0, 1.0f));
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    tradingData.getTradingDataSet().setMaxUses(10);
    tradingData.getTradingDataSet().setRewardedXP(0);
    tradingData.setTradingOffers(offers);

    tradingData.notifyTrade(tradingData.getMerchantTradingOffers().get(0));

    tradingData.getTradingDataSet().setMaxUses(3);
    tradingData.getTradingDataSet().setRewardedXP(7);
    TradingOfferHandler.updateBasicTradingOffers(tradingData);

    MerchantOffer updated = tradingData.getTradingOffers().get(0);

    if (updated.getMaxUses() != 3) {
      helper.fail("Bug 2 regression: maxUses expected 3, got " + updated.getMaxUses());
      return;
    }
    if (updated.getXp() != 7) {
      helper.fail("Bug 2 regression: xp expected 7, got " + updated.getXp());
      return;
    }
    if (updated.getUses() != 1) {
      helper.fail(
          "Bug 2 regression: uses must be preserved (expected 1), got " + updated.getUses());
    }
  }

  public static void assertTimedTradingResetRestoresUses(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));

    TradingDataCapable<?> tradingData = npc.getEasyNPCTradingData();
    if (tradingData == null) {
      helper.fail("NPC has no TradingDataCapable");
      return;
    }

    MerchantOffers offers = new MerchantOffers();
    offers.add(
        new MerchantOffer(new ItemCost(Items.DIAMOND), new ItemStack(Items.EMERALD), 1, 0, 1.0f));
    tradingData.getTradingDataSet().setType(TradingType.BASIC);
    tradingData.getTradingDataSet().setResetsEveryMin(1);
    tradingData.getTradingDataSet().setLastReset(System.currentTimeMillis());
    tradingData.setTradingOffers(offers);

    tradingData.notifyTrade(tradingData.getMerchantTradingOffers().get(0));

    if (tradingData.getTradingOffers().get(0).getUses() != 1) {
      helper.fail("Setup: expected exhausted trade with uses=1");
      return;
    }

    tradingData.getTradingDataSet().setLastReset(System.currentTimeMillis() - (2L * 60L * 1000L));

    if (!tradingData.resetExpiredTradingOffers()) {
      helper.fail("Expected expired trading offers to reset");
      return;
    }

    MerchantOffer resetOffer = tradingData.getTradingOffers().get(0);
    if (resetOffer.getUses() != 0) {
      helper.fail("Timed reset expected uses=0, got " + resetOffer.getUses());
    }
  }
}
