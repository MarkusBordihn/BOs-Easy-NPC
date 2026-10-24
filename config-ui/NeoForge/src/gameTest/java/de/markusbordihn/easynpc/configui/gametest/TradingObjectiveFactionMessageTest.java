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

import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

public final class TradingObjectiveFactionMessageTest {

  private TradingObjectiveFactionMessageTest() {}

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  public static void testTradingTypeChange(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertTradingTypeChange(helper, humanoid());
    helper.succeed();
  }

  public static void testBasicTradingMaxUsesChange(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertBasicTradingMaxUsesChange(helper, humanoid());
    helper.succeed();
  }

  public static void testBasicTradingResetIntervalChange(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertBasicTradingResetIntervalChange(
        helper, humanoid());
    helper.succeed();
  }

  public static void testAdvancedTradingPriceMultiplierChange(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertAdvancedTradingPriceMultiplierChange(
        helper, humanoid());
    helper.succeed();
  }

  public static void testObjectiveAddition(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertObjectiveAddition(helper, humanoid());
    helper.succeed();
  }

  public static void testObjectiveRemoval(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertObjectiveRemoval(helper, humanoid());
    helper.succeed();
  }

  public static void testFactionAssignment(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertFactionAssignment(helper, humanoid());
    helper.succeed();
  }

  public static void testFactionUnassignment(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertFactionUnassignment(helper, humanoid());
    helper.succeed();
  }

  public static void testFactionCreation(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertFactionCreation(helper, humanoid());
    helper.succeed();
  }

  public static void testFactionColorChange(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertFactionColorChange(helper, humanoid());
    helper.succeed();
  }

  public static void testFactionRelationChange(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertFactionRelationChange(helper, humanoid());
    helper.succeed();
  }

  public static void testFactionEntryRemoval(GameTestHelper helper) {
    TradingObjectiveFactionMessageTestHelper.assertFactionEntryRemoval(helper, humanoid());
    helper.succeed();
  }
}
