/*
 * Copyright 2025 Markus Bordihn
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

import de.markusbordihn.easynpc.condition.HasItemInHandCondition;
import de.markusbordihn.easynpc.condition.HasItemInInventoryCondition;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.HandItemType;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.GameType;

public class ConditionEvaluationTestHelper {

  private ConditionEvaluationTestHelper() {}

  public static void assertItemQuantityConditions(GameTestHelper helper) {
    Player player = helper.makeMockPlayer(GameType.SURVIVAL);
    player.getInventory().clearContent();

    ConditionDataEntry needTenDiamonds =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_INVENTORY,
            ConditionOperationType.EQUALS,
            "minecraft:diamond",
            10);

    player.getInventory().add(new ItemStack(Items.DIAMOND, 9));
    GameTestHelpers.assertTrue(
        helper,
        "9 diamonds should not satisfy a >= 10 condition",
        !HasItemInInventoryCondition.evaluate(needTenDiamonds, player));

    player.getInventory().add(new ItemStack(Items.DIAMOND, 1));
    GameTestHelpers.assertTrue(
        helper,
        "10 diamonds should satisfy a >= 10 condition",
        HasItemInInventoryCondition.evaluate(needTenDiamonds, player));

    ConditionDataEntry needOneEmerald =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY).withName("minecraft:emerald");
    GameTestHelpers.assertTrue(
        helper,
        "no emerald should fail a default (>= 1) condition",
        !HasItemInInventoryCondition.evaluate(needOneEmerald, player));
    player.getInventory().add(new ItemStack(Items.EMERALD, 1));
    GameTestHelpers.assertTrue(
        helper,
        "one emerald should satisfy a default (>= 1) condition",
        HasItemInInventoryCondition.evaluate(needOneEmerald, player));

    ConditionDataEntry notTenGold =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_INVENTORY,
            ConditionOperationType.NOT_EQUALS,
            "minecraft:gold_ingot",
            10);
    GameTestHelpers.assertTrue(
        helper,
        "no gold should satisfy a NOT (>= 10) condition",
        HasItemInInventoryCondition.evaluate(notTenGold, player));
    player.getInventory().add(new ItemStack(Items.GOLD_INGOT, 10));
    GameTestHelpers.assertTrue(
        helper,
        "10 gold should fail a NOT (>= 10) condition",
        !HasItemInInventoryCondition.evaluate(notTenGold, player));

    ConditionDataEntry needFiveSticks =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.MAIN_HAND,
            ConditionOperationType.EQUALS,
            "minecraft:stick",
            5);
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 4));
    GameTestHelpers.assertTrue(
        helper,
        "4 sticks in hand should not satisfy a >= 5 condition",
        !HasItemInHandCondition.evaluate(needFiveSticks, player));
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 5));
    GameTestHelpers.assertTrue(
        helper,
        "5 sticks in hand should satisfy a >= 5 condition",
        HasItemInHandCondition.evaluate(needFiveSticks, player));
  }
}
