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
    helper.assertTrue(
        !HasItemInInventoryCondition.evaluate(needTenDiamonds, player),
        "9 diamonds should not satisfy a >= 10 condition");

    player.getInventory().add(new ItemStack(Items.DIAMOND, 1));
    helper.assertTrue(
        HasItemInInventoryCondition.evaluate(needTenDiamonds, player),
        "10 diamonds should satisfy a >= 10 condition");

    ConditionDataEntry needOneEmerald =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY).withName("minecraft:emerald");
    helper.assertTrue(
        !HasItemInInventoryCondition.evaluate(needOneEmerald, player),
        "no emerald should fail a default (>= 1) condition");
    player.getInventory().add(new ItemStack(Items.EMERALD, 1));
    helper.assertTrue(
        HasItemInInventoryCondition.evaluate(needOneEmerald, player),
        "one emerald should satisfy a default (>= 1) condition");

    ConditionDataEntry notTenGold =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_INVENTORY,
            ConditionOperationType.NOT_EQUALS,
            "minecraft:gold_ingot",
            10);
    helper.assertTrue(
        HasItemInInventoryCondition.evaluate(notTenGold, player),
        "no gold should satisfy a NOT (>= 10) condition");
    player.getInventory().add(new ItemStack(Items.GOLD_INGOT, 10));
    helper.assertTrue(
        !HasItemInInventoryCondition.evaluate(notTenGold, player),
        "10 gold should fail a NOT (>= 10) condition");

    ConditionDataEntry needFiveSticks =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.MAIN_HAND,
            ConditionOperationType.EQUALS,
            "minecraft:stick",
            5);
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 4));
    helper.assertTrue(
        !HasItemInHandCondition.evaluate(needFiveSticks, player),
        "4 sticks in hand should not satisfy a >= 5 condition");
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 5));
    helper.assertTrue(
        HasItemInHandCondition.evaluate(needFiveSticks, player),
        "5 sticks in hand should satisfy a >= 5 condition");
  }
}
