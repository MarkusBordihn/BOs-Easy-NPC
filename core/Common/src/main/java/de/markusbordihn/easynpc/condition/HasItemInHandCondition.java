/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.condition;

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.HandItemType;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

public class HasItemInHandCondition {

  private HasItemInHandCondition() {}

  public static boolean evaluate(ConditionDataEntry conditionDataEntry, Player player) {
    if (!conditionDataEntry.hasName() || player == null) {
      return false;
    }

    Item targetItem = ItemStackConditionMatcher.resolveItem(conditionDataEntry.name());
    CompoundTag requiredData =
        ItemStackConditionMatcher.parseRequiredData(conditionDataEntry.customData());
    if (requiredData == null && conditionDataEntry.hasCustomData()) {
      return false;
    }
    if (!ItemStackConditionMatcher.isCustomDataComponentValid(
        conditionDataEntry.customDataComponent())) {
      return false;
    }

    HandItemType handItemType = HandItemType.BOTH;
    if (conditionDataEntry.subType() instanceof HandItemType handItemTypeEntry) {
      handItemType = handItemTypeEntry;
    }

    int required = Math.max(1, conditionDataEntry.value());
    String customDataComponent = conditionDataEntry.customDataComponent();
    int count =
        switch (handItemType) {
          case MAIN_HAND ->
              countInStack(player.getMainHandItem(), targetItem, requiredData, customDataComponent);
          case OFF_HAND ->
              countInStack(player.getOffhandItem(), targetItem, requiredData, customDataComponent);
          default ->
              countInStack(player.getMainHandItem(), targetItem, requiredData, customDataComponent)
                  + countInStack(
                      player.getOffhandItem(), targetItem, requiredData, customDataComponent);
        };

    boolean hasEnough = count >= required;
    return (conditionDataEntry.operationType() == ConditionOperationType.NOT_EQUALS) != hasEnough;
  }

  private static int countInStack(
      ItemStack stack, Item targetItem, CompoundTag requiredData, String customDataComponent) {
    return ItemStackConditionMatcher.matches(stack, targetItem, requiredData, customDataComponent)
        ? stack.getCount()
        : 0;
  }
}
