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

package de.markusbordihn.easynpc.condition;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.Identifier;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;

public class ItemStackConditionMatcher {

  private static final Identifier EASY_NPC_CUSTOM_DATA_COMPONENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, DataComponents.CUSTOM_DATA_ID);
  private static final Identifier MINECRAFT_CUSTOM_DATA_COMPONENT =
      Identifier.fromNamespaceAndPath(
          Constants.MINECRAFT_PREFIX, DataComponents.CUSTOM_DATA_ID);

  private ItemStackConditionMatcher() {}

  public static boolean isCustomDataValid(String customData) {
    return !hasContent(customData) || tryParse(customData) != null;
  }

  public static boolean isCustomDataComponentValid(String customDataComponent) {
    return !hasContent(customDataComponent)
        || Identifier.tryParse(customDataComponent.trim()) != null;
  }

  public static Item resolveItem(String itemName) {
    if (itemName == null || itemName.isEmpty()) {
      return null;
    }

    Identifier key = Identifier.tryParse(itemName);
    return key != null ? BuiltInRegistries.ITEM.getOptional(key).orElse(null) : null;
  }

  public static CompoundTag parseRequiredData(String customData) {
    return hasContent(customData) ? tryParse(customData) : null;
  }

  public static boolean matches(ItemStack stack, String itemName, String customData) {
    return matches(stack, itemName, customData, "");
  }

  public static boolean matches(
      ItemStack stack, String itemName, String customData, String customDataComponent) {
    if (!isCustomDataValid(customData)) {
      return false;
    }
    if (!isCustomDataComponentValid(customDataComponent)) {
      return false;
    }
    return matches(
        stack, resolveItem(itemName), parseRequiredData(customData), customDataComponent);
  }

  public static boolean matches(ItemStack stack, String itemName, CompoundTag requiredData) {
    return matches(stack, resolveItem(itemName), requiredData, "");
  }

  public static boolean matches(ItemStack stack, Item targetItem, CompoundTag requiredData) {
    return matches(stack, targetItem, requiredData, "");
  }

  public static boolean matches(
      ItemStack stack, String itemName, CompoundTag requiredData, String customDataComponent) {
    return matches(stack, resolveItem(itemName), requiredData, customDataComponent);
  }

  public static boolean matches(
      ItemStack stack, Item targetItem, CompoundTag requiredData, String customDataComponent) {
    if (targetItem == null || stack.isEmpty() || stack.getItem() != targetItem) {
      return false;
    }

    if (requiredData == null || requiredData.isEmpty()) {
      return true;
    }

    if (!isCustomDataComponentValid(customDataComponent)) {
      return false;
    }

    return hasContent(customDataComponent)
        ? matchesCustomData(getCustomData(stack, customDataComponent), requiredData)
        : matchesAnyCustomData(stack, requiredData);
  }

  private static boolean matchesAnyCustomData(ItemStack stack, CompoundTag requiredData) {
    return matchesCustomData(stack.get(DataComponents.CUSTOM_DATA), requiredData)
        || matchesCustomData(
            stack.get(net.minecraft.core.component.DataComponents.CUSTOM_DATA), requiredData);
  }

  private static boolean matchesCustomData(CustomData customData, CompoundTag requiredData) {
    return customData != null && containsAll(customData.copyTag(), requiredData);
  }

  private static CustomData getCustomData(ItemStack stack, String customDataComponent) {
    Identifier componentKey = Identifier.tryParse(customDataComponent.trim());
    if (componentKey == null) {
      return null;
    }
    if (EASY_NPC_CUSTOM_DATA_COMPONENT.equals(componentKey)) {
      return stack.get(DataComponents.CUSTOM_DATA);
    }
    if (MINECRAFT_CUSTOM_DATA_COMPONENT.equals(componentKey)) {
      return stack.get(net.minecraft.core.component.DataComponents.CUSTOM_DATA);
    }
    return null;
  }

  private static CompoundTag tryParse(String customData) {
    try {
      return TagParser.parseCompoundFully(normalizeCustomData(customData));
    } catch (Exception ignored) {
      return null;
    }
  }

  private static String normalizeCustomData(String customData) {
    String trimmedCustomData = customData.trim();
    return trimmedCustomData.startsWith("{") ? trimmedCustomData : "{" + trimmedCustomData + "}";
  }

  private static boolean hasContent(String customData) {
    return customData != null && !customData.trim().isEmpty();
  }

  private static boolean containsAll(CompoundTag actual, CompoundTag required) {
    for (String key : required.keySet()) {
      Tag actualValue = actual.get(key);
      Tag requiredValue = required.get(key);
      if (actualValue == null || requiredValue == null) {
        return false;
      }
      if (!matchesTag(actualValue, requiredValue)) {
        return false;
      }
    }

    return true;
  }

  private static boolean matchesTag(Tag actual, Tag required) {
    if (actual.getId() != required.getId()) {
      return false;
    }
    if (actual instanceof CompoundTag actualCompound
        && required instanceof CompoundTag requiredCompound) {
      return containsAll(actualCompound, requiredCompound);
    }

    return actual.equals(required);
  }
}
