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

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

public class ItemStackConditionMatcher {

  private ItemStackConditionMatcher() {}

  public static boolean isCustomDataValid(String customData) {
    return !hasContent(customData) || tryParse(customData) != null;
  }

  /**
   * Resolves the registry name to a single {@link Item} reference. Callers should resolve once and
   * reuse it for every stack instead of comparing registry name strings per slot.
   *
   * @return the matching item, or {@code null} when the name is malformed or unregistered.
   */
  public static Item resolveItem(String itemName) {
    if (itemName == null || itemName.isEmpty()) {
      return null;
    }

    ResourceLocation key = ResourceLocation.tryParse(itemName);
    return key != null && BuiltInRegistries.ITEM.containsKey(key)
        ? BuiltInRegistries.ITEM.get(key)
        : null;
  }

  /**
   * Parses the custom-data filter once so callers can reuse it across multiple stacks instead of
   * re-parsing the NBT string for every slot.
   *
   * @return the parsed filter, or {@code null} when empty or invalid (use {@link
   *     #isCustomDataValid(String)} to distinguish the two).
   */
  public static CompoundTag parseRequiredData(String customData) {
    return hasContent(customData) ? tryParse(customData) : null;
  }

  public static boolean matches(ItemStack stack, String itemName, String customData) {
    if (!isCustomDataValid(customData)) {
      return false;
    }
    return matches(stack, resolveItem(itemName), parseRequiredData(customData));
  }

  public static boolean matches(ItemStack stack, String itemName, CompoundTag requiredData) {
    return matches(stack, resolveItem(itemName), requiredData);
  }

  public static boolean matches(ItemStack stack, Item targetItem, CompoundTag requiredData) {
    if (targetItem == null || stack.isEmpty() || stack.getItem() != targetItem) {
      return false;
    }

    if (requiredData == null || requiredData.isEmpty()) {
      return true;
    }

    CompoundTag stackData = stack.getTag();
    return stackData != null && containsAll(stackData, requiredData);
  }

  private static CompoundTag tryParse(String customData) {
    try {
      return TagParser.parseTag(customData.trim());
    } catch (Exception ignored) {
      return null;
    }
  }

  private static boolean hasContent(String customData) {
    return customData != null && !customData.trim().isEmpty();
  }

  private static boolean containsAll(CompoundTag actual, CompoundTag required) {
    for (String key : required.getAllKeys()) {
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
