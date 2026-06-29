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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ItemStackConditionMatcherTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  @DisplayName("Item type matches without custom data")
  void testItemTypeMatchesWithoutCustomData() {
    assertTrue(
        ItemStackConditionMatcher.matches(new ItemStack(Items.DIAMOND), "minecraft:diamond", ""));
    assertFalse(
        ItemStackConditionMatcher.matches(new ItemStack(Items.EMERALD), "minecraft:diamond", ""));
  }

  @Test
  @DisplayName("Required custom data matches exact item data")
  void testRequiredCustomDataMatchesExactItemData() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "first_diamond");
    itemStack.setTag(tag);

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"first_diamond\"}"));
  }

  @Test
  @DisplayName("Required custom data matches when item has additional data")
  void testRequiredCustomDataMatchesWithAdditionalItemData() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "first_diamond");
    tag.putInt("Uses", 3);
    itemStack.setTag(tag);

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"first_diamond\"}"));
  }

  @Test
  @DisplayName("Nested custom data is matched as a subset")
  void testNestedCustomDataMatchesAsSubset() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag displayTag = new CompoundTag();
    displayTag.putString("Name", "{\"text\":\"Quest Diamond\"}");
    displayTag.putInt("color", 16711680);
    CompoundTag itemTag = new CompoundTag();
    itemTag.put("display", displayTag);
    itemStack.setTag(itemTag);

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{display:{Name:'{\"text\":\"Quest Diamond\"}'}}"));
  }

  @Test
  @DisplayName("Missing or different required custom data does not match")
  void testMissingOrDifferentRequiredCustomDataDoesNotMatch() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "first_diamond");
    itemStack.setTag(tag);

    assertFalse(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"second_diamond\"}"));
    assertFalse(
        ItemStackConditionMatcher.matches(
            new ItemStack(Items.DIAMOND), "minecraft:diamond", "{QuestId:\"first_diamond\"}"));
  }

  @Test
  @DisplayName("Invalid custom data is invalid and never matches")
  void testInvalidCustomDataDoesNotMatch() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "first_diamond");
    itemStack.setTag(tag);

    assertFalse(ItemStackConditionMatcher.isCustomDataValid("{QuestId:"));
    assertFalse(ItemStackConditionMatcher.matches(itemStack, "minecraft:diamond", "{QuestId:"));
  }

  @Test
  @DisplayName("Item names are resolved once to a single registry reference")
  void testResolveItem() {
    assertSame(Items.DIAMOND, ItemStackConditionMatcher.resolveItem("minecraft:diamond"));
    assertNull(ItemStackConditionMatcher.resolveItem("minecraft:not_a_real_item"));
    assertNull(ItemStackConditionMatcher.resolveItem("definitely not a resource location"));
    assertNull(ItemStackConditionMatcher.resolveItem(""));
    assertNull(ItemStackConditionMatcher.resolveItem(null));
  }

  @Test
  @DisplayName("Pre-parsed custom data is reused across stacks")
  void testMatchesWithPreParsedData() {
    CompoundTag requiredData = ItemStackConditionMatcher.parseRequiredData("{QuestId:\"diamond\"}");

    ItemStack matching = new ItemStack(Items.DIAMOND);
    CompoundTag matchingTag = new CompoundTag();
    matchingTag.putString("QuestId", "diamond");
    matching.setTag(matchingTag);

    assertTrue(ItemStackConditionMatcher.matches(matching, Items.DIAMOND, requiredData));
    assertFalse(
        ItemStackConditionMatcher.matches(
            new ItemStack(Items.DIAMOND), Items.DIAMOND, requiredData));
    assertTrue(
        ItemStackConditionMatcher.matches(new ItemStack(Items.DIAMOND), Items.DIAMOND, null));
    assertNull(ItemStackConditionMatcher.parseRequiredData(""));
  }
}
