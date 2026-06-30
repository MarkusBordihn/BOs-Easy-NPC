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

import de.markusbordihn.easynpc.component.DataComponents;
import net.minecraft.SharedConstants;
import net.minecraft.core.component.DataComponentInitializers;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.registries.VanillaRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.CustomData;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ItemStackConditionMatcherTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    BuiltInRegistries.DATA_COMPONENT_INITIALIZERS
        .build(VanillaRegistries.createLookup())
        .forEach(DataComponentInitializers.PendingComponents::apply);
    DataComponentType<CustomData> customDataComponent =
        DataComponentType.<CustomData>builder()
            .persistent(CustomData.CODEC)
            .networkSynchronized(CustomData.STREAM_CODEC)
            .build();
    DataComponents.registerCustomData(() -> customDataComponent);
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
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(tag));

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"first_diamond\"}"));
  }

  @Test
  @DisplayName("Required custom data accepts shorthand compound data")
  void testRequiredCustomDataMatchesShorthandCompoundData() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "first_diamond");
    tag.putString("Color", "gold");
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(tag));

    assertTrue(ItemStackConditionMatcher.isCustomDataValid("QuestId:\"first_diamond\""));
    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "QuestId:\"first_diamond\",Color:\"gold\""));
  }

  @Test
  @DisplayName("Required custom data matches when item has additional data")
  void testRequiredCustomDataMatchesWithAdditionalItemData() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "first_diamond");
    tag.putInt("Uses", 3);
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(tag));

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
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(itemTag));

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
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(tag));

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
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(tag));

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
    matching.set(DataComponents.CUSTOM_DATA, CustomData.of(matchingTag));

    assertTrue(ItemStackConditionMatcher.matches(matching, Items.DIAMOND, requiredData));
    assertFalse(
        ItemStackConditionMatcher.matches(
            new ItemStack(Items.DIAMOND), Items.DIAMOND, requiredData));
    assertTrue(
        ItemStackConditionMatcher.matches(new ItemStack(Items.DIAMOND), Items.DIAMOND, null));
    assertNull(ItemStackConditionMatcher.parseRequiredData(""));
  }

  @Test
  @DisplayName("Legacy vanilla custom data remains supported")
  void testLegacyVanillaCustomDataFallback() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag tag = new CompoundTag();
    tag.putString("QuestId", "legacy_diamond");
    itemStack.set(net.minecraft.core.component.DataComponents.CUSTOM_DATA, CustomData.of(tag));

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"legacy_diamond\"}"));
  }

  @Test
  @DisplayName("Unqualified custom data matches any known custom data component")
  void testUnqualifiedCustomDataMatchesAnyKnownComponent() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag easyNpcTag = new CompoundTag();
    easyNpcTag.putString("QuestId", "easy_npc_diamond");
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(easyNpcTag));
    CompoundTag vanillaTag = new CompoundTag();
    vanillaTag.putString("QuestId", "vanilla_diamond");
    itemStack.set(
        net.minecraft.core.component.DataComponents.CUSTOM_DATA, CustomData.of(vanillaTag));

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"easy_npc_diamond\"}"));
    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"vanilla_diamond\"}"));
  }

  @Test
  @DisplayName("Qualified custom data matches only the selected component")
  void testQualifiedCustomDataMatchesSelectedComponentOnly() {
    ItemStack itemStack = new ItemStack(Items.DIAMOND);
    CompoundTag easyNpcTag = new CompoundTag();
    easyNpcTag.putString("QuestId", "easy_npc_diamond");
    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(easyNpcTag));
    CompoundTag vanillaTag = new CompoundTag();
    vanillaTag.putString("QuestId", "vanilla_diamond");
    itemStack.set(
        net.minecraft.core.component.DataComponents.CUSTOM_DATA, CustomData.of(vanillaTag));

    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack,
            "minecraft:diamond",
            "{QuestId:\"easy_npc_diamond\"}",
            "easy_npc:custom_data"));
    assertFalse(
        ItemStackConditionMatcher.matches(
            itemStack,
            "minecraft:diamond",
            "{QuestId:\"vanilla_diamond\"}",
            "easy_npc:custom_data"));
    assertTrue(
        ItemStackConditionMatcher.matches(
            itemStack,
            "minecraft:diamond",
            "{QuestId:\"vanilla_diamond\"}",
            "minecraft:custom_data"));
    assertFalse(
        ItemStackConditionMatcher.matches(
            itemStack,
            "minecraft:diamond",
            "{QuestId:\"easy_npc_diamond\"}",
            "minecraft:custom_data"));
    assertFalse(
        ItemStackConditionMatcher.matches(
            itemStack, "minecraft:diamond", "{QuestId:\"easy_npc_diamond\"}", "minecraft:lore"));
  }
}
