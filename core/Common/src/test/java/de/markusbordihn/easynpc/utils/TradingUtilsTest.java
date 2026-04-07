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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.nbt.TagParser;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TradingUtilsTest {

  private CompoundTag loadCompoundTagResource(String resourcePath)
      throws IOException, CommandSyntaxException {
    try (InputStream inputStream = getClass().getResourceAsStream(resourcePath)) {
      assertNotNull(inputStream, "Test resource not found: " + resourcePath);
      String snbt = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
      return TagParser.parseTag(snbt);
    }
  }

  @Test
  @DisplayName("Should extract recipes from direct legacy offer list")
  void testExtractRecipesListFromDirectList() throws CommandSyntaxException {
    CompoundTag root = TagParser.parseTag("{Offers:[{sell:{}},{sell:{}}]}");
    ListTag recipesList = TradingUtils.extractRecipesList(root.get("Offers"));

    assertNotNull(recipesList);
    assertEquals(2, recipesList.size());
  }

  @Test
  @DisplayName("Should extract recipes from double nested legacy offers")
  void testExtractRecipesListFromDoubleNestedOffers() throws CommandSyntaxException {
    CompoundTag root = TagParser.parseTag("{Offers:{Inventory:{},Recipes:{Recipes:[{sell:{}}]}}}");
    ListTag recipesList = TradingUtils.extractRecipesList(root.get("Offers"));

    assertNotNull(recipesList);
    assertEquals(1, recipesList.size());
  }

  @Test
  @DisplayName("Should migrate legacy trade entries to codec-friendly item compounds")
  void testMigrateLegacyTradeEntry() throws CommandSyntaxException {
    CompoundTag tradeEntry =
        TagParser.parseTag(
            "{buy:{Count:2b,id:\"minecraft:emerald\",tag:{foo:\"bar\"}},sell:{Count:1b,id:\"minecraft:stone\"}}");
    Tag migratedTag = TradingUtils.migrateLegacyTradeEntry(tradeEntry);

    assertInstanceOf(CompoundTag.class, migratedTag);
    CompoundTag migratedEntry = (CompoundTag) migratedTag;
    CompoundTag migratedBuy = migratedEntry.getCompound("buy");
    CompoundTag migratedSell = migratedEntry.getCompound("sell");

    assertFalse(migratedBuy.contains("Count"));
    assertEquals(2, migratedBuy.getInt("count"));
    assertFalse(migratedBuy.contains("tag"));
    assertFalse(migratedSell.contains("Count"));
    assertEquals(1, migratedSell.getInt("count"));
  }

  @Test
  @DisplayName("Should migrate legacy item tag data into modern item components")
  void testMigrateLegacyItemTagToComponents() throws CommandSyntaxException {
    CompoundTag tradeEntry =
        TagParser.parseTag(
            """
            {
              sell:{
                Count:1b,
                id:"minecraft:diamond_sword",
                tag:{
                  Damage:7,
                  RepairCost:3,
                  CustomModelData:123,
                  Unbreakable:1b,
                  HideFlags:69,
                  Enchantments:[{id:"minecraft:sharpness",lvl:3s}],
                  display:{
                    Name:"Legendary Relic",
                    Lore:["First line",'{"text":"Second line"}'],
                    color:16711680
                  },
                  foo:1b
                }
              }
            }""");

    CompoundTag migratedEntry = (CompoundTag) TradingUtils.migrateLegacyTradeEntry(tradeEntry);
    CompoundTag migratedSell = migratedEntry.getCompound("sell");
    CompoundTag components = migratedSell.getCompound("components");

    assertFalse(migratedSell.contains("Count"));
    assertFalse(migratedSell.contains("tag"));
    assertEquals(1, migratedSell.getInt("count"));
    assertEquals("{\"text\":\"Legendary Relic\"}", components.getString("minecraft:custom_name"));

    ListTag lore = components.getList("minecraft:lore", Tag.TAG_STRING);
    assertEquals(2, lore.size());
    assertEquals("{\"text\":\"First line\"}", lore.getString(0));
    assertEquals("{\"text\":\"Second line\"}", lore.getString(1));

    assertEquals(7, components.getInt("minecraft:damage"));
    assertEquals(3, components.getInt("minecraft:repair_cost"));
    assertTrue(components.contains("minecraft:unbreakable", Tag.TAG_COMPOUND));

    CompoundTag dyedColor = components.getCompound("minecraft:dyed_color");
    assertEquals(16711680, dyedColor.getInt("rgb"));
    assertFalse(dyedColor.getBoolean("show_in_tooltip"));

    CompoundTag enchantments = components.getCompound("minecraft:enchantments");
    assertEquals(3, enchantments.getCompound("levels").getInt("minecraft:sharpness"));
    assertFalse(enchantments.getBoolean("show_in_tooltip"));

    CompoundTag customModelData = components.getCompound("minecraft:custom_model_data");
    assertEquals(123.0f, customModelData.getList("floats", Tag.TAG_FLOAT).getFloat(0));

    CompoundTag customData = components.getCompound("minecraft:custom_data");
    assertEquals(1, customData.getByte("foo"));
  }

  @Test
  @DisplayName("Should keep existing modern components when migrating legacy item tag data")
  void testPreserveExistingComponentsOnLegacyMigration() throws CommandSyntaxException {
    CompoundTag tradeEntry =
        TagParser.parseTag(
            """
            {
              sell:{
                id:"minecraft:paper",
                count:1,
                components:{
                  "minecraft:custom_name":'{"text":"Modern Name"}',
                  "minecraft:custom_data":{existing:1b}
                },
                tag:{
                  display:{Name:"Legacy Name"},
                  foo:1b
                }
              }
            }""");

    CompoundTag migratedEntry = (CompoundTag) TradingUtils.migrateLegacyTradeEntry(tradeEntry);
    CompoundTag migratedSell = migratedEntry.getCompound("sell");
    CompoundTag components = migratedSell.getCompound("components");
    CompoundTag customData = components.getCompound("minecraft:custom_data");

    assertEquals("{\"text\":\"Modern Name\"}", components.getString("minecraft:custom_name"));
    assertEquals(1, customData.getByte("existing"));
    assertEquals(1, customData.getByte("foo"));
    assertTrue(customData.contains("display", Tag.TAG_COMPOUND));
    assertEquals("Legacy Name", customData.getCompound("display").getString("Name"));
  }

  @Test
  @DisplayName("Should parse mixed offers with components as direct legacy list")
  void testMixedOffersWithComponentsResource() throws IOException, CommandSyntaxException {
    CompoundTag root =
        loadCompoundTagResource(
            "/de/markusbordihn/easynpc/utils/trading/mixed_offers_with_components.snbt");

    ListTag recipesList = TradingUtils.extractRecipesList(root.get("Offers"));
    assertNotNull(recipesList);
    assertEquals(7, recipesList.size());

    CompoundTag firstEntry = (CompoundTag) TradingUtils.migrateLegacyTradeEntry(recipesList.get(0));
    assertEquals("cobblemon:sky_tumblestone", firstEntry.getCompound("buy").getString("id"));
    assertEquals(6, firstEntry.getCompound("buy").getInt("count"));
    assertEquals("cobblemon:hp_up", firstEntry.getCompound("sell").getString("id"));

    CompoundTag specialEntry =
        (CompoundTag) TradingUtils.migrateLegacyTradeEntry(recipesList.get(6));
    CompoundTag specialSell = specialEntry.getCompound("sell");
    CompoundTag components = specialSell.getCompound("components");
    assertEquals("cobblemon:suspicious_sherd", specialSell.getString("id"));
    assertEquals(1, specialSell.getInt("count"));
    assertTrue(specialSell.contains("components"));
    assertEquals(
        "\"* RELIQUIA DA TERRA DO BITTO *\"", components.getString("minecraft:custom_name"));
  }

  @Test
  @DisplayName("Should parse multi offers with secondary buy as direct legacy list")
  void testMultiOffersWithSecondaryBuyResource() throws IOException, CommandSyntaxException {
    CompoundTag root =
        loadCompoundTagResource(
            "/de/markusbordihn/easynpc/utils/trading/multi_offers_with_secondary_buy.snbt");

    ListTag recipesList = TradingUtils.extractRecipesList(root.get("Offers"));

    assertNotNull(recipesList);
    assertEquals(12, recipesList.size());

    CompoundTag lastEntry = (CompoundTag) TradingUtils.migrateLegacyTradeEntry(recipesList.get(11));
    assertEquals("minecraft:iron_ingot", lastEntry.getCompound("buy").getString("id"));
    assertEquals("cobblemon:sport_ball", lastEntry.getCompound("sell").getString("id"));
    assertEquals(6, lastEntry.getCompound("sell").getInt("count"));
  }

  @Test
  @DisplayName("Should parse simple offers with xp as direct legacy list")
  void testSimpleOffersWithXpResource() throws IOException, CommandSyntaxException {
    CompoundTag root =
        loadCompoundTagResource(
            "/de/markusbordihn/easynpc/utils/trading/simple_offers_with_xp.snbt");

    ListTag recipesList = TradingUtils.extractRecipesList(root.get("Offers"));

    assertNotNull(recipesList);
    assertEquals(2, recipesList.size());

    CompoundTag secondEntry =
        (CompoundTag) TradingUtils.migrateLegacyTradeEntry(recipesList.get(1));
    assertEquals("minecraft:emerald", secondEntry.getCompound("buy").getString("id"));
    assertEquals(3, secondEntry.getCompound("buy").getInt("count"));
    assertEquals("cobblemon:revive", secondEntry.getCompound("sell").getString("id"));
  }
}
