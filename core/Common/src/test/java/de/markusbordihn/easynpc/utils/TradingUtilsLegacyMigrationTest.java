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

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import net.minecraft.ChatFormatting;
import net.minecraft.SharedConstants;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import net.minecraft.network.chat.TextColor;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.trading.MerchantOffers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TradingUtilsLegacyMigrationTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static RegistryAccess.Frozen registries() {
    return RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
  }

  @Test
  @DisplayName("Legacy display name/lore migrate to proper text components, not raw JSON strings")
  void testLegacyDisplayNameMigratesToComponent() throws Exception {
    CompoundTag offersTag =
        TagParser.parseCompoundFully(
            "{Offers:{Recipes:[{"
                + "buy:{id:\"minecraft:emerald\",count:1},"
                + "sell:{Count:1b,id:\"minecraft:chest\",tag:{display:{"
                + "Name:'{\"text\":\"Surprise Chest\",\"color\":\"gold\",\"italic\":false}',"
                + "Lore:['{\"text\":\"A small cache of simple useful items.\","
                + "\"color\":\"gray\",\"italic\":false}']"
                + "}}},"
                + "maxUses:64,priceMultiplier:1.0f,rewardExp:1b}]}}");

    MerchantOffers offers =
        TradingUtils.parseMerchantOffers(
            offersTag, "Offers", registries(), "legacy-migration-test");

    assertNotNull(offers, "Migrated offers should not be null");
    assertEquals(1, offers.size());

    ItemStack result = offers.get(0).getResult();
    assertEquals(
        "Surprise Chest",
        result.getHoverName().getString(),
        "Custom name should render as plain text, not as a raw JSON string");

    TextColor color = result.getHoverName().getStyle().getColor();
    assertNotNull(color, "Custom name color should be preserved during migration");
    assertEquals(TextColor.fromLegacyFormat(ChatFormatting.GOLD), color);
  }
}
