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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import net.minecraft.SharedConstants;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.RegistryOps;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.ItemLore;
import net.minecraft.world.item.trading.ItemCost;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TradingUtilsItemCostTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static RegistryOps<Tag> registryOps() {
    RegistryAccess.Frozen registries =
        RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
    return registries.createSerializationContext(NbtOps.INSTANCE);
  }

  @Test
  @DisplayName("Should keep modified trade item matching stable across codec and rebuild")
  void testModifiedItemCostMatchingSurvivesCodecAndRebuild() {
    ItemStack questItem = new ItemStack(Items.SUGAR);
    questItem.set(DataComponents.CUSTOM_NAME, Component.literal("Lethal Bio-Reagent"));
    questItem.set(
        DataComponents.LORE, new ItemLore(List.of(Component.literal("It singes at your skin..."))));
    questItem.remove(DataComponents.ATTRIBUTE_MODIFIERS);

    ItemCost itemCost = TradingUtils.getItemCost(questItem);
    ItemCost loadedItemCost =
        ItemCost.CODEC
            .parse(registryOps(), ItemCost.CODEC.encodeStart(registryOps(), itemCost).getOrThrow())
            .getOrThrow();
    ItemCost rebuiltItemCost = TradingUtils.getItemCost(loadedItemCost.itemStack());

    assertTrue(itemCost.test(questItem));
    assertTrue(loadedItemCost.test(questItem));
    assertTrue(rebuiltItemCost.test(questItem));
    assertFalse(rebuiltItemCost.test(new ItemStack(Items.SUGAR)));
    assertEquals(
        itemCost.itemStack().getComponentsPatch(), loadedItemCost.itemStack().getComponentsPatch());
    assertEquals(
        loadedItemCost.itemStack().getComponentsPatch(),
        rebuiltItemCost.itemStack().getComponentsPatch());
  }
}
