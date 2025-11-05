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

import de.markusbordihn.easynpc.Constants;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public class ItemUtils {

  private static final ResourceLocation EASY_NPC_WAND_RESOURCE_LOCATION =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "easy_npc_wand");
  private static final ResourceLocation MOVE_EASY_NPC_RESOURCE_LOCATION =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, "move_easy_npc");
  private static Item cachedEasyNpcWandItem = null;
  private static Item cachedMoveEasyNpcItem = null;

  private ItemUtils() {}

  public static boolean isPlayerHoldingEasyNPCWand(Player player) {
    if (player == null) {
      return false;
    }

    Item easyNpcWandItem = getEasyNPCWandItem();
    if (easyNpcWandItem == null || easyNpcWandItem == Items.AIR) {
      return false;
    }

    return isPlayerHoldingItem(player, easyNpcWandItem);
  }

  public static boolean isPlayerHoldingItem(Player player, Item targetItem) {
    if (player == null || targetItem == null) {
      return false;
    }

    // Check main hand
    ItemStack mainHandItem = player.getMainHandItem();
    if (isItemStackOfType(mainHandItem, targetItem)) {
      return true;
    }

    // Check offhand
    ItemStack offHandItem = player.getOffhandItem();
    return isItemStackOfType(offHandItem, targetItem);
  }

  private static boolean isItemStackOfType(ItemStack itemStack, Item targetItem) {
    return !itemStack.isEmpty() && itemStack.getItem() == targetItem;
  }

  public static Item getEasyNPCWandItem() {
    if (cachedEasyNpcWandItem == null) {
      cachedEasyNpcWandItem = BuiltInRegistries.ITEM.get(EASY_NPC_WAND_RESOURCE_LOCATION);
    }
    return cachedEasyNpcWandItem;
  }

  public static Item getMoveEasyNPCItem() {
    if (cachedMoveEasyNpcItem == null) {
      cachedMoveEasyNpcItem = BuiltInRegistries.ITEM.get(MOVE_EASY_NPC_RESOURCE_LOCATION);
    }
    return cachedMoveEasyNpcItem;
  }

  public static boolean isPlayerHoldingMoveEasyNPC(Player player) {
    if (player == null) {
      return false;
    }

    Item moveEasyNpcItem = getMoveEasyNPCItem();
    if (moveEasyNpcItem == null || moveEasyNpcItem == Items.AIR) {
      return false;
    }

    return isPlayerHoldingItem(player, moveEasyNpcItem);
  }
}
