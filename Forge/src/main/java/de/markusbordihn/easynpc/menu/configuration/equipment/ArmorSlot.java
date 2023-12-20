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

package de.markusbordihn.easynpc.menu.configuration.equipment;

import com.mojang.datafixers.util.Pair;
import de.markusbordihn.easynpc.Constants;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.EnchantmentHelper;

public class ArmorSlot extends Slot {

  public static final ResourceLocation EMPTY_ARMOR_SLOT_HELMET =
      new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_helmet");
  public static final ResourceLocation EMPTY_ARMOR_SLOT_CHESTPLATE =
      new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_chestplate");
  public static final ResourceLocation EMPTY_ARMOR_SLOT_LEGGINGS =
      new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_leggings");
  public static final ResourceLocation EMPTY_ARMOR_SLOT_BOOTS =
      new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_boots");
  static final ResourceLocation[] TEXTURE_EMPTY_SLOTS =
      new ResourceLocation[]{
          EMPTY_ARMOR_SLOT_BOOTS,
          EMPTY_ARMOR_SLOT_LEGGINGS,
          EMPTY_ARMOR_SLOT_CHESTPLATE,
          EMPTY_ARMOR_SLOT_HELMET
      };
  private static final EquipmentSlot[] SLOT_IDS =
      new EquipmentSlot[]{
          EquipmentSlot.FEET, EquipmentSlot.LEGS, EquipmentSlot.CHEST, EquipmentSlot.HEAD
      };

  final EquipmentSlot equipmentSlot;
  final EquipmentConfigurationMenu menu;

  public ArmorSlot(EquipmentConfigurationMenu menu, Container container, int index, int x, int y) {
    super(container, index, x, y);
    this.menu = menu;
    this.equipmentSlot = SLOT_IDS[index];
  }

  @Override
  public void set(ItemStack itemStack) {
    super.set(itemStack);

    this.menu.setArmorChanged(this.equipmentSlot, this.getSlotIndex(), itemStack);
  }

  @Override
  public int getMaxStackSize() {
    return 1;
  }

  @Override
  public boolean mayPlace(ItemStack itemStack) {
    return this.equipmentSlot == LivingEntity.getEquipmentSlotForItem(itemStack);
  }

  @Override
  public boolean mayPickup(Player player) {
    ItemStack itemStack = this.getItem();
    return !itemStack.isEmpty()
        && (player.isCreative() || !EnchantmentHelper.hasBindingCurse(itemStack))
        && super.mayPickup(player);
  }

  @Override
  public Pair<ResourceLocation, ResourceLocation> getNoItemIcon() {
    return Pair.of(InventoryMenu.BLOCK_ATLAS, TEXTURE_EMPTY_SLOTS[this.equipmentSlot.getIndex()]);
  }
}
