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

package de.markusbordihn.easynpc.menu.configuration.equipment.slot;

import com.mojang.datafixers.util.Pair;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
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

  private static final ResourceLocation[] TEXTURE_EMPTY_SLOTS =
      new ResourceLocation[] {
        InventoryMenu.EMPTY_ARMOR_SLOT_BOOTS,
        InventoryMenu.EMPTY_ARMOR_SLOT_LEGGINGS,
        InventoryMenu.EMPTY_ARMOR_SLOT_CHESTPLATE,
        InventoryMenu.EMPTY_ARMOR_SLOT_HELMET
      };
  private static final EquipmentSlot[] SLOT_IDS =
      new EquipmentSlot[] {
        EquipmentSlot.FEET, EquipmentSlot.LEGS, EquipmentSlot.CHEST, EquipmentSlot.HEAD
      };
  public final int slotIndex;
  final EquipmentSlot equipmentSlot;
  final EquipmentConfigurationMenu menu;

  public ArmorSlot(
      EquipmentConfigurationMenu menu, Container container, int slotIndex, int x, int y) {
    super(container, slotIndex, x, y);
    this.slotIndex = slotIndex;
    this.menu = menu;
    this.equipmentSlot = SLOT_IDS[slotIndex];
  }

  @Override
  public void set(ItemStack itemStack) {
    super.set(itemStack);

    this.menu.setArmorChanged(this.equipmentSlot, this.slotIndex, itemStack);
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
