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
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.EnchantmentHelper;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class HandSlot extends Slot {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final EquipmentSlot[] SLOT_IDS =
      new EquipmentSlot[] {EquipmentSlot.MAINHAND, EquipmentSlot.OFFHAND};

  public final int slotIndex;
  final EquipmentSlot equipmentSlot;
  final EquipmentConfigurationMenu menu;

  public HandSlot(
      EquipmentConfigurationMenu menu, Container container, int slotIndex, int x, int y) {
    super(container, slotIndex, x, y);
    this.slotIndex = slotIndex;
    this.menu = menu;
    this.equipmentSlot = SLOT_IDS[slotIndex];
  }

  public EquipmentSlot getEquipmentSlot() {
    return this.equipmentSlot;
  }

  @Override
  public void set(ItemStack itemStack) {
    super.set(itemStack);

    this.menu.setHandChanged(
        this.slotIndex == 0 ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND, itemStack);
  }

  @Override
  public boolean mayPlace(ItemStack itemStack) {
    if (itemStack.isEmpty()) {
      return false;
    }
    EquipmentSlot equipmentSlotForItem = LivingEntity.getEquipmentSlotForItem(itemStack);
    return this.equipmentSlot == EquipmentSlot.OFFHAND
            && equipmentSlotForItem == EquipmentSlot.OFFHAND
        || this.equipmentSlot == EquipmentSlot.MAINHAND
            && equipmentSlotForItem == EquipmentSlot.MAINHAND
        || this.equipmentSlot == EquipmentSlot.OFFHAND
            && equipmentSlotForItem == EquipmentSlot.MAINHAND;
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
    if (this.equipmentSlot == EquipmentSlot.OFFHAND) {
      return Pair.of(InventoryMenu.BLOCK_ATLAS, InventoryMenu.EMPTY_ARMOR_SLOT_SHIELD);
    }
    return null;
  }
}
