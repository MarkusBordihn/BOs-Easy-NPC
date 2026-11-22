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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.SmartAttackHandler;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.item.ItemStack;

public class SmartAttackGoal<T extends EasyNPC<?>> extends Goal {

  public static final int WEAPON_SELECTION_INTERVAL_TICKS = 20;
  public static final int RESTORE_MAIN_HAND_DELAY_TICKS = 100;

  private final T easyNPC;
  private final Mob mob;
  private LivingEntity lastTarget;
  private int nextWeaponSelectionTick;
  private int lastTargetTick;
  private int originalMainHandSlot = SmartAttackHandler.NO_SLOT;
  private ItemStack originalMainHandItem = ItemStack.EMPTY;

  public SmartAttackGoal(T easyNPC) {
    this.easyNPC = easyNPC;
    this.mob = easyNPC.getMob();
  }

  @Override
  public boolean canUse() {
    return this.mob.getTarget() != null || this.originalMainHandSlot != SmartAttackHandler.NO_SLOT;
  }

  @Override
  public void stop() {
    this.lastTarget = null;
  }

  @Override
  public void tick() {
    LivingEntity target = this.mob.getTarget();
    if (target == null) {
      if (this.mob.tickCount - this.lastTargetTick >= RESTORE_MAIN_HAND_DELAY_TICKS) {
        this.restoreOriginalMainHandItem();
      }
      return;
    }

    this.lastTargetTick = this.mob.tickCount;
    if ((target == this.lastTarget && this.mob.tickCount < this.nextWeaponSelectionTick)
        || this.mob.isUsingItem()) {
      return;
    }

    this.lastTarget = target;
    this.nextWeaponSelectionTick = this.mob.tickCount + WEAPON_SELECTION_INTERVAL_TICKS;
    int weaponSlot = SmartAttackHandler.findBetterWeaponSlot(this.easyNPC, target);
    if (weaponSlot == SmartAttackHandler.NO_SLOT) {
      return;
    }

    if (this.originalMainHandSlot == SmartAttackHandler.NO_SLOT) {
      this.originalMainHandSlot = weaponSlot;
      this.originalMainHandItem = this.mob.getMainHandItem();
    }
    this.swapMainHandWithInventorySlot(weaponSlot);
  }

  private void restoreOriginalMainHandItem() {
    if (this.originalMainHandSlot != SmartAttackHandler.NO_SLOT
        && this.mob.getMainHandItem() != this.originalMainHandItem
        && this.getInventory().getItem(this.originalMainHandSlot) == this.originalMainHandItem) {
      this.swapMainHandWithInventorySlot(this.originalMainHandSlot);
    }
    this.originalMainHandSlot = SmartAttackHandler.NO_SLOT;
    this.originalMainHandItem = ItemStack.EMPTY;
  }

  private void swapMainHandWithInventorySlot(int slot) {
    SimpleContainer inventory = this.getInventory();
    ItemStack mainHandItem = this.mob.getMainHandItem();
    this.mob.setItemSlot(EquipmentSlot.MAINHAND, inventory.getItem(slot));
    inventory.setItem(slot, mainHandItem);
  }

  private SimpleContainer getInventory() {
    return this.easyNPC.getEasyNPCInventoryData().getEasyNPCInventory();
  }
}
