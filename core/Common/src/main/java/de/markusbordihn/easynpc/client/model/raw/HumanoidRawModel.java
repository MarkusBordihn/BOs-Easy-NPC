/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.client.model.raw;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import net.minecraft.client.model.HumanoidModel.ArmPose;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;

public class HumanoidRawModel {

  public static ArmPose getArmPose(
    LivingEntity entityLiving, InteractionHand hand, ItemStack itemStack) {
    if (entityLiving == null || hand == null || itemStack.isEmpty()) {
      return ArmPose.EMPTY;
    }

    // Get EasyNPC
    if (!(entityLiving instanceof EasyNPC<?> easyNPC)) {
      return ArmPose.EMPTY;
    }

    // Get Item in Main Hand
    boolean isAggressive = easyNPC instanceof Mob mob && mob.isAggressive();

    // Bow arm pose
    if (isAggressive && AttackHandler.isBowWeapon(itemStack)) {
      return ArmPose.BOW_AND_ARROW;
    }

    // Crossbow arm pose
    AttackDataCapable<?> attackData = easyNPC.getEasyNPCAttackData();
    if (AttackHandler.isCrossbowWeapon(itemStack) && attackData != null) {
      if (attackData.isChargingCrossbow()) {
        return ArmPose.CROSSBOW_CHARGE;
      } else if (isAggressive) {
        return ArmPose.CROSSBOW_HOLD;
      }
    }

    return ArmPose.ITEM;
  }
}
