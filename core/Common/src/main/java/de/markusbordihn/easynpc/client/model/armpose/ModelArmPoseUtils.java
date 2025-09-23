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

package de.markusbordihn.easynpc.client.model.armpose;

import de.markusbordihn.easynpc.data.model.ModelArmPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ItemUseAnimation;

public class ModelArmPoseUtils {

  public static ModelArmPose getArmPoseForLeftArm(EasyNPC<?> easyNPC) {
    return calculateArmPose(easyNPC, false);
  }

  public static ModelArmPose getArmPoseForRightArm(EasyNPC<?> easyNPC) {
    return calculateArmPose(easyNPC, true);
  }

  private static ModelArmPose calculateArmPose(EasyNPC<?> easyNPC, boolean isRightArm) {
    if (easyNPC == null) {
      return ModelArmPose.DEFAULT;
    }

    LivingEntity livingEntity = easyNPC.getLivingEntity();
    boolean isRightHanded = livingEntity.getMainArm() == HumanoidArm.RIGHT;

    // Check if entity is using an item
    if (livingEntity.isUsingItem()) {
      return getArmPoseWhileUsingItem(livingEntity, isRightArm, isRightHanded);
    }

    // Get idle pose when not using item
    return getIdleArmPose(easyNPC, livingEntity, isRightArm, isRightHanded);
  }

  private static ModelArmPose getArmPoseWhileUsingItem(
      LivingEntity livingEntity, boolean isRightArm, boolean isRightHanded) {
    ItemStack useItem = livingEntity.getUseItem();
    if (useItem.isEmpty()) {
      return ModelArmPose.DEFAULT;
    }

    InteractionHand usedHand = livingEntity.getUsedItemHand();
    boolean isUsingMainHand = usedHand == InteractionHand.MAIN_HAND;
    boolean isUsingRightHand =
        (isRightHanded && isUsingMainHand) || (!isRightHanded && !isUsingMainHand);

    // Return DEFAULT if this arm is not the one being used
    if (isRightArm != isUsingRightHand) {
      return ModelArmPose.DEFAULT;
    }

    // Determine pose based on item use animation
    ItemUseAnimation itemUseAnimation = useItem.getUseAnimation();
    switch (itemUseAnimation) {
      case BOW:
        return ModelArmPose.BOW_AND_ARROW;
      case CROSSBOW:
        return ModelArmPose.CROSSBOW_CHARGE;
      case SPYGLASS:
        return ModelArmPose.SPYGLASS;
      case SPEAR:
        return ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
      default:
        return ModelArmPose.DEFAULT;
    }
  }

  private static ModelArmPose getIdleArmPose(
      EasyNPC<?> easyNPC, LivingEntity livingEntity, boolean isRightArm, boolean isRightHanded) {

    // Check if entity is aggressive
    boolean isAggressive =
        (easyNPC.getPathfinderMob().getTarget() != null)
            || (livingEntity instanceof Mob mob && mob.isAggressive());

    // Only show special poses when aggressive
    if (!isAggressive) {
      return ModelArmPose.DEFAULT;
    }

    // Get the item in the current arm
    ItemStack mainHandItem = livingEntity.getMainHandItem();
    ItemStack offHandItem = livingEntity.getOffhandItem();

    // Determine which item is in which arm based on handedness
    ItemStack itemInRightArm = isRightHanded ? mainHandItem : offHandItem;
    ItemStack itemInLeftArm = isRightHanded ? offHandItem : mainHandItem;
    ItemStack currentArmItem = isRightArm ? itemInRightArm : itemInLeftArm;

    if (currentArmItem.isEmpty()) {
      return ModelArmPose.DEFAULT;
    }

    // Check for specific weapon types
    if (currentArmItem.getItem() instanceof CrossbowItem) {
      return ModelArmPose.CROSSBOW_HOLD;
    } else if (AttackHandler.isBowWeapon(currentArmItem)) {
      return ModelArmPose.BOW_AND_ARROW;
    } else if (AttackHandler.isGunWeapon(currentArmItem)) {
      return ModelArmPose.GUN_HOLD;
    } else if (AttackHandler.isMeleeWeapon(currentArmItem)) {
      return ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
    }

    return ModelArmPose.DEFAULT;
  }
}
