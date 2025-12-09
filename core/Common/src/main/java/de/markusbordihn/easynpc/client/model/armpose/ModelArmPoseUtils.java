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

public class ModelArmPoseUtils {

  public static ModelArmPose getArmPoseForLeftArm(final EasyNPC<?> easyNPC) {
    return getArmPose(easyNPC, false);
  }

  public static ModelArmPose getArmPoseForRightArm(final EasyNPC<?> easyNPC) {
    return getArmPose(easyNPC, true);
  }

  private static ModelArmPose getArmPose(final EasyNPC<?> easyNPC, final boolean isRightArm) {
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
      final LivingEntity livingEntity, final boolean isRightArm, final boolean isRightHanded) {
    ItemStack useItem = livingEntity.getUseItem();
    if (useItem.isEmpty()) {
      return ModelArmPose.DEFAULT;
    }

    // Determine pose based on item use animation
    ModelArmPose itemUseModelArmPose =
        switch (useItem.getUseAnimation()) {
          case BOW -> ModelArmPose.BOW_AND_ARROW;
          case CROSSBOW -> ModelArmPose.CROSSBOW_CHARGE;
          case SPYGLASS -> ModelArmPose.SPYGLASS;
          case SPEAR -> ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
          default -> ModelArmPose.DEFAULT;
        };

    // For two-handed weapons (bow, crossbow, gun), apply pose to both arms
    if (itemUseModelArmPose == ModelArmPose.BOW_AND_ARROW
        || itemUseModelArmPose == ModelArmPose.CROSSBOW_CHARGE) {
      return itemUseModelArmPose;
    }

    // Check if we should use the GUN_HOLD pose (also two-handed)
    if (AttackHandler.isGunWeapon(useItem)) {
      return ModelArmPose.GUN_HOLD;
    }

    // For other items, only apply pose to the arm being used
    boolean isUsingMainHand = livingEntity.getUsedItemHand() == InteractionHand.MAIN_HAND;
    boolean isUsingRightHand =
        (isRightHanded && isUsingMainHand) || (!isRightHanded && !isUsingMainHand);
    if (isRightArm != isUsingRightHand) {
      return ModelArmPose.DEFAULT;
    }

    return itemUseModelArmPose;
  }

  private static ModelArmPose getIdleArmPose(
      final EasyNPC<?> easyNPC,
      final LivingEntity livingEntity,
      final boolean isRightArm,
      final boolean isRightHanded) {

    // Only show special poses when aggressive
    if (!((easyNPC.getPathfinderMob().getTarget() != null)
        || (livingEntity instanceof Mob mob && mob.isAggressive()))) {
      return ModelArmPose.DEFAULT;
    }

    // Get the item in the main hand and offhand
    ItemStack mainHandItem = livingEntity.getMainHandItem();
    ItemStack offHandItem = livingEntity.getOffhandItem();

    // Check main hand for two-handed weapons first (applies to both arms)
    if (!mainHandItem.isEmpty()) {
      if (mainHandItem.getItem() instanceof CrossbowItem) {
        return ModelArmPose.CROSSBOW_HOLD;
      } else if (AttackHandler.isBowWeapon(mainHandItem)) {
        return ModelArmPose.BOW_AND_ARROW;
      } else if (AttackHandler.isGunWeapon(mainHandItem)) {
        return ModelArmPose.GUN_HOLD;
      }
    }

    // Check offhand for two-handed weapons (rare but possible)
    if (!offHandItem.isEmpty()) {
      if (offHandItem.getItem() instanceof CrossbowItem) {
        return ModelArmPose.CROSSBOW_HOLD;
      } else if (AttackHandler.isBowWeapon(offHandItem)) {
        return ModelArmPose.BOW_AND_ARROW;
      } else if (AttackHandler.isGunWeapon(offHandItem)) {
        return ModelArmPose.GUN_HOLD;
      }
    }

    // For single-handed weapons, check the current arm's item
    ItemStack itemInRightArm = isRightHanded ? mainHandItem : offHandItem;
    ItemStack itemInLeftArm = isRightHanded ? offHandItem : mainHandItem;
    ItemStack currentArmItem = isRightArm ? itemInRightArm : itemInLeftArm;

    if (!currentArmItem.isEmpty() && AttackHandler.isMeleeWeapon(currentArmItem)) {
      return ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
    }

    return ModelArmPose.DEFAULT;
  }
}
