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
    return livingEntity.isUsingItem()
        ? getArmPoseWhileUsingItem(livingEntity, isRightArm, isRightHanded)
        : getIdleArmPose(easyNPC, livingEntity, isRightArm, isRightHanded);
  }

  private static ModelArmPose getArmPoseWhileUsingItem(
      final LivingEntity livingEntity, final boolean isRightArm, final boolean isRightHanded) {
    ItemStack useItem = livingEntity.getUseItem();
    if (useItem.isEmpty()) {
      return ModelArmPose.DEFAULT;
    }

    boolean isUsingMainHand = livingEntity.getUsedItemHand() == InteractionHand.MAIN_HAND;
    boolean isUsingRightHand =
        (isRightHanded && isUsingMainHand) || (!isRightHanded && !isUsingMainHand);
    if (isRightArm != isUsingRightHand) {
      return ModelArmPose.DEFAULT;
    }

    return switch (useItem.getUseAnimation()) {
      case BOW -> ModelArmPose.BOW_AND_ARROW;
      case CROSSBOW -> ModelArmPose.CROSSBOW_CHARGE;
      case SPYGLASS -> ModelArmPose.SPYGLASS;
      case SPEAR -> ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
      default -> AttackHandler.isGunWeapon(useItem) ? ModelArmPose.GUN_HOLD : ModelArmPose.DEFAULT;
    };
  }

  private static ModelArmPose getIdleArmPose(
      final EasyNPC<?> easyNPC,
      final LivingEntity livingEntity,
      final boolean isRightArm,
      final boolean isRightHanded) {
    if (easyNPC.getPathfinderMob() == null) {
      return ModelArmPose.DEFAULT;
    }

    boolean isAggressive =
        easyNPC.getPathfinderMob().getTarget() != null
            || (livingEntity instanceof Mob mob && mob.isAggressive());
    if (!isAggressive) {
      return ModelArmPose.DEFAULT;
    }

    ItemStack mainHandItem = livingEntity.getMainHandItem();
    ItemStack offHandItem = livingEntity.getOffhandItem();
    ItemStack itemInRightArm = isRightHanded ? mainHandItem : offHandItem;
    ItemStack itemInLeftArm = isRightHanded ? offHandItem : mainHandItem;
    ItemStack currentArmItem = isRightArm ? itemInRightArm : itemInLeftArm;
    if (currentArmItem.isEmpty()) {
      return ModelArmPose.DEFAULT;
    }

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
