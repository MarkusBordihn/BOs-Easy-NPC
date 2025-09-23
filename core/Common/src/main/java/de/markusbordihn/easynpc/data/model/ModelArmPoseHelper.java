package de.markusbordihn.easynpc.data.model;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ArmPoseProvider;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import net.minecraft.client.model.HumanoidModel.ArmPose;
import net.minecraft.world.entity.HumanoidArm;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public class ModelArmPoseHelper {

  private ModelArmPoseHelper() {}

  public static ModelArmPose getArmPoseForNPC(EasyNPC<?> easyNPC, HumanoidArm humanoidArm) {
    if (easyNPC == null || humanoidArm == null) {
      return ModelArmPose.DEFAULT;
    }

    // Get the item in the specified hand
    ItemStack itemStack = getItemStackForArm(easyNPC, humanoidArm);
    if (itemStack.isEmpty()) {
      return ModelArmPose.NEUTRAL;
    }

    // Handle bow weapons
    boolean isAggressive = easyNPC instanceof Mob mob && mob.isAggressive();
    if (isAggressive && AttackHandler.isBowWeapon(itemStack)) {
      return ModelArmPose.BOW_AND_ARROW;
    }

    // Handle crossbow weapons
    if (AttackHandler.isCrossbowWeapon(itemStack)) {
      AttackDataCapable<?> attackData = easyNPC.getEasyNPCAttackData();
      if (attackData != null && attackData.isChargingCrossbow()) {
        return ModelArmPose.CROSSBOW_CHARGE;
      } else if (isAggressive) {
        return ModelArmPose.CROSSBOW_HOLD;
      }
    }

    // Handle spyglass
    if (itemStack.is(Items.SPYGLASS)) {
      return ModelArmPose.SPYGLASS;
    }

    // Handle melee weapons when aggressive - use correct method name
    if (isAggressive && AttackHandler.isMeeleeWeapon(itemStack)) {
      return ModelArmPose.ATTACKING_WITH_MELEE_WEAPON;
    }

    // Default to neutral if holding an item but no specific pose applies
    return ModelArmPose.NEUTRAL;
  }

  private static ItemStack getItemStackForArm(EasyNPC<?> easyNPC, HumanoidArm humanoidArm) {
    if (!(easyNPC instanceof Mob mob)) {
      return ItemStack.EMPTY;
    }

    // Determine main arm (assuming right-handed by default)
    HumanoidArm mainArm = HumanoidArm.RIGHT;
    if (easyNPC instanceof ArmPoseProvider armPoseProvider) {
      mainArm = armPoseProvider.getMainArm();
    }

    // Return the appropriate item based on which arm is requested
    if (humanoidArm == mainArm) {
      return mob.getMainHandItem();
    } else {
      return mob.getOffhandItem();
    }
  }

  public static ArmPose toMinecraftArmPose(ModelArmPose modelArmPose) {
    return switch (modelArmPose) {
      case BOW_AND_ARROW -> net.minecraft.client.model.HumanoidModel.ArmPose.BOW_AND_ARROW;
      case CROSSBOW_CHARGE -> net.minecraft.client.model.HumanoidModel.ArmPose.CROSSBOW_CHARGE;
      case CROSSBOW_HOLD -> net.minecraft.client.model.HumanoidModel.ArmPose.CROSSBOW_HOLD;
      case SPYGLASS -> net.minecraft.client.model.HumanoidModel.ArmPose.SPYGLASS;
      case ATTACKING, ATTACKING_WITH_MELEE_WEAPON ->
          net.minecraft.client.model.HumanoidModel.ArmPose.ITEM;
      case SPELLCASTING -> net.minecraft.client.model.HumanoidModel.ArmPose.BLOCK;
      case CELEBRATING, CROSSED, DANCING -> net.minecraft.client.model.HumanoidModel.ArmPose.EMPTY;
      default -> net.minecraft.client.model.HumanoidModel.ArmPose.ITEM;
    };
  }
}
