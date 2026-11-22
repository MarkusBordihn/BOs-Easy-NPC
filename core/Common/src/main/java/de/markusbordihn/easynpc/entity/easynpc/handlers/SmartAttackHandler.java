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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.InventoryDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import java.util.List;
import java.util.function.Predicate;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.Identifier;
import net.minecraft.tags.TagKey;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.item.ItemStack;

public class SmartAttackHandler {

  public static final TagKey<EntityType<?>> RANGED_TARGET =
      TagKey.create(
          Registries.ENTITY_TYPE,
          Identifier.fromNamespaceAndPath(Constants.MOD_ID, "smart_attack/ranged_target"));
  public static final int NO_SLOT = -1;

  private SmartAttackHandler() {}

  public static boolean isSmartAttackEnabled(EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    return objectiveData != null && objectiveData.hasObjective(ObjectiveType.SMART_ATTACK);
  }

  public static boolean isAttackTypeAllowed(EasyNPC<?> easyNPC, ObjectiveType attackType) {
    return !isSmartAttackEnabled(easyNPC) || getSelectedAttackType(easyNPC) == attackType;
  }

  private static ObjectiveType getSelectedAttackType(EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    ItemStack mainHandItem = easyNPC.getLivingEntity().getMainHandItem();
    ObjectiveType rangedWeaponAttackType = getRangedWeaponAttackType(objectiveData, mainHandItem);
    if (rangedWeaponAttackType != ObjectiveType.NONE) {
      return rangedWeaponAttackType;
    }

    if (objectiveData.hasObjective(ObjectiveType.PROJECTILE_ATTACK)
        && (prefersRangedAttack(easyNPC.getMob().getTarget())
            || !isUsableMeleeWeapon(objectiveData, mainHandItem))) {
      return ObjectiveType.PROJECTILE_ATTACK;
    }

    return hasMeleeAttack(objectiveData) ? ObjectiveType.MELEE_ATTACK : ObjectiveType.NONE;
  }

  public static boolean prefersRangedAttack(LivingEntity target) {
    return target != null
        && (target.getType().is(RANGED_TARGET)
            || (target instanceof FlyingAnimal && !target.onGround()));
  }

  public static int findBetterWeaponSlot(EasyNPC<?> easyNPC, LivingEntity target) {
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    Predicate<ItemStack> rangedWeapon = itemStack -> isUsableRangedWeapon(objectiveData, itemStack);
    Predicate<ItemStack> meleeWeapon = itemStack -> isUsableMeleeWeapon(objectiveData, itemStack);
    List<Predicate<ItemStack>> weaponPreference;
    if (!prefersRangedAttack(target)) {
      weaponPreference = List.of(meleeWeapon, rangedWeapon);
    } else if (objectiveData.hasObjective(ObjectiveType.PROJECTILE_ATTACK)) {
      weaponPreference = List.of(rangedWeapon);
    } else {
      weaponPreference = List.of(rangedWeapon, meleeWeapon);
    }

    ItemStack mainHandItem = easyNPC.getLivingEntity().getMainHandItem();
    for (Predicate<ItemStack> preferredWeapon : weaponPreference) {
      if (preferredWeapon.test(mainHandItem)) {
        return NO_SLOT;
      }

      int weaponSlot = findNPCOwnedSlot(easyNPC, preferredWeapon);
      if (weaponSlot != NO_SLOT) {
        return weaponSlot;
      }
    }
    return NO_SLOT;
  }

  private static int findNPCOwnedSlot(EasyNPC<?> easyNPC, Predicate<ItemStack> itemPredicate) {
    InventoryDataCapable<?> inventoryData = easyNPC.getEasyNPCInventoryData();
    if (inventoryData == null) {
      return NO_SLOT;
    }

    SimpleContainer inventory = inventoryData.getEasyNPCInventory();
    for (int slot = inventoryData.getInventoryAttributes().playerSlotCount();
        slot < inventory.getContainerSize();
        slot++) {
      if (itemPredicate.test(inventory.getItem(slot))) {
        return slot;
      }
    }
    return NO_SLOT;
  }

  private static ObjectiveType getRangedWeaponAttackType(
      ObjectiveDataCapable<?> objectiveData, ItemStack itemStack) {
    if (objectiveData.hasObjective(ObjectiveType.BOW_ATTACK)
        && AttackHandler.isBowWeapon(itemStack)) {
      return ObjectiveType.BOW_ATTACK;
    } else if (objectiveData.hasObjective(ObjectiveType.CROSSBOW_ATTACK)
        && AttackHandler.isCrossbowWeapon(itemStack)) {
      return ObjectiveType.CROSSBOW_ATTACK;
    } else if (objectiveData.hasObjective(ObjectiveType.GUN_ATTACK)
        && AttackHandler.isGunWeapon(itemStack)) {
      return ObjectiveType.GUN_ATTACK;
    }
    return ObjectiveType.NONE;
  }

  public static boolean isUsableRangedWeapon(
      ObjectiveDataCapable<?> objectiveData, ItemStack itemStack) {
    return getRangedWeaponAttackType(objectiveData, itemStack) != ObjectiveType.NONE;
  }

  private static boolean isUsableMeleeWeapon(
      ObjectiveDataCapable<?> objectiveData, ItemStack itemStack) {
    return hasMeleeAttack(objectiveData) && AttackHandler.isMeleeWeapon(itemStack);
  }

  private static boolean hasMeleeAttack(ObjectiveDataCapable<?> objectiveData) {
    return objectiveData.hasObjective(ObjectiveType.MELEE_ATTACK)
        || objectiveData.hasObjective(ObjectiveType.ZOMBIE_ATTACK);
  }
}
