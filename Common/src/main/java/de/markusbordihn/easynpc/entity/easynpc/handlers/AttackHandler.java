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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.item.ModItemTags;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.projectile.AbstractArrow;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.item.TieredItem;

public class AttackHandler {

  public static final String CHARGED_PROJECTILES_TAG = "ChargedProjectiles";

  private AttackHandler() {
  }

  public static void addChargedProjectile(
      ItemStack weaponItemStack, ItemStack projectileItemStack) {
    CompoundTag weaponCompoundTag = weaponItemStack.getOrCreateTag();
    ListTag listTag;
    if (weaponCompoundTag.contains(CHARGED_PROJECTILES_TAG, 9)) {
      listTag = weaponCompoundTag.getList(CHARGED_PROJECTILES_TAG, 10);
    } else {
      listTag = new ListTag();
    }
    CompoundTag projectileCompoundTag = new CompoundTag();
    projectileItemStack.save(projectileCompoundTag);
    listTag.add(projectileCompoundTag);
    weaponCompoundTag.put(CHARGED_PROJECTILES_TAG, listTag);
  }

  public static boolean isMeeleeWeapon(ItemStack itemStack) {
    return itemStack.getItem() instanceof TieredItem;
  }

  public static boolean isBowWeapon(ItemStack itemStack) {
    return itemStack.getItem() instanceof BowItem || itemStack.is(ModItemTags.RANGED_WEAPON_BOW);
  }

  public static boolean isCrossbowWeapon(ItemStack itemStack) {
    return itemStack.getItem() instanceof CrossbowItem
        || itemStack.is(ModItemTags.RANGED_WEAPON_CROSSBOW);
  }

  public static boolean isGunWeapon(ItemStack itemStack) {
    return itemStack.is(ModItemTags.RANGED_WEAPON_GUN);
  }

  public static boolean canFireProjectileWeapon(ProjectileWeaponItem projectileWeaponItem) {
    return projectileWeaponItem instanceof CrossbowItem || projectileWeaponItem instanceof BowItem;
  }

  public static boolean isHoldingBowWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isBowWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingCrossbowWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isCrossbowWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingGunWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isGunWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingMeleeWeapon(LivingEntity livingEntity) {
    return livingEntity != null && isMeeleeWeapon(livingEntity.getMainHandItem());
  }

  public static boolean isHoldingProjectileWeapon(LivingEntity livingEntity) {
    return livingEntity != null
        && livingEntity.getMainHandItem().getItem() instanceof ProjectileWeaponItem;
  }

  public static boolean isHoldingWeapon(LivingEntity livingEntity) {
    return isHoldingMeleeWeapon(livingEntity) || isHoldingProjectileWeapon(livingEntity);
  }

  public static void performDefaultRangedAttack(
      LivingEntity livingEntity, LivingEntity targedtedLivingEntity, float damage) {
    if (isHoldingBowWeapon(livingEntity)) {
      performBowAttack(livingEntity, targedtedLivingEntity, damage);
    } else if (livingEntity instanceof CrossbowAttackMob crossbowAttackMob
        && isHoldingCrossbowWeapon(livingEntity)) {
      addChargedProjectile(livingEntity.getMainHandItem(), new ItemStack(Items.ARROW, 1));
      crossbowAttackMob.performCrossbowAttack(livingEntity, 1.6F);
    } else if (isHoldingGunWeapon(livingEntity)) {
      performGunAttack(livingEntity, targedtedLivingEntity, damage);
    }
  }

  public static InteractionHand getBowHoldingHand(LivingEntity livingEntity) {
    ItemStack itemStack = livingEntity.getMainHandItem();
    return isBowWeapon(itemStack) ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND;
  }

  public static InteractionHand getCrossbowHoldingHand(LivingEntity livingEntity) {
    ItemStack itemStack = livingEntity.getMainHandItem();
    return isCrossbowWeapon(itemStack) ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND;
  }

  public static InteractionHand getGunHoldingHand(LivingEntity livingEntity) {
    ItemStack itemStack = livingEntity.getMainHandItem();
    return isGunWeapon(itemStack) ? InteractionHand.MAIN_HAND : InteractionHand.OFF_HAND;
  }

  public static void performGunAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget, float damage) {
    AbstractArrow abstractArrow = getBullet(livingEntity, damage);
    if (isGunWeapon(livingEntity.getMainHandItem())) {
      double targetX = livingEntityTarget.getX() - livingEntity.getX();
      double targetY = livingEntityTarget.getY() - abstractArrow.getY();
      double targetZ = livingEntityTarget.getZ() - livingEntity.getZ();
      double targetRadius = Math.sqrt(targetX * targetX + targetZ * targetZ);
      abstractArrow.shoot(
          targetX,
          targetY + targetRadius * 0.2F,
          targetZ,
          1.6F,
          14.0F - livingEntity.level.getDifficulty().getId() * 4);
      livingEntity.playSound(
          SoundEvents.FIRECHARGE_USE,
          1.0F,
          1.0F / (livingEntity.getRandom().nextFloat() * 0.4F + 0.8F));
      livingEntity.level.addFreshEntity(abstractArrow);
    }
  }

  public static void performBowAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget, float damage) {
    ItemStack itemStack =
        livingEntity.getProjectile(livingEntity.getItemInHand(getBowHoldingHand(livingEntity)));
    AbstractArrow abstractArrow = getArrow(livingEntity, itemStack, damage);
    if (isBowWeapon(livingEntity.getMainHandItem())) {
      double targetX = livingEntityTarget.getX() - livingEntity.getX();
      double targetY = livingEntityTarget.getY(0.3333333333333333D) - abstractArrow.getY();
      double targetZ = livingEntityTarget.getZ() - livingEntity.getZ();
      double targetRadius = Math.sqrt(targetX * targetX + targetZ * targetZ);
      abstractArrow.shoot(
          targetX,
          targetY + targetRadius * 0.2F,
          targetZ,
          1.6F,
          14.0F - livingEntity.level.getDifficulty().getId() * 4);
      livingEntity.playSound(
          SoundEvents.SKELETON_SHOOT,
          1.0F,
          1.0F / (livingEntity.getRandom().nextFloat() * 0.4F + 0.8F));
      livingEntity.level.addFreshEntity(abstractArrow);
    }
  }

  public static AbstractArrow getArrow(
      LivingEntity livingEntity, ItemStack itemStack, float damage) {
    return ProjectileUtil.getMobArrow(livingEntity, itemStack, damage);
  }

  public static AbstractArrow getBullet(LivingEntity livingEntity, float damage) {
    Item item = Registry.ITEM.get(new ResourceLocation(Constants.MOD_ID, "bullet"));
    return ProjectileUtil.getMobArrow(livingEntity, new ItemStack(item), damage);
  }
}
