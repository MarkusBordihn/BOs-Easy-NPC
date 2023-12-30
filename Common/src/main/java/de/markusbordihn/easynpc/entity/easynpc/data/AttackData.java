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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.projectile.AbstractArrow;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.TieredItem;

public interface AttackData<T extends LivingEntity> extends EasyNPC<T> {

  // Synced entity data
  EntityDataAccessor<Boolean> DATA_AGGRESSIVE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);

  EntityDataAccessor<Boolean> IS_CHARGING_CROSSBOW =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);

  // CompoundTags
  String DATA_AGGRESSIVE_TAG = "Aggressive";
  String DATA_CHARGED_PROJECTILES_TAG = "ChargedProjectiles";

  static void addChargedProjectile(ItemStack weaponItemStack, ItemStack projectileItemStack) {
    CompoundTag weaponCompoundTag = weaponItemStack.getOrCreateTag();
    ListTag listTag;
    if (weaponCompoundTag.contains(DATA_CHARGED_PROJECTILES_TAG, 9)) {
      listTag = weaponCompoundTag.getList(DATA_CHARGED_PROJECTILES_TAG, 10);
    } else {
      listTag = new ListTag();
    }
    CompoundTag projectileCompoundTag = new CompoundTag();
    projectileItemStack.save(projectileCompoundTag);
    listTag.add(projectileCompoundTag);
    weaponCompoundTag.put(DATA_CHARGED_PROJECTILES_TAG, listTag);
  }

  default boolean getDefaultAggression() {
    return false;
  }

  default boolean isAggressive() {
    return getEasyNPCData(DATA_AGGRESSIVE);
  }

  default void setAggressive(Boolean aggressive) {
    setEasyNPCData(DATA_AGGRESSIVE, aggressive);
  }

  default boolean isChargingCrossbow() {
    return getEasyNPCData(IS_CHARGING_CROSSBOW);
  }

  default void setChargingCrossbow(boolean isCharging) {
    setEasyNPCData(IS_CHARGING_CROSSBOW, isCharging);
  }

  default void defineSynchedAttackData() {
    defineEasyNPCData(DATA_AGGRESSIVE, getDefaultAggression());
    defineEasyNPCData(IS_CHARGING_CROSSBOW, false);
  }

  default void performBowAttack(
      LivingEntity livingEntity, LivingEntity livingEntityTarget, float damage) {
    ItemStack itemstack =
        livingEntity.getProjectile(
            livingEntity.getItemInHand(
                ProjectileUtil.getWeaponHoldingHand(livingEntity, Items.BOW)));
    AbstractArrow abstractArrow = this.getArrow(livingEntity, itemstack, damage);
    if (livingEntity.getMainHandItem().getItem() instanceof BowItem) {
      double targetX = livingEntityTarget.getX() - livingEntity.getX();
      double targetY = livingEntityTarget.getY(0.3333333333333333D) - abstractArrow.getY();
      double targetZ = livingEntityTarget.getZ() - livingEntity.getZ();
      double targetRadius = Math.sqrt(targetX * targetX + targetZ * targetZ);
      abstractArrow.shoot(
          targetX,
          targetY + targetRadius * 0.2F,
          targetZ,
          1.6F,
          14.0F - livingEntity.level().getDifficulty().getId() * 4);
      livingEntity.playSound(
          SoundEvents.SKELETON_SHOOT,
          1.0F,
          1.0F / (livingEntity.getRandom().nextFloat() * 0.4F + 0.8F));
      livingEntity.level().addFreshEntity(abstractArrow);
    }
  }

  default AbstractArrow getArrow(LivingEntity livingEntity, ItemStack itemStack, float damage) {
    return ProjectileUtil.getMobArrow(livingEntity, itemStack, damage);
  }

  default boolean isHoldingMeleeWeapon(LivingEntity livingEntity) {
    return livingEntity.getMainHandItem().getItem() instanceof TieredItem;
  }

  default void addAdditionalAttackData(CompoundTag compoundTag) {
    compoundTag.putBoolean(DATA_AGGRESSIVE_TAG, this.isAggressive());
  }

  default void readAdditionalAttackData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_AGGRESSIVE_TAG)) {
      this.setAggressive(compoundTag.getBoolean(DATA_AGGRESSIVE_TAG));
    }
  }
}
