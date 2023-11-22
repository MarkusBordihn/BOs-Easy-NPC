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

package de.markusbordihn.easynpc.entity.data;

import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
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
import net.minecraft.world.item.TieredItem;

public interface EntityAttackData extends EntityDataInterface {

  // Synced entity data
  EntityDataAccessor<Boolean> DATA_AGGRESSIVE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  EntityDataAccessor<Boolean> IS_CHARGING_CROSSBOW =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // CompoundTags
  String DATA_AGGRESSIVE_TAG = "Aggressive";
  String DATA_CHARGED_PROJECTILES_TAG = "ChargedProjectiles";

  static void addChargedProjectile(
      ItemStack weaponItemStack, ItemStack projectileItemStack) {
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
    return getEntityData(DATA_AGGRESSIVE);
  }

  default void setAggressive(Boolean aggressive) {
    setEntityData(DATA_AGGRESSIVE, aggressive);
  }

  default boolean isChargingCrossbow() {
    return getEntityData(IS_CHARGING_CROSSBOW);
  }

  default void setChargingCrossbow(boolean isCharging) {
    setEntityData(IS_CHARGING_CROSSBOW, isCharging);
  }

  default void defineSynchedAttackData() {
    defineEntityData(DATA_AGGRESSIVE, getDefaultAggression());
    defineEntityData(IS_CHARGING_CROSSBOW, false);
  }

  default void performBowAttack(LivingEntity livingEntity, float damage) {
    ItemStack itemstack =
        this.getEntity()
            .getProjectile(
                this.getEntity()
                    .getItemInHand(
                        ProjectileUtil.getWeaponHoldingHand(
                            this.getEntity(), BowItem.class::isInstance)));
    AbstractArrow abstractArrow = this.getEntity().getArrow(itemstack, damage);
    if (this.getEntity().getMainHandItem().getItem() instanceof BowItem bowItem) {
      abstractArrow = bowItem.customArrow(abstractArrow);
      double targetX = livingEntity.getX() - this.getEntity().getX();
      double targetY = livingEntity.getY(0.3333333333333333D) - abstractArrow.getY();
      double targetZ = livingEntity.getZ() - this.getEntity().getZ();
      double targetRadius = Math.sqrt(targetX * targetX + targetZ * targetZ);
      abstractArrow.shoot(
          targetX,
          targetY + targetRadius * 0.2F,
          targetZ,
          1.6F,
          14.0F - this.getEntity().level.getDifficulty().getId() * 4);
      this.getEntity()
          .playSound(
              SoundEvents.SKELETON_SHOOT,
              1.0F,
              1.0F / (this.getEntity().getRandom().nextFloat() * 0.4F + 0.8F));
      this.getEntity().level.addFreshEntity(abstractArrow);
    }
  }

  default AbstractArrow getArrow(ItemStack itemStack, float damage) {
    return ProjectileUtil.getMobArrow(this.getEntity(), itemStack, damage);
  }

  default boolean isHoldingMeleeWeapon() {
    return this.getEntity().getMainHandItem().getItem() instanceof TieredItem;
  }

  default void addAdditionalAttackData(CompoundTag compoundTag) {
    compoundTag.putBoolean(DATA_AGGRESSIVE_TAG, this.isAggressive());

    // Add persistent anger save data
    this.getEntity().addPersistentAngerSaveData(compoundTag);
  }

  default void readAdditionalAttackData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_AGGRESSIVE_TAG)) {
      this.setAggressive(compoundTag.getBoolean(DATA_AGGRESSIVE_TAG));
    }

    // Read persistent anger save data
    this.getEntity().readPersistentAngerSaveData(this.getEntityLevel(), compoundTag);
  }
}
