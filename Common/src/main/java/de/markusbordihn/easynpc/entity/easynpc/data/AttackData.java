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

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.RangedAttackMob;

public interface AttackData<E extends PathfinderMob>
    extends EasyNPC<E>, NeutralMob, RangedAttackMob, CrossbowAttackMob {

  String DATA_AGGRESSIVE_TAG = "Aggressive";

  static void registerSyncedAttackData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Attack Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.ATTACK_IS_AGGRESSIVE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
  }

  int getAttackAnimationTick();

  default boolean getDefaultAggression() {
    return false;
  }

  default boolean isAggressive() {
    return getSynchedEntityData(SynchedDataIndex.ATTACK_IS_AGGRESSIVE);
  }

  default void setAggressive(Boolean aggressive) {
    setSynchedEntityData(SynchedDataIndex.ATTACK_IS_AGGRESSIVE, aggressive);
  }

  default boolean isChargingCrossbow() {
    return getSynchedEntityData(SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW);
  }

  @Override
  default void setChargingCrossbow(boolean isCharging) {
    setSynchedEntityData(SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW, isCharging);
  }

  @Override
  default void performRangedAttack(LivingEntity livingEntity, float damage) {
    AttackHandler.performDefaultRangedAttack(this.getLivingEntity(), livingEntity, damage);
  }

  default void defineSynchedAttackData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.ATTACK_IS_AGGRESSIVE, getDefaultAggression());
    defineSynchedEntityData(builder, SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW, false);
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
