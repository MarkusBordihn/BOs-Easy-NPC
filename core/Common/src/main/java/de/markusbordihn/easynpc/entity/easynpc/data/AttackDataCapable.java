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

import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface AttackDataCapable<E extends Mob>
    extends EasyNPC<E>, NeutralMob, RangedAttackMob, CrossbowAttackMob {

  ServerDataAccessor<ProjectileData> CUSTOM_DATA_PROJECTILE_DATA =
      ServerEntityData.defineId(
          ServerDataIndex.PROJECTILE_DATA, EntityDataSerializersManager.PROJECTILE_DATA);

  String DATA_PROJECTILE_DATA_TAG = "ProjectileData";

  int getAttackAnimationTick();

  default ProjectileData getProjectileData() {
    ServerDataCapable<E> serverData = this.getEasyNPCServerData();
    if (this.isClientSideInstance() || serverData == null || !serverData.hasServerEntityData()) {
      return ProjectileData.DEFAULT;
    }

    ProjectileData projectileData = serverData.getServerEntityData(CUSTOM_DATA_PROJECTILE_DATA);
    return projectileData != null ? projectileData : ProjectileData.DEFAULT;
  }

  default void setProjectileData(ProjectileData projectileData) {
    ProjectileData updatedProjectileData =
        projectileData != null ? projectileData : ProjectileData.DEFAULT;
    if (updatedProjectileData.equals(this.getProjectileData())) {
      return;
    }

    this.getEasyNPCServerData()
        .setServerEntityData(CUSTOM_DATA_PROJECTILE_DATA, updatedProjectileData);
  }

  default void defineCustomAttackData() {
    this.getEasyNPCServerData()
        .defineServerEntityData(CUSTOM_DATA_PROJECTILE_DATA, ProjectileData.DEFAULT);
  }

  default boolean isChargingCrossbow() {
    return this.getSynchedEntityData(SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW);
  }

  @Override
  default void setChargingCrossbow(boolean isCharging) {
    this.setSynchedEntityData(SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW, isCharging);
  }

  @Override
  default void performRangedAttack(LivingEntity livingEntity, float damage) {
    AttackHandler.performDefaultRangedAttack(this.getLivingEntity(), livingEntity, damage);
  }

  default void defineSynchedAttackData(SynchedEntityData.Builder builder) {
    this.defineSynchedEntityData(builder, SynchedDataIndex.ATTACK_IS_CHARGING_CROSSBOW, false);
  }

  default void addAdditionalAttackData(ValueOutput valueOutput) {
    ProjectileData projectileData = this.getProjectileData();
    if (!projectileData.equals(ProjectileData.DEFAULT)) {
      valueOutput.store(DATA_PROJECTILE_DATA_TAG, CompoundTag.CODEC, projectileData.createTag());
    }
  }

  default void readAdditionalAttackData(ValueInput valueInput) {
    valueInput
        .read(DATA_PROJECTILE_DATA_TAG, CompoundTag.CODEC)
        .ifPresent(
            projectileDataTag -> this.setProjectileData(ProjectileData.fromTag(projectileDataTag)));
  }
}
