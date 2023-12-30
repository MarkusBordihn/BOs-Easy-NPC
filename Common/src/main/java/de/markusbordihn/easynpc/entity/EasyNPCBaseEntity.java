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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.data.entity.CustomEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import java.util.UUID;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCBaseEntity extends AgeableMob
    implements NeutralMob,
        EasyNPC<AgeableMob>,
        AttackData<AgeableMob>,
        AttributeData<AgeableMob>,
        ModelData<AgeableMob>,
        ScaleData<AgeableMob>,
        SkinData<AgeableMob>,
        VariantData<AgeableMob> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final UniformInt PERSISTENT_ANGER_TIME = TimeUtil.rangeOfSeconds(20, 39);

  static {
    EntityDataSerializers.registerSerializer(SkinData.SKIN_TYPE);
    EntityDataSerializers.registerSerializer(ModelData.MODEL_POSE);
    EntityDataSerializers.registerSerializer(ModelData.POSITION);
    EntityDataSerializers.registerSerializer(ModelData.SCALE);
  }

  private final CustomEntityData customEntityData = new CustomEntityData(this);
  private boolean attributeDataLoaded = false;
  private int remainingPersistentAngerTime;
  private UUID persistentAngerTarget;

  public EasyNPCBaseEntity(EntityType<? extends AgeableMob> entityType, Level world) {
    super(entityType, world);
  }

  @Override
  public boolean canFireProjectileWeapon(ProjectileWeaponItem projectileWeaponItem) {
    return projectileWeaponItem == Items.CROSSBOW || projectileWeaponItem == Items.BOW;
  }

  @Nullable
  @Override
  public AgeableMob getBreedOffspring(ServerLevel serverLevel, AgeableMob ageableMob) {
    return null;
  }

  @Override
  public int getRemainingPersistentAngerTime() {
    return this.remainingPersistentAngerTime;
  }

  @Override
  public void setRemainingPersistentAngerTime(int remainingPersistentAngerTime) {
    this.remainingPersistentAngerTime = remainingPersistentAngerTime;
  }

  @Nullable
  @Override
  public UUID getPersistentAngerTarget() {
    return this.persistentAngerTarget;
  }

  @Override
  public void setPersistentAngerTarget(UUID targetUUID) {
    this.persistentAngerTarget = targetUUID;
  }

  @Override
  public void startPersistentAngerTimer() {
    this.setRemainingPersistentAngerTime(PERSISTENT_ANGER_TIME.sample(this.random));
  }

  @Override
  public Level getEasyNPCLevel() {
    return this.level;
  }

  @Override
  public ServerLevel getEasyNPCServerLevel() {
    if (this.level instanceof ServerLevel serverLevel) {
      return serverLevel;
    }
    return null;
  }

  @Override
  public EasyNPC<AgeableMob> getEasyNPC() {
    return this;
  }

  @Override
  public AgeableMob getEasyNPCEntity() {
    return this;
  }

  @Override
  public GoalSelector getEasyNPCGoalSelector() {
    return this.goalSelector;
  }

  @Override
  public GoalSelector getEasyNPCTargetSelector() {
    return this.targetSelector;
  }

  @Override
  public GroundPathNavigation getEasyNPCGroundPathNavigation() {
    if (this.getNavigation() instanceof GroundPathNavigation groundPathNavigation) {
      return groundPathNavigation;
    }
    return null;
  }

  @Override
  public <T> void setEasyNPCData(EntityDataAccessor<T> entityDataAccessor, T entityData) {
    this.entityData.set(entityDataAccessor, entityData);
  }

  @Override
  public <T> T getEasyNPCData(EntityDataAccessor<T> entityDataAccessor) {
    return this.entityData.get(entityDataAccessor);
  }

  @Override
  public <T> void defineEasyNPCData(EntityDataAccessor<T> entityDataAccessor, T entityData) {
    this.entityData.define(entityDataAccessor, entityData);
  }

  @Override
  public <T> void setEasyNPCCustomData(CustomDataAccessor<T> entityDataAccessor, T entityData) {
    this.customEntityData.set(entityDataAccessor, entityData);
  }

  @Override
  public <T> T getEasyNPCCustomData(CustomDataAccessor<T> entityDataAccessor) {
    return this.customEntityData.get(entityDataAccessor);
  }

  @Override
  public <T> void defineEasyNPCCustomData(CustomDataAccessor<T> entityDataAccessor, T entityData) {
    this.customEntityData.define(entityDataAccessor, entityData);
  }

  public boolean attributeDataLoaded() {
    return this.attributeDataLoaded;
  }

  @Override
  public boolean isAttackable() {
    return this.attributeDataLoaded && getAttributeIsAttackable();
  }

  @Override
  public boolean isPushable() {
    return this.attributeDataLoaded() && getAttributeIsPushable();
  }

  @Override
  protected void handleNetherPortal() {
    if (this.attributeDataLoaded && getAttributeCanUseNetherPortal()) {
      super.handleNetherPortal();
    }
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();

    this.defineSynchedAttackData();
    this.defineSynchedAttributeData();
    this.defineSynchedModelData();
    this.defineSynchedScaleData();
    this.defineSynchedSkinData();
    this.defineSynchedVariantData();
  }

  @Override
  public void addAdditionalSaveData(@Nonnull CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);

    this.addAdditionalAttackData(compoundTag);
    this.addAdditionalAttributeData(compoundTag);
    this.addAdditionalModelData(compoundTag);
    this.addAdditionalScaleData(compoundTag);
    this.addAdditionalSkinData(compoundTag);
    this.addAdditionalVariantData(compoundTag);

    this.addPersistentAngerSaveData(compoundTag);
  }

  @Override
  public void readAdditionalSaveData(@Nonnull CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);

    this.readAdditionalAttackData(compoundTag);
    this.readAdditionalAttributeData(compoundTag);
    this.readAdditionalModelData(compoundTag);
    this.readAdditionalScaleData(compoundTag);
    this.readAdditionalSkinData(compoundTag);
    this.readAdditionalVariantData(compoundTag);

    this.readPersistentAngerSaveData(this.level, compoundTag);

    this.attributeDataLoaded = true;
  }
}
