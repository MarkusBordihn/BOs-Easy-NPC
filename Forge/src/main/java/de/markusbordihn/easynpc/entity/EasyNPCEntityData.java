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
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.List;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.AgeableMob;
import net.minecraft.world.entity.EntitySelector;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraft.world.entity.npc.InventoryCarrier;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.item.BowItem;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.ProjectileWeaponItem;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEntityData extends EasyNPCBaseEntity
    implements RangedAttackMob, CrossbowAttackMob, InventoryCarrier {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Synced Data
  private static final EntityDataAccessor<Boolean> DATA_TAME =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  private static final String DATA_INVENTORY_TAG = "Inventory";
  private static final String DATA_POSE_TAG = "Pose";
  private static final String DATA_TAME_TAG = "Tame";
  private static final UniformInt PERSISTENT_ANGER_TIME = TimeUtil.rangeOfSeconds(20, 39);
  private final SimpleContainer inventory = new SimpleContainer(8);
  protected int attackAnimationTick;
  private boolean syncedDataLoaded = false;
  private int remainingPersistentAngerTime;
  private UUID persistentAngerTarget;

  public EasyNPCEntityData(EntityType<? extends EasyNPCEntity> entityType, Level level) {
    super(entityType, level);
  }

  public Pose getPose(String pose) {
    return Pose.valueOf(pose);
  }

  public boolean isTame() {
    return this.entityData.get(DATA_TAME);
  }

  public void setTame(boolean tamed) {
    this.entityData.set(DATA_TAME, tamed);
  }

  public int getAttackAnimationTick() {
    return this.attackAnimationTick;
  }

  public List<? extends Player> getPlayersInRange(Double range) {
    return this.level.players().stream()
        .filter(EntitySelector.NO_SPECTATORS)
        .filter(entity -> this.closerThan(entity, range))
        .toList();
  }

  public boolean synchedDataLoaded() {
    return this.syncedDataLoaded;
  }

  @Override
  public Component getName() {
    Component component = this.getCustomName();
    return component != null ? TextUtils.removeAction(component) : this.getTypeName();
  }

  @Override
  public EasyNPCEntity getEntity() {
    return (EasyNPCEntity) this;
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();

    // Handle pose
    this.entityData.define(DATA_TAME, false);
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);

    // Handle pose
    if (this.getModelPose() == ModelPose.DEFAULT && this.getPose() != null) {
      compoundTag.putString(DATA_POSE_TAG, this.getPose().name());
    } else {
      compoundTag.putString(DATA_POSE_TAG, Pose.STANDING.name());
    }

    // Inventory
    if (!this.inventory.isEmpty()) {
      compoundTag.put(DATA_INVENTORY_TAG, this.inventory.createTag());
    }

    // Handle tame state.
    compoundTag.putBoolean(DATA_TAME_TAG, this.isTame());
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);

    // Handle pose
    if (this.getModelPose() == ModelPose.DEFAULT && compoundTag.contains(DATA_POSE_TAG)) {
      String pose = compoundTag.getString(DATA_POSE_TAG);
      if (pose != null && !pose.isEmpty()) {
        this.setPose(this.getPose(pose));
      }
    }

    // Handle tame state.
    if (compoundTag.contains(DATA_TAME_TAG)) {
      this.setTame(compoundTag.getBoolean(DATA_TAME_TAG));
    }

    // Inventory
    if (compoundTag.contains(DATA_INVENTORY_TAG, 9)) {
      this.inventory.fromTag(compoundTag.getList(DATA_INVENTORY_TAG, 10));
    }

    // Set synced data loaded state.
    this.syncedDataLoaded = true;
  }

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
  public void setChargingCrossbow(boolean isCharging) {
    this.entityData.set(IS_CHARGING_CROSSBOW, isCharging);
  }

  @Override
  public void performRangedAttack(LivingEntity livingEntity, float damage) {
    if (this.getMainHandItem().getItem() instanceof BowItem) {
      this.performBowAttack(this, livingEntity, damage);
    } else if (this.getMainHandItem().getItem() instanceof CrossbowItem) {
      AttackData.addChargedProjectile(this.getMainHandItem(), new ItemStack(Items.ARROW, 1));
      this.performCrossbowAttack(this, 1.6F);
    }
  }

  @Override
  public void shootCrossbowProjectile(
      LivingEntity livingEntity, ItemStack itemStack, Projectile projectile, float rangeFactor) {
    this.shootCrossbowProjectile(this, livingEntity, projectile, rangeFactor, 1.6F);
  }

  @Override
  public void onCrossbowAttackPerformed() {
    this.noActionTime = 0;
  }

  @Override
  public boolean canFireProjectileWeapon(ProjectileWeaponItem projectileWeaponItem) {
    return projectileWeaponItem == Items.CROSSBOW || projectileWeaponItem == Items.BOW;
  }

  @Override
  public SimpleContainer getInventory() {
    return this.inventory;
  }

  protected ItemStack addToInventory(ItemStack itemStack) {
    return this.inventory.addItem(itemStack);
  }

  protected boolean canAddToInventory(ItemStack itemStack) {
    return this.inventory.canAddItem(itemStack);
  }
}
