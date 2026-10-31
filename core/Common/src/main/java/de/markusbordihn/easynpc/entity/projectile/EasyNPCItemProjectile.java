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

package de.markusbordihn.easynpc.entity.projectile;

import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.attack.ProjectileTrailParticle;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.entity.ModProjectileEntityType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.projectile.ThrowableItemProjectile;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.EntityHitResult;
import net.minecraft.world.phys.HitResult;

public class EasyNPCItemProjectile extends ThrowableItemProjectile implements EasyNPCProjectile {

  public static final String ID = "npc_item_projectile";
  public static final int MAXIMUM_LIFETIME_TICKS = 200;
  private static final String PROJECTILE_DATA_TAG = "ProjectileData";
  private static final EntityDataAccessor<Byte> DATA_TRAIL_PARTICLE =
      SynchedEntityData.defineId(EasyNPCItemProjectile.class, EntityDataSerializers.BYTE);
  private static final EntityDataAccessor<Byte> DATA_PROJECTILE_TYPE =
      SynchedEntityData.defineId(EasyNPCItemProjectile.class, EntityDataSerializers.BYTE);

  private ProjectileData projectileData = ProjectileData.DEFAULT;

  public EasyNPCItemProjectile(
      EntityType<? extends EasyNPCItemProjectile> entityType, Level level) {
    super(entityType, level);
  }

  public static EasyNPCItemProjectile create(LivingEntity owner, ProjectileData projectileData) {
    EntityType<?> entityType =
        BuiltInRegistries.ENTITY_TYPE.get(ModProjectileEntityType.ITEM_PROJECTILE.getResourceKey());
    if (!(entityType.create(owner.level()) instanceof EasyNPCItemProjectile itemProjectile)) {
      return null;
    }

    itemProjectile.setPos(owner.getX(), owner.getEyeY() - 0.1F, owner.getZ());
    itemProjectile.setOwner(owner);
    itemProjectile.setProjectileData(projectileData);
    return itemProjectile;
  }

  private static Item getProjectileItem(ProjectileData projectileData) {
    Item projectileItem =
        switch (projectileData.projectileType()) {
          case EGG -> Items.EGG;
          case SMALL_FIREBALL, LARGE_FIREBALL -> Items.FIRE_CHARGE;
          case WITHER_SKULL -> Items.WITHER_SKELETON_SKULL;
          case ITEM -> BuiltInRegistries.ITEM.get(projectileData.projectileItem());
          default -> Items.SNOWBALL;
        };
    return projectileItem == Items.AIR ? Items.SNOWBALL : projectileItem;
  }

  private static boolean hasNoGravity(ProjectileType projectileType) {
    return projectileType == ProjectileType.SMALL_FIREBALL
        || projectileType == ProjectileType.LARGE_FIREBALL
        || projectileType == ProjectileType.WITHER_SKULL;
  }

  @Override
  public ProjectileData getProjectileData() {
    return this.projectileData;
  }

  @Override
  public void setProjectileData(ProjectileData projectileData) {
    this.projectileData = projectileData != null ? projectileData : ProjectileData.DEFAULT;
    ProjectileType projectileType = this.projectileData.projectileType();
    this.setItem(new ItemStack(getProjectileItem(this.projectileData)));
    this.setNoGravity(hasNoGravity(projectileType));
    this.entityData.set(
        DATA_TRAIL_PARTICLE, (byte) this.projectileData.trailParticle().ordinal());
    this.entityData.set(DATA_PROJECTILE_TYPE, (byte) projectileType.ordinal());
  }

  @Override
  public ProjectileTrailParticle getTrailParticle() {
    return ProjectileTrailParticle.byOrdinal(this.entityData.get(DATA_TRAIL_PARTICLE));
  }

  @Override
  public ProjectileType getProjectileType() {
    return ProjectileType.byOrdinal(this.entityData.get(DATA_PROJECTILE_TYPE));
  }

  @Override
  public float getGravityPerTick() {
    return this.isNoGravity() ? 0.0F : (float) this.getGravity();
  }

  @Override
  protected Item getDefaultItem() {
    return Items.SNOWBALL;
  }

  @Override
  protected void defineSynchedData(SynchedEntityData.Builder builder) {
    super.defineSynchedData(builder);
    builder.define(DATA_TRAIL_PARTICLE, (byte) ProjectileTrailParticle.DEFAULT.ordinal());
    builder.define(DATA_PROJECTILE_TYPE, (byte) ProjectileType.DEFAULT.ordinal());
  }

  @Override
  public void tick() {
    super.tick();
    if (this.level().isClientSide()) {
      ProjectileImpactHandler.addTrailParticle(this, this.getTrailParticle());
    } else if (this.tickCount > MAXIMUM_LIFETIME_TICKS) {
      this.discard();
    }
  }

  @Override
  protected void onHitEntity(EntityHitResult entityHitResult) {
    super.onHitEntity(entityHitResult);
    if (this.level().isClientSide()) {
      return;
    }

    ProjectileImpactHandler.hurtTarget(this, entityHitResult.getEntity(), this.projectileData);
  }

  @Override
  protected void onHit(HitResult hitResult) {
    super.onHit(hitResult);
    if (this.level().isClientSide() || this.isRemoved()) {
      return;
    }

    ProjectileImpactHandler.applyImpactEffect(this, this.projectileData);
    this.discard();
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    compoundTag.put(PROJECTILE_DATA_TAG, this.projectileData.createTag());
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    if (compoundTag.contains(PROJECTILE_DATA_TAG)) {
      this.setProjectileData(ProjectileData.fromTag(compoundTag.getCompound(PROJECTILE_DATA_TAG)));
    }
  }
}
