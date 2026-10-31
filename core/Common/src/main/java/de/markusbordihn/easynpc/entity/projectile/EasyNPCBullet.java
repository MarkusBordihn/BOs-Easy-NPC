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
import net.minecraft.world.entity.projectile.AbstractArrow;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.EntityHitResult;

public class EasyNPCBullet extends AbstractArrow implements EasyNPCProjectile {

  public static final String ID = "npc_bullet";
  public static final int MAXIMUM_LIFETIME_TICKS = 200;
  public static final float ARROW_GRAVITY = 0.05F;
  public static final int FIRE_ARROW_BURN_SECONDS = 100;
  private static final String PROJECTILE_DATA_TAG = "ProjectileData";
  private static final EntityDataAccessor<Byte> DATA_TRAIL_PARTICLE =
      SynchedEntityData.defineId(EasyNPCBullet.class, EntityDataSerializers.BYTE);
  private static final EntityDataAccessor<Byte> DATA_PROJECTILE_TYPE =
      SynchedEntityData.defineId(EasyNPCBullet.class, EntityDataSerializers.BYTE);

  private ProjectileData projectileData = ProjectileData.DEFAULT;

  public EasyNPCBullet(EntityType<? extends EasyNPCBullet> entityType, Level level) {
    super(entityType, level);
    this.pickup = Pickup.DISALLOWED;
    this.setNoGravity(true);
  }

  public static EasyNPCBullet create(LivingEntity owner, ProjectileData projectileData) {
    EntityType<?> entityType =
        BuiltInRegistries.ENTITY_TYPE.get(ModProjectileEntityType.BULLET.getResourceKey());
    if (!(entityType.create(owner.level()) instanceof EasyNPCBullet bullet)) {
      return null;
    }

    bullet.setPos(owner.getX(), owner.getEyeY() - 0.1F, owner.getZ());
    bullet.setOwner(owner);
    bullet.setProjectileData(projectileData);
    return bullet;
  }

  public static boolean supportsProjectileType(ProjectileType projectileType) {
    return switch (projectileType) {
      case BULLET, ARROW, FIRE_ARROW, SPECTRAL_ARROW, TRIDENT -> true;
      default -> false;
    };
  }

  @Override
  public ProjectileData getProjectileData() {
    return this.projectileData;
  }

  @Override
  public void setProjectileData(ProjectileData projectileData) {
    this.projectileData = projectileData != null ? projectileData : ProjectileData.DEFAULT;
    ProjectileType projectileType = this.projectileData.projectileType();
    this.setBaseDamage(this.projectileData.damage());
    this.setNoGravity(projectileType == ProjectileType.BULLET);
    if (projectileType == ProjectileType.FIRE_ARROW) {
      this.igniteForSeconds(FIRE_ARROW_BURN_SECONDS);
    }
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
    return this.isNoGravity() ? 0.0F : ARROW_GRAVITY;
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
      if (!this.inGround) {
        ProjectileImpactHandler.addTrailParticle(this, this.getTrailParticle());
      }
    } else if (this.tickCount > MAXIMUM_LIFETIME_TICKS) {
      this.discard();
    }
  }

  @Override
  protected void onHitEntity(EntityHitResult entityHitResult) {
    if (this.level().isClientSide()) {
      return;
    }

    ProjectileImpactHandler.hurtTarget(this, entityHitResult.getEntity(), this.projectileData);
    ProjectileImpactHandler.applyImpactEffect(this, this.projectileData);
    this.discard();
  }

  @Override
  protected void onHitBlock(BlockHitResult blockHitResult) {
    super.onHitBlock(blockHitResult);
    if (this.level().isClientSide()) {
      return;
    }

    ProjectileImpactHandler.applyImpactEffect(this, this.projectileData);
    this.discard();
  }

  @Override
  protected ItemStack getPickupItem() {
    return ItemStack.EMPTY;
  }

  @Override
  protected ItemStack getDefaultPickupItem() {
    return new ItemStack(Items.ARROW);
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
