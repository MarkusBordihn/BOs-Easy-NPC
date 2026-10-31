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

package de.markusbordihn.easynpc.data.attack;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import java.util.Optional;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;

public record ProjectileData(
    boolean enabled,
    float damage,
    float velocity,
    float inaccuracy,
    ProjectileTrailParticle trailParticle,
    ProjectileImpactEffect impactEffect,
    float explosionPower,
    ProjectileType projectileType,
    ResourceLocation projectileItem,
    int knockback,
    int shotCount,
    ProjectileHitEffect hitEffect,
    int hitEffectLevel,
    int hitEffectSeconds) {

  public static final float MINIMUM_DAMAGE = 0.5F;
  public static final float MAXIMUM_DAMAGE = 100.0F;
  public static final float DEFAULT_DAMAGE = 6.0F;
  public static final float MINIMUM_VELOCITY = 0.5F;
  public static final float MAXIMUM_VELOCITY = 5.0F;
  public static final float DEFAULT_VELOCITY = 3.0F;
  public static final float MINIMUM_INACCURACY = 0.0F;
  public static final float MAXIMUM_INACCURACY = 20.0F;
  public static final float DEFAULT_INACCURACY = 2.0F;
  public static final float MINIMUM_EXPLOSION_POWER = 0.0F;
  public static final float MAXIMUM_EXPLOSION_POWER = 6.0F;
  public static final float DEFAULT_EXPLOSION_POWER = 0.0F;
  public static final int MINIMUM_KNOCKBACK = 0;
  public static final int MAXIMUM_KNOCKBACK = 5;
  public static final int DEFAULT_KNOCKBACK = 0;
  public static final int MINIMUM_SHOT_COUNT = 1;
  public static final int MAXIMUM_SHOT_COUNT = 8;
  public static final int DEFAULT_SHOT_COUNT = 1;
  public static final int MINIMUM_HIT_EFFECT_LEVEL = 1;
  public static final int MAXIMUM_HIT_EFFECT_LEVEL = 5;
  public static final int DEFAULT_HIT_EFFECT_LEVEL = 1;
  public static final int MINIMUM_HIT_EFFECT_SECONDS = 1;
  public static final int MAXIMUM_HIT_EFFECT_SECONDS = 60;
  public static final int DEFAULT_HIT_EFFECT_SECONDS = 5;
  public static final ResourceLocation DEFAULT_PROJECTILE_ITEM =
      new ResourceLocation("minecraft", "snowball");

  public static final ProjectileData DEFAULT =
      new ProjectileData(
          false,
          DEFAULT_DAMAGE,
          DEFAULT_VELOCITY,
          DEFAULT_INACCURACY,
          ProjectileTrailParticle.DEFAULT,
          ProjectileImpactEffect.DEFAULT,
          DEFAULT_EXPLOSION_POWER,
          ProjectileType.DEFAULT,
          DEFAULT_PROJECTILE_ITEM,
          DEFAULT_KNOCKBACK,
          DEFAULT_SHOT_COUNT,
          ProjectileHitEffect.DEFAULT,
          DEFAULT_HIT_EFFECT_LEVEL,
          DEFAULT_HIT_EFFECT_SECONDS);

  private static final String ENABLED_TAG = "Enabled";
  private static final String DAMAGE_TAG = "Damage";
  private static final String VELOCITY_TAG = "Velocity";
  private static final String INACCURACY_TAG = "Inaccuracy";
  private static final String TRAIL_PARTICLE_TAG = "TrailParticle";
  private static final String IMPACT_EFFECT_TAG = "ImpactEffect";
  private static final String EXPLOSION_POWER_TAG = "ExplosionPower";
  private static final String PROJECTILE_TYPE_TAG = "ProjectileType";
  private static final String PROJECTILE_ITEM_TAG = "ProjectileItem";
  private static final String KNOCKBACK_TAG = "Knockback";
  private static final String SHOT_COUNT_TAG = "ShotCount";
  private static final String HIT_EFFECT_TAG = "HitEffect";
  private static final String HIT_EFFECT_LEVEL_TAG = "HitEffectLevel";
  private static final String HIT_EFFECT_SECONDS_TAG = "HitEffectSeconds";

  private static final Codec<ResourceLocation> LENIENT_ITEM_CODEC =
      Codec.STRING.xmap(
          itemId ->
              Optional.ofNullable(ResourceLocation.tryParse(itemId))
                  .orElse(DEFAULT_PROJECTILE_ITEM),
          ResourceLocation::toString);

  public static final Codec<ProjectileData> CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      Codec.BOOL
                          .optionalFieldOf(ENABLED_TAG, DEFAULT.enabled())
                          .forGetter(ProjectileData::enabled),
                      Codec.FLOAT
                          .optionalFieldOf(DAMAGE_TAG, DEFAULT_DAMAGE)
                          .forGetter(ProjectileData::damage),
                      Codec.FLOAT
                          .optionalFieldOf(VELOCITY_TAG, DEFAULT_VELOCITY)
                          .forGetter(ProjectileData::velocity),
                      Codec.FLOAT
                          .optionalFieldOf(INACCURACY_TAG, DEFAULT_INACCURACY)
                          .forGetter(ProjectileData::inaccuracy),
                      ProjectileTrailParticle.CODEC
                          .optionalFieldOf(TRAIL_PARTICLE_TAG, ProjectileTrailParticle.DEFAULT)
                          .forGetter(ProjectileData::trailParticle),
                      ProjectileImpactEffect.CODEC
                          .optionalFieldOf(IMPACT_EFFECT_TAG, ProjectileImpactEffect.DEFAULT)
                          .forGetter(ProjectileData::impactEffect),
                      Codec.FLOAT
                          .optionalFieldOf(EXPLOSION_POWER_TAG, DEFAULT_EXPLOSION_POWER)
                          .forGetter(ProjectileData::explosionPower),
                      ProjectileType.CODEC
                          .optionalFieldOf(PROJECTILE_TYPE_TAG, ProjectileType.DEFAULT)
                          .forGetter(ProjectileData::projectileType),
                      LENIENT_ITEM_CODEC
                          .optionalFieldOf(PROJECTILE_ITEM_TAG, DEFAULT_PROJECTILE_ITEM)
                          .forGetter(ProjectileData::projectileItem),
                      Codec.INT
                          .optionalFieldOf(KNOCKBACK_TAG, DEFAULT_KNOCKBACK)
                          .forGetter(ProjectileData::knockback),
                      Codec.INT
                          .optionalFieldOf(SHOT_COUNT_TAG, DEFAULT_SHOT_COUNT)
                          .forGetter(ProjectileData::shotCount),
                      ProjectileHitEffect.CODEC
                          .optionalFieldOf(HIT_EFFECT_TAG, ProjectileHitEffect.DEFAULT)
                          .forGetter(ProjectileData::hitEffect),
                      Codec.INT
                          .optionalFieldOf(HIT_EFFECT_LEVEL_TAG, DEFAULT_HIT_EFFECT_LEVEL)
                          .forGetter(ProjectileData::hitEffectLevel),
                      Codec.INT
                          .optionalFieldOf(HIT_EFFECT_SECONDS_TAG, DEFAULT_HIT_EFFECT_SECONDS)
                          .forGetter(ProjectileData::hitEffectSeconds))
                  .apply(instance, ProjectileData::new));

  public ProjectileData {
    damage = clamp(damage, MINIMUM_DAMAGE, MAXIMUM_DAMAGE, DEFAULT_DAMAGE);
    velocity = clamp(velocity, MINIMUM_VELOCITY, MAXIMUM_VELOCITY, DEFAULT_VELOCITY);
    inaccuracy = clamp(inaccuracy, MINIMUM_INACCURACY, MAXIMUM_INACCURACY, DEFAULT_INACCURACY);
    explosionPower =
        clamp(
            explosionPower,
            MINIMUM_EXPLOSION_POWER,
            MAXIMUM_EXPLOSION_POWER,
            DEFAULT_EXPLOSION_POWER);
    trailParticle = trailParticle != null ? trailParticle : ProjectileTrailParticle.DEFAULT;
    impactEffect = impactEffect != null ? impactEffect : ProjectileImpactEffect.DEFAULT;
    projectileType = projectileType != null ? projectileType : ProjectileType.DEFAULT;
    projectileItem = projectileItem != null ? projectileItem : DEFAULT_PROJECTILE_ITEM;
    knockback = Mth.clamp(knockback, MINIMUM_KNOCKBACK, MAXIMUM_KNOCKBACK);
    shotCount = Mth.clamp(shotCount, MINIMUM_SHOT_COUNT, MAXIMUM_SHOT_COUNT);
    hitEffect = hitEffect != null ? hitEffect : ProjectileHitEffect.DEFAULT;
    hitEffectLevel = Mth.clamp(hitEffectLevel, MINIMUM_HIT_EFFECT_LEVEL, MAXIMUM_HIT_EFFECT_LEVEL);
    hitEffectSeconds =
        Mth.clamp(hitEffectSeconds, MINIMUM_HIT_EFFECT_SECONDS, MAXIMUM_HIT_EFFECT_SECONDS);
  }

  private static float clamp(float value, float minimum, float maximum, float fallback) {
    return Float.isFinite(value) ? Mth.clamp(value, minimum, maximum) : fallback;
  }

  public static ProjectileData fromTag(CompoundTag compoundTag) {
    if (compoundTag == null) {
      return DEFAULT;
    }

    return CODEC.parse(NbtOps.INSTANCE, compoundTag).result().orElse(DEFAULT);
  }

  public ProjectileData withEnabled(boolean enabled) {
    return new ProjectileData(
        enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withDamage(float damage) {
    return new ProjectileData(
        this.enabled,
        damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withVelocity(float velocity) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withInaccuracy(float inaccuracy) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withTrailParticle(ProjectileTrailParticle trailParticle) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withImpactEffect(ProjectileImpactEffect impactEffect) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withExplosionPower(float explosionPower) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withProjectileType(ProjectileType projectileType) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withProjectileItem(ResourceLocation projectileItem) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withKnockback(int knockback) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withShotCount(int shotCount) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withHitEffect(ProjectileHitEffect hitEffect) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        hitEffect,
        this.hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withHitEffectLevel(int hitEffectLevel) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        hitEffectLevel,
        this.hitEffectSeconds);
  }

  public ProjectileData withHitEffectSeconds(int hitEffectSeconds) {
    return new ProjectileData(
        this.enabled,
        this.damage,
        this.velocity,
        this.inaccuracy,
        this.trailParticle,
        this.impactEffect,
        this.explosionPower,
        this.projectileType,
        this.projectileItem,
        this.knockback,
        this.shotCount,
        this.hitEffect,
        this.hitEffectLevel,
        hitEffectSeconds);
  }

  public CompoundTag createTag() {
    return CODEC
        .encodeStart(NbtOps.INSTANCE, this)
        .result()
        .filter(CompoundTag.class::isInstance)
        .map(CompoundTag.class::cast)
        .orElseGet(CompoundTag::new);
  }
}
