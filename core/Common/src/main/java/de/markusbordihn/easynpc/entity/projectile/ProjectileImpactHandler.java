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

import de.markusbordihn.easynpc.config.NPCBaseConfig;
import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.attack.ProjectileHitEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileImpactEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileTrailParticle;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import net.minecraft.core.Holder;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.damagesource.DamageType;
import net.minecraft.world.damagesource.DamageTypes;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class ProjectileImpactHandler {

  public static final int TARGET_FIRE_SECONDS = 5;
  public static final int TARGET_EFFECT_DURATION_TICKS = 200;
  public static final int WITHER_EFFECT_AMPLIFIER = 1;
  private static final double KNOCKBACK_STRENGTH_PER_LEVEL = 0.6;
  private static final double KNOCKBACK_LIFT = 0.1;
  private static final int TICKS_PER_SECOND = 20;

  private ProjectileImpactHandler() {}

  public static void addTrailParticle(
      Projectile projectile, ProjectileTrailParticle trailParticle) {
    ParticleOptions trailParticleOptions = getTrailParticleOptions(trailParticle);
    if (trailParticleOptions != null) {
      projectile
          .level()
          .addParticle(
              trailParticleOptions,
              projectile.getX(),
              projectile.getY(),
              projectile.getZ(),
              0,
              0,
              0);
    }
  }

  private static ParticleOptions getTrailParticleOptions(ProjectileTrailParticle trailParticle) {
    return switch (trailParticle) {
      case NONE -> null;
      case SMOKE -> ParticleTypes.SMOKE;
      case FLAME -> ParticleTypes.FLAME;
      case CRIT -> ParticleTypes.CRIT;
      case SOUL_FIRE_FLAME -> ParticleTypes.SOUL_FIRE_FLAME;
      case END_ROD -> ParticleTypes.END_ROD;
      case ELECTRIC_SPARK -> ParticleTypes.ELECTRIC_SPARK;
    };
  }

  private static ResourceKey<DamageType> getDamageType(ProjectileType projectileType) {
    return switch (projectileType) {
      case BULLET, ARROW, FIRE_ARROW, SPECTRAL_ARROW -> DamageTypes.ARROW;
      case TRIDENT -> DamageTypes.TRIDENT;
      case SNOWBALL, EGG, ITEM -> DamageTypes.THROWN;
      case SMALL_FIREBALL, LARGE_FIREBALL -> DamageTypes.FIREBALL;
      case WITHER_SKULL -> DamageTypes.WITHER_SKULL;
    };
  }

  public static void hurtTarget(
      Projectile projectile, Entity target, ProjectileData projectileData) {
    Entity owner = projectile.getOwner();
    DamageSource damageSource =
        new DamageSource(
            projectile
                .level()
                .registryAccess()
                .registryOrThrow(Registries.DAMAGE_TYPE)
                .getHolderOrThrow(getDamageType(projectileData.projectileType())),
            projectile,
            owner != null ? owner : projectile);
    if (!target.hurt(damageSource, projectileData.damage())
        || !(target instanceof LivingEntity livingTarget)) {
      return;
    }

    switch (projectileData.projectileType()) {
      case FIRE_ARROW, SMALL_FIREBALL, LARGE_FIREBALL ->
          livingTarget.igniteForSeconds(TARGET_FIRE_SECONDS);
      case SPECTRAL_ARROW ->
          livingTarget.addEffect(
              new MobEffectInstance(MobEffects.GLOWING, TARGET_EFFECT_DURATION_TICKS), owner);
      case WITHER_SKULL ->
          livingTarget.addEffect(
              new MobEffectInstance(
                  MobEffects.WITHER, TARGET_EFFECT_DURATION_TICKS, WITHER_EFFECT_AMPLIFIER),
              owner);
      default -> {}
    }
    applyKnockback(projectile, livingTarget, projectileData.knockback());
    applyHitEffect(livingTarget, projectileData, owner);
  }

  private static void applyKnockback(Projectile projectile, LivingEntity target, int knockback) {
    if (knockback <= 0) {
      return;
    }

    double knockbackResistance =
        Math.max(0.0, 1.0 - target.getAttributeValue(Attributes.KNOCKBACK_RESISTANCE));
    Vec3 knockbackMotion =
        projectile
            .getDeltaMovement()
            .multiply(1.0, 0.0, 1.0)
            .normalize()
            .scale(knockback * KNOCKBACK_STRENGTH_PER_LEVEL * knockbackResistance);
    if (knockbackMotion.lengthSqr() > 0.0) {
      target.push(knockbackMotion.x, KNOCKBACK_LIFT, knockbackMotion.z);
    }
  }

  private static void applyHitEffect(
      LivingEntity target, ProjectileData projectileData, Entity owner) {
    Holder<MobEffect> mobEffect = getMobEffect(projectileData.hitEffect());
    if (mobEffect != null) {
      target.addEffect(
          new MobEffectInstance(
              mobEffect,
              projectileData.hitEffectSeconds() * TICKS_PER_SECOND,
              projectileData.hitEffectLevel() - 1),
          owner);
    }
  }

  private static Holder<MobEffect> getMobEffect(ProjectileHitEffect hitEffect) {
    return switch (hitEffect) {
      case NONE -> null;
      case POISON -> MobEffects.POISON;
      case SLOWNESS -> MobEffects.MOVEMENT_SLOWDOWN;
      case WEAKNESS -> MobEffects.WEAKNESS;
      case WITHER -> MobEffects.WITHER;
      case BLINDNESS -> MobEffects.BLINDNESS;
      case NAUSEA -> MobEffects.CONFUSION;
      case HUNGER -> MobEffects.HUNGER;
      case LEVITATION -> MobEffects.LEVITATION;
      case GLOWING -> MobEffects.GLOWING;
      case MINING_FATIGUE -> MobEffects.DIG_SLOWDOWN;
    };
  }

  public static void applyImpactEffect(Projectile projectile, ProjectileData projectileData) {
    if (!(projectile.level() instanceof ServerLevel serverLevel)) {
      return;
    }

    ProjectileImpactEffect impactEffect = projectileData.impactEffect();
    if (impactEffect == ProjectileImpactEffect.PARTICLES) {
      serverLevel.sendParticles(
          ParticleTypes.POOF,
          projectile.getX(),
          projectile.getY(),
          projectile.getZ(),
          8,
          0.1,
          0.1,
          0.1,
          0.02);
    } else if (impactEffect == ProjectileImpactEffect.EXPLOSION) {
      float explosionPower =
          Math.min(
              projectileData.explosionPower(), NPCBaseConfig.PROJECTILE_MAXIMUM_EXPLOSION_POWER);
      if (explosionPower > 0) {
        serverLevel.explode(
            projectile,
            projectile.getX(),
            projectile.getY(),
            projectile.getZ(),
            explosionPower,
            NPCBaseConfig.PROJECTILE_EXPLOSION_BLOCK_DAMAGE
                ? Level.ExplosionInteraction.MOB
                : Level.ExplosionInteraction.NONE);
      }
    }
  }
}
