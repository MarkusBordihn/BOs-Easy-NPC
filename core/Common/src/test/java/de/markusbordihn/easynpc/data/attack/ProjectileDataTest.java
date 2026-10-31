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

import static org.junit.jupiter.api.Assertions.*;

import java.util.Locale;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ProjectileDataTest {

  @Test
  @DisplayName("Should keep all values through a tag round trip")
  void testTagRoundTrip() {
    ProjectileData projectileData =
        new ProjectileData(
            true,
            12.5F,
            4.0F,
            0.0F,
            ProjectileTrailParticle.FLAME,
            ProjectileImpactEffect.EXPLOSION,
            2.0F,
            ProjectileType.ITEM,
            Identifier.fromNamespaceAndPath("minecraft", "diamond"),
            3,
            4,
            ProjectileHitEffect.POISON,
            2,
            30);

    assertEquals(projectileData, ProjectileData.fromTag(projectileData.createTag()));
  }

  @Test
  @DisplayName("Should clamp out of range values")
  void testClampOutOfRange() {
    ProjectileData projectileData =
        new ProjectileData(
            true,
            1000.0F,
            0.0F,
            -5.0F,
            ProjectileTrailParticle.NONE,
            ProjectileImpactEffect.NONE,
            50.0F,
            ProjectileType.BULLET,
            ProjectileData.DEFAULT_PROJECTILE_ITEM,
            99,
            0,
            ProjectileHitEffect.NONE,
            100,
            -1);

    assertEquals(ProjectileData.MAXIMUM_DAMAGE, projectileData.damage());
    assertEquals(ProjectileData.MINIMUM_VELOCITY, projectileData.velocity());
    assertEquals(ProjectileData.MINIMUM_INACCURACY, projectileData.inaccuracy());
    assertEquals(ProjectileData.MAXIMUM_EXPLOSION_POWER, projectileData.explosionPower());
    assertEquals(ProjectileData.MAXIMUM_KNOCKBACK, projectileData.knockback());
    assertEquals(ProjectileData.MINIMUM_SHOT_COUNT, projectileData.shotCount());
    assertEquals(ProjectileData.MAXIMUM_HIT_EFFECT_LEVEL, projectileData.hitEffectLevel());
    assertEquals(ProjectileData.MINIMUM_HIT_EFFECT_SECONDS, projectileData.hitEffectSeconds());
  }

  @Test
  @DisplayName("Should replace non-finite values and missing enums with defaults")
  void testNonFiniteValues() {
    ProjectileData projectileData =
        new ProjectileData(
            true,
            Float.NaN,
            Float.POSITIVE_INFINITY,
            Float.NEGATIVE_INFINITY,
            null,
            null,
            Float.NaN,
            null,
            null,
            ProjectileData.DEFAULT_KNOCKBACK,
            ProjectileData.DEFAULT_SHOT_COUNT,
            null,
            ProjectileData.DEFAULT_HIT_EFFECT_LEVEL,
            ProjectileData.DEFAULT_HIT_EFFECT_SECONDS);

    assertEquals(ProjectileData.DEFAULT_DAMAGE, projectileData.damage());
    assertEquals(ProjectileData.DEFAULT_VELOCITY, projectileData.velocity());
    assertEquals(ProjectileData.DEFAULT_INACCURACY, projectileData.inaccuracy());
    assertEquals(ProjectileData.DEFAULT_EXPLOSION_POWER, projectileData.explosionPower());
    assertEquals(ProjectileTrailParticle.DEFAULT, projectileData.trailParticle());
    assertEquals(ProjectileImpactEffect.DEFAULT, projectileData.impactEffect());
    assertEquals(ProjectileType.DEFAULT, projectileData.projectileType());
    assertEquals(ProjectileData.DEFAULT_PROJECTILE_ITEM, projectileData.projectileItem());
    assertEquals(ProjectileHitEffect.DEFAULT, projectileData.hitEffect());
  }

  @Test
  @DisplayName("Should fall back to defaults for unknown enum names and keep other values")
  void testUnknownEnumNames() {
    CompoundTag compoundTag = ProjectileData.DEFAULT.withDamage(20.0F).createTag();
    compoundTag.putString("TrailParticle", "unknown_particle");
    compoundTag.putString("ImpactEffect", "unknown_effect");
    compoundTag.putString("ProjectileType", "unknown_type");
    compoundTag.putString("HitEffect", "unknown_hit_effect");

    ProjectileData projectileData = ProjectileData.fromTag(compoundTag);

    assertEquals(20.0F, projectileData.damage());
    assertEquals(ProjectileTrailParticle.DEFAULT, projectileData.trailParticle());
    assertEquals(ProjectileImpactEffect.DEFAULT, projectileData.impactEffect());
    assertEquals(ProjectileType.DEFAULT, projectileData.projectileType());
    assertEquals(ProjectileHitEffect.DEFAULT, projectileData.hitEffect());
  }

  @Test
  @DisplayName("Should fall back to the default item for an invalid item id")
  void testInvalidProjectileItem() {
    CompoundTag compoundTag =
        ProjectileData.DEFAULT.withProjectileType(ProjectileType.ITEM).createTag();
    compoundTag.putString("ProjectileItem", "Invalid Item!");

    ProjectileData projectileData = ProjectileData.fromTag(compoundTag);

    assertEquals(ProjectileType.ITEM, projectileData.projectileType());
    assertEquals(ProjectileData.DEFAULT_PROJECTILE_ITEM, projectileData.projectileItem());
  }

  @Test
  @DisplayName("Should parse every projectile type by its serialized name")
  void testProjectileTypeNames() {
    for (ProjectileType projectileType : ProjectileType.values()) {
      assertEquals(
          projectileType,
          ProjectileType.parse(
                  " " + projectileType.getSerializedName().toUpperCase(Locale.ROOT) + " ")
              .orElseThrow());
    }
  }

  @Test
  @DisplayName("Should clamp values read from a tag")
  void testClampFromTag() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putBoolean("Enabled", true);
    compoundTag.putFloat("Damage", 500.0F);
    compoundTag.putFloat("ExplosionPower", 99.0F);
    compoundTag.putInt("ShotCount", 50);

    ProjectileData projectileData = ProjectileData.fromTag(compoundTag);

    assertTrue(projectileData.enabled());
    assertEquals(ProjectileData.MAXIMUM_DAMAGE, projectileData.damage());
    assertEquals(ProjectileData.MAXIMUM_EXPLOSION_POWER, projectileData.explosionPower());
    assertEquals(ProjectileData.MAXIMUM_SHOT_COUNT, projectileData.shotCount());
  }

  @Test
  @DisplayName("Should return the disabled default for missing or empty tags")
  void testMissingTag() {
    assertEquals(ProjectileData.DEFAULT, ProjectileData.fromTag(null));
    assertEquals(ProjectileData.DEFAULT, ProjectileData.fromTag(new CompoundTag()));
    assertFalse(ProjectileData.DEFAULT.enabled());
  }
}
