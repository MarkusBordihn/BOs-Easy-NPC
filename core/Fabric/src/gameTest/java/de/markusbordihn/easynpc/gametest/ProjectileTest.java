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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

@SuppressWarnings("unused")
public class ProjectileTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileDataIsSavedAndLoaded(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileDataIsSavedAndLoaded(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDefaultProjectileDataIsNotSaved(GameTestHelper helper) {
    ProjectileTestHelper.assertDefaultProjectileDataIsNotSaved(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testCustomProjectileAttackSpawnsBullet(GameTestHelper helper) {
    ProjectileTestHelper.assertCustomProjectileAttackSpawnsBullet(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testRangedAttackPlaysConfiguredSound(GameTestHelper helper) {
    ProjectileTestHelper.assertRangedAttackPlaysConfiguredSound(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testRangedAttackPlaysFallbackSoundWithoutConfiguredSound(GameTestHelper helper) {
    ProjectileTestHelper.assertRangedAttackPlaysFallbackSoundWithoutConfiguredSound(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBulletDealsConfiguredDamage(GameTestHelper helper) {
    ProjectileTestHelper.assertBulletDealsConfiguredDamage(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testExplosionDoesNotDestroyBlocks(GameTestHelper helper) {
    ProjectileTestHelper.assertExplosionDoesNotDestroyBlocks(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testEveryProjectileTypeSpawnsMatchingEntity(GameTestHelper helper) {
    ProjectileTestHelper.assertEveryProjectileTypeSpawnsMatchingEntity(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testEveryProjectileTypeDealsConfiguredDamage(GameTestHelper helper) {
    ProjectileTestHelper.assertEveryProjectileTypeDealsConfiguredDamage(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileItemIsResolved(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileItemIsResolved(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testEveryProjectileTypeIsRemovedOnBlockHit(GameTestHelper helper) {
    ProjectileTestHelper.assertEveryProjectileTypeIsRemovedOnBlockHit(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testEveryProjectileTypeIsRemovedAfterMaximumLifetime(GameTestHelper helper) {
    ProjectileTestHelper.assertEveryProjectileTypeIsRemovedAfterMaximumLifetime(
        helper, humanoid());
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testNPCProjectilesCannotBePickedUp(GameTestHelper helper) {
    ProjectileTestHelper.assertNPCProjectilesCannotBePickedUp(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testShotCountFiresEveryProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertShotCountFiresEveryProjectile(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testKnockbackPushesTargetFurther(GameTestHelper helper) {
    ProjectileTestHelper.assertKnockbackPushesTargetFurther(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testHitEffectIsApplied(GameTestHelper helper) {
    ProjectileTestHelper.assertHitEffectIsApplied(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileCommandChangesSettings(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileCommandChangesSettings(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileCommandRejectsInvalidValues(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileCommandRejectsInvalidValues(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileCommandCapsExplosionPower(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileCommandCapsExplosionPower(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBowAttackShootsCustomProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertBowAttackShootsCustomProjectile(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBowAttackKeepsArrowWithoutCustomProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertBowAttackKeepsArrowWithoutCustomProjectile(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testCrossbowAttackShootsCustomProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertCrossbowAttackShootsCustomProjectile(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileAttackNeedsNoWeapon(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileAttackNeedsNoWeapon(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProjectileAttackObjectiveCreatesGoal(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileAttackObjectiveCreatesGoal(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testCrossbowAttackKeepsArrowWithoutCustomProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertCrossbowAttackKeepsArrowWithoutCustomProjectile(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3", timeoutTicks = 400)
  public void testProjectileAttackObjectiveHitsEnemy(GameTestHelper helper) {
    ProjectileTestHelper.assertProjectileAttackObjectiveHitsEnemy(helper, humanoid());
  }

  @GameTest(template = "easy_npc:gametest.3x3x3", timeoutTicks = 400)
  public void testBowAttackObjectiveHitsEnemyWithCustomProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertBowAttackObjectiveHitsEnemyWithCustomProjectile(helper, humanoid());
  }

  @GameTest(template = "easy_npc:gametest.3x3x3", timeoutTicks = 400)
  public void testMeleeAndBowAttackObjectivesShootWhenHoldingBow(GameTestHelper helper) {
    ProjectileTestHelper.assertMeleeAndBowAttackObjectivesShootWhenHoldingBow(helper, humanoid());
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testMeleeAttackYieldsOnlyToMatchingRangedWeapon(GameTestHelper helper) {
    ProjectileTestHelper.assertMeleeAttackYieldsOnlyToMatchingRangedWeapon(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3", timeoutTicks = 400)
  public void testCrossbowAttackObjectiveHitsEnemyWithCustomProjectile(GameTestHelper helper) {
    ProjectileTestHelper.assertCrossbowAttackObjectiveHitsEnemyWithCustomProjectile(
        helper, humanoid());
  }
}
