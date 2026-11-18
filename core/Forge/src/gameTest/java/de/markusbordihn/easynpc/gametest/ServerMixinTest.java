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
public class ServerMixinTest {

  private static EntityType<?> entityType(ModNPCEntityType npcEntityType) {
    return ModEntityType.getEntityType(npcEntityType);
  }

  @GameTest(template = "gametest.3x3x3")
  public void testCreeperNPCDoesNotExplode(GameTestHelper helper) {
    ServerMixinTestHelper.assertCreeperNPCDoesNotExplode(
        helper, entityType(ModNPCEntityType.CREEPER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPiglinNPCSkipsVanillaActivityUpdate(GameTestHelper helper) {
    ServerMixinTestHelper.assertPiglinNPCSkipsVanillaActivityUpdate(
        helper, entityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPiglinBruteNPCSkipsVanillaActivityUpdate(GameTestHelper helper) {
    ServerMixinTestHelper.assertPiglinNPCSkipsVanillaActivityUpdate(
        helper, entityType(ModNPCEntityType.PIGLIN_BRUTE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testFoxNPCGetsNoVanillaTargetGoals(GameTestHelper helper) {
    ServerMixinTestHelper.assertFoxNPCGetsNoVanillaTargetGoals(
        helper, entityType(ModNPCEntityType.FOX));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testWitchNPCDrinksNoPotions(GameTestHelper helper) {
    ServerMixinTestHelper.assertWitchNPCDrinksNoPotions(helper, entityType(ModNPCEntityType.WITCH));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testVillagerNPCKeepsItsTradingPlayer(GameTestHelper helper) {
    ServerMixinTestHelper.assertVillagerNPCKeepsItsTradingPlayer(
        helper, entityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSlimeNPCKeepsItsSizeOnFinalizeSpawn(GameTestHelper helper) {
    ServerMixinTestHelper.assertSlimeNPCKeepsItsSizeOnFinalizeSpawn(
        helper, entityType(ModNPCEntityType.SLIME));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testWalkAnimationCopyTakesEveryValue(GameTestHelper helper) {
    ServerMixinTestHelper.assertWalkAnimationCopyTakesEveryValue(helper);
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSpawnerDataMatchesEverySpawnerType(GameTestHelper helper) {
    ServerMixinTestHelper.assertSpawnerDataMatchesEverySpawnerType(helper);
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testZombieNPCIsNotReplacedByVanillaConversion(GameTestHelper helper) {
    ServerMixinTestHelper.assertNPCIsNotReplacedByVanillaConversion(
        helper, entityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSkeletonNPCIsNotReplacedByVanillaConversion(GameTestHelper helper) {
    ServerMixinTestHelper.assertNPCIsNotReplacedByVanillaConversion(
        helper, entityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testZombieNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testHuskNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.ZOMBIE_HUSK));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDrownedNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.DROWNED));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testZombieVillagerNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.ZOMBIE_VILLAGER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testZombifiedPiglinNPCKeepsItsWeaponOnFinalizeSpawn(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombifiedPiglinNPCKeepsItsWeaponOnFinalizeSpawn(
        helper, entityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSkeletonNPCGetsTheVanillaBowOnlyWithoutEquipment(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsTheVanillaBowOnlyWithoutEquipment(
        helper, entityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSkeletonNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testStrayNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.STRAY));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testWitherSkeletonNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.WITHER_SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testVillagerNPCPicksUpLootOnlyIntoItsInventory(GameTestHelper helper) {
    ServerMixinTestHelper.assertNPCPicksUpLootOnlyIntoItsInventory(
        helper, entityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }
}
