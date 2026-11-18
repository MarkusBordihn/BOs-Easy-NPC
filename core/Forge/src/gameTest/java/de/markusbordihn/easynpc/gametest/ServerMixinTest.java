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
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

public final class ServerMixinTest {

  private ServerMixinTest() {}

  private static EntityType<?> entityType(ModNPCEntityType npcEntityType) {
    return ModEntityType.getEntityType(npcEntityType);
  }

  public static void testCreeperNPCDoesNotExplode(GameTestHelper helper) {
    ServerMixinTestHelper.assertCreeperNPCDoesNotExplode(
        helper, entityType(ModNPCEntityType.CREEPER));
    helper.succeed();
  }

  public static void testPiglinNPCSkipsVanillaActivityUpdate(GameTestHelper helper) {
    ServerMixinTestHelper.assertPiglinNPCSkipsVanillaActivityUpdate(
        helper, entityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  public static void testPiglinBruteNPCSkipsVanillaActivityUpdate(GameTestHelper helper) {
    ServerMixinTestHelper.assertPiglinNPCSkipsVanillaActivityUpdate(
        helper, entityType(ModNPCEntityType.PIGLIN_BRUTE));
    helper.succeed();
  }

  public static void testFoxNPCGetsNoVanillaTargetGoals(GameTestHelper helper) {
    ServerMixinTestHelper.assertFoxNPCGetsNoVanillaTargetGoals(
        helper, entityType(ModNPCEntityType.FOX));
    helper.succeed();
  }

  public static void testWitchNPCDrinksNoPotions(GameTestHelper helper) {
    ServerMixinTestHelper.assertWitchNPCDrinksNoPotions(helper, entityType(ModNPCEntityType.WITCH));
    helper.succeed();
  }

  public static void testVillagerNPCKeepsItsTradingPlayer(GameTestHelper helper) {
    ServerMixinTestHelper.assertVillagerNPCKeepsItsTradingPlayer(
        helper, entityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }

  public static void testSlimeNPCKeepsItsSizeOnFinalizeSpawn(GameTestHelper helper) {
    ServerMixinTestHelper.assertSlimeNPCKeepsItsSizeOnFinalizeSpawn(
        helper, entityType(ModNPCEntityType.SLIME));
    helper.succeed();
  }

  public static void testWalkAnimationCopyTakesEveryValue(GameTestHelper helper) {
    ServerMixinTestHelper.assertWalkAnimationCopyTakesEveryValue(helper);
    helper.succeed();
  }

  public static void testSpawnerDataMatchesEverySpawnerType(GameTestHelper helper) {
    ServerMixinTestHelper.assertSpawnerDataMatchesEverySpawnerType(helper);
    helper.succeed();
  }

  public static void testZombieNPCIsNotReplacedByVanillaConversion(GameTestHelper helper) {
    ServerMixinTestHelper.assertNPCIsNotReplacedByVanillaConversion(
        helper, entityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  public static void testSkeletonNPCIsNotReplacedByVanillaConversion(GameTestHelper helper) {
    ServerMixinTestHelper.assertNPCIsNotReplacedByVanillaConversion(
        helper, entityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  public static void testZombieNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  public static void testHuskNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.ZOMBIE_HUSK));
    helper.succeed();
  }

  public static void testDrownedNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.DROWNED));
    helper.succeed();
  }

  public static void testZombieVillagerNPCGetsNoRandomSpawnExtras(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombieNPCGetsNoRandomSpawnExtras(
        helper, entityType(ModNPCEntityType.ZOMBIE_VILLAGER));
    helper.succeed();
  }

  public static void testZombifiedPiglinNPCKeepsItsWeaponOnFinalizeSpawn(GameTestHelper helper) {
    ServerMixinTestHelper.assertZombifiedPiglinNPCKeepsItsWeaponOnFinalizeSpawn(
        helper, entityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
    helper.succeed();
  }

  public static void testSkeletonNPCGetsTheVanillaBowOnlyWithoutEquipment(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsTheVanillaBowOnlyWithoutEquipment(
        helper, entityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  public static void testSkeletonNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  public static void testStrayNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.STRAY));
    helper.succeed();
  }

  public static void testWitherSkeletonNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.WITHER_SKELETON));
    helper.succeed();
  }

  public static void testBoggedNPCGetsNoVanillaWeaponGoal(GameTestHelper helper) {
    ServerMixinTestHelper.assertSkeletonNPCGetsNoVanillaWeaponGoal(
        helper, entityType(ModNPCEntityType.BOGGED));
    helper.succeed();
  }

  public static void testVillagerNPCPicksUpLootOnlyIntoItsInventory(GameTestHelper helper) {
    ServerMixinTestHelper.assertNPCPicksUpLootOnlyIntoItsInventory(
        helper, entityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }
}
