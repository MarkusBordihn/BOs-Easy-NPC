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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTestHelper;

public final class PosePersistenceTest {

  private PosePersistenceTest() {}

  public static void testAllayTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ALLAY));
  }

  public static void testBoggedTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.BOGGED));
  }

  public static void testCatTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CAT));
  }

  public static void testChickenTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CHICKEN));
  }

  public static void testCreeperTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CREEPER));
  }

  public static void testDrownedTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.DROWNED));
  }

  public static void testEndermanTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ENDERMAN));
  }

  public static void testEvokerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.EVOKER));
  }

  public static void testFoxTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.FOX));
  }

  public static void testGhastTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.GHAST));
  }

  public static void testHorseTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HORSE));
  }

  public static void testHorseSkeletonTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HORSE_SKELETON));
  }

  public static void testHorseZombieTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HORSE_ZOMBIE));
  }

  public static void testHumanoidTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HUMANOID));
  }

  public static void testHumanoidSlimTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HUMANOID_SLIM));
  }

  public static void testIllusionerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ILLUSIONER));
  }

  public static void testIronGolemTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.IRON_GOLEM));
  }

  public static void testPiglinTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIGLIN));
  }

  public static void testPiglinBruteTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_BRUTE));
  }

  public static void testPiglinZombifiedTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
  }

  public static void testPigTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIG));
  }

  public static void testPillagerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PILLAGER));
  }

  public static void testSkeletonTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.SKELETON));
  }

  public static void testStrayTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.STRAY));
  }

  public static void testWitherSkeletonTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WITHER_SKELETON));
  }

  public static void testSlimeTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.SLIME));
  }

  public static void testSpiderTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.SPIDER));
  }

  public static void testCaveSpiderTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CAVE_SPIDER));
  }

  public static void testVillagerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.VILLAGER));
  }

  public static void testWanderingTraderTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WANDERING_TRADER));
  }

  public static void testVexTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.VEX));
  }

  public static void testVindicatorTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.VINDICATOR));
  }

  public static void testWitchTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WITCH));
  }

  public static void testWolfTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WOLF));
  }

  public static void testZombieTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE));
  }

  public static void testZombieHuskTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_HUSK));
  }

  public static void testZombieVillagerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_VILLAGER));
  }
}
