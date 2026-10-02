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
import net.fabricmc.fabric.api.gametest.v1.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

@SuppressWarnings("unused")
public class PosePersistenceTest {

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testAllayTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ALLAY));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testBoggedTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.BOGGED));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testCatTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CAT));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testChickenTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CHICKEN));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testCreeperTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CREEPER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testDrownedTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.DROWNED));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testEndermanTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ENDERMAN));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testEvokerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.EVOKER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testFoxTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.FOX));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testGhastTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.GHAST));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testHorseTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HORSE));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testHorseSkeletonTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HORSE_SKELETON));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testHorseZombieTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HORSE_ZOMBIE));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testHumanoidTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HUMANOID));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testHumanoidSlimTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.HUMANOID_SLIM));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testIllusionerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ILLUSIONER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testIronGolemTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.IRON_GOLEM));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testPiglinTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIGLIN));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testPiglinBruteTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_BRUTE));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testPiglinZombifiedTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testPigTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PIG));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testPillagerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.PILLAGER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testSkeletonTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.SKELETON));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testStrayTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.STRAY));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testWitherSkeletonTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WITHER_SKELETON));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testSlimeTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.SLIME));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testSpiderTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.SPIDER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testCaveSpiderTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.CAVE_SPIDER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testVillagerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.VILLAGER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testWanderingTraderTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WANDERING_TRADER));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testVexTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.VEX));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testVindicatorTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.VINDICATOR));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testWitchTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WITCH));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testWolfTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.WOLF));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testZombieTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testZombieHuskTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_HUSK));
  }

  @GameTest(
      structure = "easy_npc_config_ui:gametest.3x3x3",
      maxTicks = PosePersistenceTestHelper.TIMEOUT_TICKS)
  public void testZombieVillagerTPosePersistence(GameTestHelper helper) {
    PosePersistenceTestHelper.assertTPoseSurvivesWaitRespawnAndPreset(
        helper, ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_VILLAGER));
  }
}
