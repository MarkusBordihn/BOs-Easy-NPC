/*
 * Copyright 2023 Markus Bordihn
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
import de.markusbordihn.easynpc.item.ModItems;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

@SuppressWarnings("unused")
public class ModSpawnEggItemTest {

  public ModSpawnEggItemTest() {}

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useAllayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ALLAY_NPC_SPAWN_EGG, ModEntityType.ALLAY);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useCatNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.CAT_NPC_SPAWN_EGG, ModEntityType.CAT);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useChickenNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.CHICKEN_NPC_SPAWN_EGG, ModEntityType.CHICKEN);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useDrownedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.DROWNED_NPC_SPAWN_EGG, ModEntityType.DROWNED);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useEvokerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.EVOKER_NPC_SPAWN_EGG, ModEntityType.EVOKER);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useFairyNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.FAIRY_NPC_SPAWN_EGG, ModEntityType.FAIRY);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HORSE_NPC_SPAWN_EGG, ModEntityType.HORSE);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useHumanoidNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUMANOID_NPC_SPAWN_EGG, ModEntityType.HUMANOID);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useHumanoidSlimNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG, ModEntityType.HUMANOID_SLIM);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useHuskNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUSK_NPC_SPAWN_EGG, ModEntityType.HUSK);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useIllusionerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ILLUSIONER_NPC_SPAWN_EGG, ModEntityType.ILLUSIONER);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useIronGolemNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.IRON_GOLEM_NPC_SPAWN_EGG, ModEntityType.IRON_GOLEM);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useOrcNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ORC_NPC_SPAWN_EGG, ModEntityType.ORC);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useOrcWarriorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ORC_WARRIOR_NPC_SPAWN_EGG, ModEntityType.ORC_WARRIOR);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void usePigNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIG_NPC_SPAWN_EGG, ModEntityType.PIG);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void usePiglinNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIGLIN_NPC_SPAWN_EGG, ModEntityType.PIGLIN);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void usePiglinBruteNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIGLIN_BRUTE_NPC_SPAWN_EGG, ModEntityType.PIGLIN_BRUTE);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void usePiglinZombifiedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIGLIN_ZOMBIFIED_NPC_SPAWN_EGG, ModEntityType.PIGLIN_ZOMBIFIED);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void usePillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PILLAGER_NPC_SPAWN_EGG, ModEntityType.PILLAGER);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useSkeletonHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.SKELETON_HORSE_NPC_SPAWN_EGG, ModEntityType.SKELETON_HORSE);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.SKELETON_NPC_SPAWN_EGG, ModEntityType.SKELETON);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useStrayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.STRAY_NPC_SPAWN_EGG, ModEntityType.STRAY);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.VILLAGER_NPC_SPAWN_EGG, ModEntityType.VILLAGER);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useVindicatorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.VINDICATOR_NPC_SPAWN_EGG, ModEntityType.VINDICATOR);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useWitherSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.WITHER_SKELETON_NPC_SPAWN_EGG, ModEntityType.WITHER_SKELETON);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useWolfNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.WOLF_NPC_SPAWN_EGG, ModEntityType.WOLF);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useZombieHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ZOMBIE_HORSE_NPC_SPAWN_EGG, ModEntityType.ZOMBIE_HORSE);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ZOMBIE_NPC_SPAWN_EGG, ModEntityType.ZOMBIE);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useZombieVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG, ModEntityType.ZOMBIE_VILLAGER);
    helper.succeed();
  }
}
