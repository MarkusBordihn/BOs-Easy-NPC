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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.item.ModItems;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.neoforged.neoforge.gametest.GameTestHolder;
import net.neoforged.neoforge.gametest.PrefixGameTestTemplate;

@SuppressWarnings("unused")
@PrefixGameTestTemplate(value = false)
@GameTestHolder(Constants.MOD_ID)
public class ModSpawnEggItemTest {

  @GameTest(template = "gametest.3x3x3")
  public static void useAllayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ALLAY_NPC_SPAWN_EGG.get(), ModEntityType.ALLAY.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useCatNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.CAT_NPC_SPAWN_EGG.get(), ModEntityType.CAT.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useChickenNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.CHICKEN_NPC_SPAWN_EGG.get(), ModEntityType.CHICKEN.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useDrownedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.DROWNED_NPC_SPAWN_EGG.get(), ModEntityType.DROWNED.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useEvokerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.EVOKER_NPC_SPAWN_EGG.get(), ModEntityType.EVOKER.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useFairyNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.FAIRY_NPC_SPAWN_EGG.get(), ModEntityType.FAIRY.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HORSE_NPC_SPAWN_EGG.get(), ModEntityType.HORSE.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHumanoidNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUMANOID_NPC_SPAWN_EGG.get(), ModEntityType.HUMANOID.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHumanoidSlimNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG.get(), ModEntityType.HUMANOID_SLIM.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHuskNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUSK_NPC_SPAWN_EGG.get(), ModEntityType.HUSK.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useIllusionerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ILLUSIONER_NPC_SPAWN_EGG.get(), ModEntityType.ILLUSIONER.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useIronGolemNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.IRON_GOLEM_NPC_SPAWN_EGG.get(), ModEntityType.IRON_GOLEM.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useOrcNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ORC_NPC_SPAWN_EGG.get(), ModEntityType.ORC.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useOrcWarriorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ORC_WARRIOR_NPC_SPAWN_EGG.get(), ModEntityType.ORC_WARRIOR.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePigNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIG_NPC_SPAWN_EGG.get(), ModEntityType.PIG.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePiglinNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIGLIN_NPC_SPAWN_EGG.get(), ModEntityType.PIGLIN.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePiglinBruteNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PIGLIN_BRUTE_NPC_SPAWN_EGG.get(), ModEntityType.PIGLIN_BRUTE.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePiglinZombifiedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.PIGLIN_ZOMBIFIED_NPC_SPAWN_EGG.get(),
        ModEntityType.PIGLIN_ZOMBIFIED.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.PILLAGER_NPC_SPAWN_EGG.get(), ModEntityType.PILLAGER.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useSkeletonHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.SKELETON_HORSE_NPC_SPAWN_EGG.get(), ModEntityType.SKELETON_HORSE.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.SKELETON_NPC_SPAWN_EGG.get(), ModEntityType.SKELETON.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useStrayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.STRAY_NPC_SPAWN_EGG.get(), ModEntityType.STRAY.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.VILLAGER_NPC_SPAWN_EGG.get(), ModEntityType.VILLAGER.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useVindicatorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.VINDICATOR_NPC_SPAWN_EGG.get(), ModEntityType.VINDICATOR.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useWitherSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.WITHER_SKELETON_NPC_SPAWN_EGG.get(), ModEntityType.WITHER_SKELETON.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useWolfNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.WOLF_NPC_SPAWN_EGG.get(), ModEntityType.WOLF.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useZombieHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ZOMBIE_HORSE_NPC_SPAWN_EGG.get(), ModEntityType.ZOMBIE_HORSE.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ZOMBIE_NPC_SPAWN_EGG.get(), ModEntityType.ZOMBIE.get());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useZombieVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG.get(), ModEntityType.ZOMBIE_VILLAGER.get());
    helper.succeed();
  }
}
