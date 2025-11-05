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
import de.markusbordihn.easynpc.entity.ModCustomEntityType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
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
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ALLAY).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ALLAY));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useCatNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CAT).get(),
        ModEntityType.getEntityType(ModNPCEntityType.CAT));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useChickenNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CHICKEN).get(),
        ModEntityType.getEntityType(ModNPCEntityType.CHICKEN));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useDrownedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.DROWNED).get(),
        ModEntityType.getEntityType(ModNPCEntityType.DROWNED));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useEvokerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.EVOKER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.EVOKER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useFairyNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.FAIRY).get(),
        ModEntityType.getEntityType(ModCustomEntityType.FAIRY));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHumanoidNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HUMANOID).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHumanoidSlimNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HUMANOID_SLIM).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID_SLIM));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useHuskNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE_HUSK).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_HUSK));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useIllusionerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ILLUSIONER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ILLUSIONER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useIronGolemNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.IRON_GOLEM).get(),
        ModEntityType.getEntityType(ModNPCEntityType.IRON_GOLEM));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useOrcNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.ORC).get(),
        ModEntityType.getEntityType(ModCustomEntityType.ORC));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useOrcWarriorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.ORC_WARRIOR).get(),
        ModEntityType.getEntityType(ModCustomEntityType.ORC_WARRIOR));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePigNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIG).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIG));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePiglinNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePiglinBruteNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN_BRUTE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_BRUTE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePiglinZombifiedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN_ZOMBIFIED).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void usePillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PILLAGER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PILLAGER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useSkeletonHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE_SKELETON).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE_SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SKELETON).get(),
        ModEntityType.getEntityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useStrayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.STRAY).get(),
        ModEntityType.getEntityType(ModNPCEntityType.STRAY));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VILLAGER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useVindicatorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VINDICATOR).get(),
        ModEntityType.getEntityType(ModNPCEntityType.VINDICATOR));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useWitherSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WITHER_SKELETON).get(),
        ModEntityType.getEntityType(ModNPCEntityType.WITHER_SKELETON));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useWolfNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WOLF).get(),
        ModEntityType.getEntityType(ModNPCEntityType.WOLF));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useZombieHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE_ZOMBIE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE_ZOMBIE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public static void useZombieVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE_VILLAGER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_VILLAGER));
    helper.succeed();
  }
}
