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

import de.markusbordihn.easynpc.entity.ModCustomEntityType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import de.markusbordihn.easynpc.item.ModItems;
import net.minecraft.gametest.framework.GameTestHelper;

public final class ModSpawnEggItemTest {

  private ModSpawnEggItemTest() {}

  public static void useAllayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ALLAY).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ALLAY));
    helper.succeed();
  }

  public static void useBoggedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.BOGGED).get(),
        ModEntityType.getEntityType(ModNPCEntityType.BOGGED));
    helper.succeed();
  }

  public static void useCatNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CAT).get(),
        ModEntityType.getEntityType(ModNPCEntityType.CAT));
    helper.succeed();
  }

  public static void useChickenNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CHICKEN).get(),
        ModEntityType.getEntityType(ModNPCEntityType.CHICKEN));
    helper.succeed();
  }

  public static void useCreeperNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CREEPER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.CREEPER));
    helper.succeed();
  }

  public static void useDrownedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.DROWNED).get(),
        ModEntityType.getEntityType(ModNPCEntityType.DROWNED));
    helper.succeed();
  }

  public static void useEndermanNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ENDERMAN).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ENDERMAN));
    helper.succeed();
  }

  public static void useEvokerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.EVOKER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.EVOKER));
    helper.succeed();
  }

  public static void useFoxNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.FOX).get(),
        ModEntityType.getEntityType(ModNPCEntityType.FOX));
    helper.succeed();
  }

  public static void useGhastNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.GHAST).get(),
        ModEntityType.getEntityType(ModNPCEntityType.GHAST));
    helper.succeed();
  }

  public static void useHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE));
    helper.succeed();
  }

  public static void useHorseSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE_SKELETON).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE_SKELETON));
    helper.succeed();
  }

  public static void useHorseZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE_ZOMBIE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE_ZOMBIE));
    helper.succeed();
  }

  public static void useHumanoidNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HUMANOID).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID));
    helper.succeed();
  }

  public static void useHumanoidSlimNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HUMANOID_SLIM).get(),
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID_SLIM));
    helper.succeed();
  }

  public static void useIllusionerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ILLUSIONER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ILLUSIONER));
    helper.succeed();
  }

  public static void useIronGolemNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.IRON_GOLEM).get(),
        ModEntityType.getEntityType(ModNPCEntityType.IRON_GOLEM));
    helper.succeed();
  }

  public static void usePiglinNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  public static void usePiglinBruteNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN_BRUTE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_BRUTE));
    helper.succeed();
  }

  public static void usePiglinZombifiedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN_ZOMBIFIED).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
    helper.succeed();
  }

  public static void usePigNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIG).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PIG));
    helper.succeed();
  }

  public static void usePillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PILLAGER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.PILLAGER));
    helper.succeed();
  }

  public static void useSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SKELETON).get(),
        ModEntityType.getEntityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  public static void useStrayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.STRAY).get(),
        ModEntityType.getEntityType(ModNPCEntityType.STRAY));
    helper.succeed();
  }

  public static void useWitherSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WITHER_SKELETON).get(),
        ModEntityType.getEntityType(ModNPCEntityType.WITHER_SKELETON));
    helper.succeed();
  }

  public static void useSlimeNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SLIME).get(),
        ModEntityType.getEntityType(ModNPCEntityType.SLIME));
    helper.succeed();
  }

  public static void useSpiderNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SPIDER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.SPIDER));
    helper.succeed();
  }

  public static void useCaveSpiderNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CAVE_SPIDER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.CAVE_SPIDER));
    helper.succeed();
  }

  public static void useVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VILLAGER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }

  public static void useWanderingTraderNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WANDERING_TRADER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.WANDERING_TRADER));
    helper.succeed();
  }

  public static void useVexNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VEX).get(),
        ModEntityType.getEntityType(ModNPCEntityType.VEX));
    helper.succeed();
  }

  public static void useVindicatorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VINDICATOR).get(),
        ModEntityType.getEntityType(ModNPCEntityType.VINDICATOR));
    helper.succeed();
  }

  public static void useWitchNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WITCH).get(),
        ModEntityType.getEntityType(ModNPCEntityType.WITCH));
    helper.succeed();
  }

  public static void useWolfNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WOLF).get(),
        ModEntityType.getEntityType(ModNPCEntityType.WOLF));
    helper.succeed();
  }

  public static void useZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  public static void useZombieHuskNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE_HUSK).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_HUSK));
    helper.succeed();
  }

  public static void useZombieVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE_VILLAGER).get(),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_VILLAGER));
    helper.succeed();
  }

  public static void useDopplerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.DOPPLER).get(),
        ModEntityType.getEntityType(ModCustomEntityType.DOPPLER));
    helper.succeed();
  }

  public static void useFairyNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.FAIRY).get(),
        ModEntityType.getEntityType(ModCustomEntityType.FAIRY));
    helper.succeed();
  }

  public static void useOrcNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.ORC).get(),
        ModEntityType.getEntityType(ModCustomEntityType.ORC));
    helper.succeed();
  }

  public static void useOrcWarriorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.ORC_WARRIOR).get(),
        ModEntityType.getEntityType(ModCustomEntityType.ORC_WARRIOR));
    helper.succeed();
  }
}
