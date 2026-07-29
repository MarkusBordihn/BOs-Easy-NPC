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
import net.fabricmc.fabric.api.gametest.v1.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

@SuppressWarnings("unused")
public class ModSpawnEggItemTest {

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useAllayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ALLAY),
        ModEntityType.getEntityType(ModNPCEntityType.ALLAY));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useBoggedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.BOGGED),
        ModEntityType.getEntityType(ModNPCEntityType.BOGGED));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useCatNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CAT),
        ModEntityType.getEntityType(ModNPCEntityType.CAT));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useChickenNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CHICKEN),
        ModEntityType.getEntityType(ModNPCEntityType.CHICKEN));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useCreeperNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CREEPER),
        ModEntityType.getEntityType(ModNPCEntityType.CREEPER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useDrownedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.DROWNED),
        ModEntityType.getEntityType(ModNPCEntityType.DROWNED));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useEndermanNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ENDERMAN),
        ModEntityType.getEntityType(ModNPCEntityType.ENDERMAN));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useEvokerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.EVOKER),
        ModEntityType.getEntityType(ModNPCEntityType.EVOKER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useFoxNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.FOX),
        ModEntityType.getEntityType(ModNPCEntityType.FOX));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useGhastNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.GHAST),
        ModEntityType.getEntityType(ModNPCEntityType.GHAST));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useHorseNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useHorseSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE_SKELETON),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE_SKELETON));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useHorseZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HORSE_ZOMBIE),
        ModEntityType.getEntityType(ModNPCEntityType.HORSE_ZOMBIE));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useHumanoidNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HUMANOID),
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useHumanoidSlimNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.HUMANOID_SLIM),
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID_SLIM));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useIllusionerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ILLUSIONER),
        ModEntityType.getEntityType(ModNPCEntityType.ILLUSIONER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useIronGolemNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.IRON_GOLEM),
        ModEntityType.getEntityType(ModNPCEntityType.IRON_GOLEM));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void usePiglinNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void usePiglinBruteNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN_BRUTE),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_BRUTE));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void usePiglinZombifiedNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIGLIN_ZOMBIFIED),
        ModEntityType.getEntityType(ModNPCEntityType.PIGLIN_ZOMBIFIED));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void usePigNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PIG),
        ModEntityType.getEntityType(ModNPCEntityType.PIG));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void usePillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.PILLAGER),
        ModEntityType.getEntityType(ModNPCEntityType.PILLAGER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SKELETON),
        ModEntityType.getEntityType(ModNPCEntityType.SKELETON));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useStrayNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.STRAY),
        ModEntityType.getEntityType(ModNPCEntityType.STRAY));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useWitherSkeletonNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WITHER_SKELETON),
        ModEntityType.getEntityType(ModNPCEntityType.WITHER_SKELETON));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useSlimeNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SLIME),
        ModEntityType.getEntityType(ModNPCEntityType.SLIME));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useSpiderNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.SPIDER),
        ModEntityType.getEntityType(ModNPCEntityType.SPIDER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useCaveSpiderNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.CAVE_SPIDER),
        ModEntityType.getEntityType(ModNPCEntityType.CAVE_SPIDER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VILLAGER),
        ModEntityType.getEntityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useWanderingTraderNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WANDERING_TRADER),
        ModEntityType.getEntityType(ModNPCEntityType.WANDERING_TRADER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useVexNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VEX),
        ModEntityType.getEntityType(ModNPCEntityType.VEX));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useVindicatorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.VINDICATOR),
        ModEntityType.getEntityType(ModNPCEntityType.VINDICATOR));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useWitchNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WITCH),
        ModEntityType.getEntityType(ModNPCEntityType.WITCH));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useWolfNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.WOLF),
        ModEntityType.getEntityType(ModNPCEntityType.WOLF));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useZombieNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useZombieHuskNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE_HUSK),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_HUSK));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useZombieVillagerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.NPC_SPAWN_EGGS.get(ModNPCEntityType.ZOMBIE_VILLAGER),
        ModEntityType.getEntityType(ModNPCEntityType.ZOMBIE_VILLAGER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useDopplerNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.DOPPLER),
        ModEntityType.getEntityType(ModCustomEntityType.DOPPLER));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useFairyNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.FAIRY),
        ModEntityType.getEntityType(ModCustomEntityType.FAIRY));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useOrcNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.ORC),
        ModEntityType.getEntityType(ModCustomEntityType.ORC));
    helper.succeed();
  }

  @GameTest(structure = "easy_npc:gametest.3x3x3")
  public void useOrcWarriorNPCSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper,
        ModItems.CUSTOM_NPC_SPAWN_EGGS.get(ModCustomEntityType.ORC_WARRIOR),
        ModEntityType.getEntityType(ModCustomEntityType.ORC_WARRIOR));
    helper.succeed();
  }
}
