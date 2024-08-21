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
import java.util.Map;
import java.util.Map.Entry;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.item.Item;

@SuppressWarnings("unused")
public class ModSpawnEggItemTest {

  public static final Map<Item, EntityType<? extends PathfinderMob>> spawnEggMap =
      Map.ofEntries(
          Map.entry(ModItems.ALLAY_NPC_SPAWN_EGG, ModEntityType.ALLAY),
          Map.entry(ModItems.CAT_NPC_SPAWN_EGG, ModEntityType.CAT),
          Map.entry(ModItems.CHICKEN_NPC_SPAWN_EGG, ModEntityType.CHICKEN),
          Map.entry(ModItems.DROWNED_NPC_SPAWN_EGG, ModEntityType.DROWNED),
          Map.entry(ModItems.EVOKER_NPC_SPAWN_EGG, ModEntityType.EVOKER),
          Map.entry(ModItems.FAIRY_NPC_SPAWN_EGG, ModEntityType.FAIRY),
          Map.entry(ModItems.HORSE_NPC_SPAWN_EGG, ModEntityType.HORSE),
          Map.entry(ModItems.HUMANOID_NPC_SPAWN_EGG, ModEntityType.HUMANOID),
          Map.entry(ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG, ModEntityType.HUMANOID_SLIM),
          Map.entry(ModItems.HUSK_NPC_SPAWN_EGG, ModEntityType.HUSK),
          Map.entry(ModItems.ILLUSIONER_NPC_SPAWN_EGG, ModEntityType.ILLUSIONER),
          Map.entry(ModItems.IRON_GOLEM_NPC_SPAWN_EGG, ModEntityType.IRON_GOLEM),
          Map.entry(ModItems.ORC_NPC_SPAWN_EGG, ModEntityType.ORC),
          Map.entry(ModItems.ORC_WARRIOR_NPC_SPAWN_EGG, ModEntityType.ORC_WARRIOR),
          Map.entry(ModItems.PIG_NPC_SPAWN_EGG, ModEntityType.PIG),
          Map.entry(ModItems.PIGLIN_BRUTE_NPC_SPAWN_EGG, ModEntityType.PIGLIN_BRUTE),
          Map.entry(ModItems.PIGLIN_NPC_SPAWN_EGG, ModEntityType.PIGLIN),
          Map.entry(ModItems.PIGLIN_ZOMBIFIED_NPC_SPAWN_EGG, ModEntityType.PIGLIN_ZOMBIFIED),
          Map.entry(ModItems.PILLAGER_NPC_SPAWN_EGG, ModEntityType.PILLAGER),
          Map.entry(ModItems.SKELETON_HORSE_NPC_SPAWN_EGG, ModEntityType.SKELETON_HORSE),
          Map.entry(ModItems.SKELETON_NPC_SPAWN_EGG, ModEntityType.SKELETON),
          Map.entry(ModItems.STRAY_NPC_SPAWN_EGG, ModEntityType.STRAY),
          Map.entry(ModItems.VILLAGER_NPC_SPAWN_EGG, ModEntityType.VILLAGER),
          Map.entry(ModItems.VINDICATOR_NPC_SPAWN_EGG, ModEntityType.VINDICATOR),
          Map.entry(ModItems.WITHER_SKELETON_NPC_SPAWN_EGG, ModEntityType.WITHER_SKELETON),
          Map.entry(ModItems.WOLF_NPC_SPAWN_EGG, ModEntityType.WOLF),
          Map.entry(ModItems.ZOMBIE_HORSE_NPC_SPAWN_EGG, ModEntityType.ZOMBIE_HORSE),
          Map.entry(ModItems.ZOMBIE_NPC_SPAWN_EGG, ModEntityType.ZOMBIE),
          Map.entry(ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG, ModEntityType.ZOMBIE_VILLAGER));

  public ModSpawnEggItemTest() {}

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useHumanoidSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUMANOID_NPC_SPAWN_EGG, ModEntityType.HUMANOID);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useSpawnEggItems(GameTestHelper helper) {
    for (Entry<Item, EntityType<? extends PathfinderMob>> item : spawnEggMap.entrySet()) {
      ModSpawnEggItemTestHelper.useSpawnEggItem(helper, item.getKey(), item.getValue());
    }
    helper.succeed();
  }
}
