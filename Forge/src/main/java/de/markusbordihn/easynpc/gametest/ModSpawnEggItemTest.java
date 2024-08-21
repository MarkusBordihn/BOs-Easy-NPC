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
import java.util.Map;
import java.util.Map.Entry;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.item.Item;
import net.minecraftforge.gametest.GameTestHolder;

@SuppressWarnings("unused")
@GameTestHolder(Constants.MOD_ID)
public class ModSpawnEggItemTest {

  public static final Map<Item, EntityType<? extends PathfinderMob>> spawnEggMap =
      Map.ofEntries(
          Map.entry(ModItems.ALLAY_NPC_SPAWN_EGG.get(), ModEntityType.ALLAY.get()),
          Map.entry(ModItems.CAT_NPC_SPAWN_EGG.get(), ModEntityType.CAT.get()),
          Map.entry(ModItems.CHICKEN_NPC_SPAWN_EGG.get(), ModEntityType.CHICKEN.get()),
          Map.entry(ModItems.DROWNED_NPC_SPAWN_EGG.get(), ModEntityType.DROWNED.get()),
          Map.entry(ModItems.EVOKER_NPC_SPAWN_EGG.get(), ModEntityType.EVOKER.get()),
          Map.entry(ModItems.FAIRY_NPC_SPAWN_EGG.get(), ModEntityType.FAIRY.get()),
          Map.entry(ModItems.HORSE_NPC_SPAWN_EGG.get(), ModEntityType.HORSE.get()),
          Map.entry(ModItems.HUMANOID_NPC_SPAWN_EGG.get(), ModEntityType.HUMANOID.get()),
          Map.entry(ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG.get(), ModEntityType.HUMANOID_SLIM.get()),
          Map.entry(ModItems.HUSK_NPC_SPAWN_EGG.get(), ModEntityType.HUSK.get()),
          Map.entry(ModItems.ILLUSIONER_NPC_SPAWN_EGG.get(), ModEntityType.ILLUSIONER.get()),
          Map.entry(ModItems.IRON_GOLEM_NPC_SPAWN_EGG.get(), ModEntityType.IRON_GOLEM.get()),
          Map.entry(ModItems.ORC_NPC_SPAWN_EGG.get(), ModEntityType.ORC.get()),
          Map.entry(ModItems.ORC_WARRIOR_NPC_SPAWN_EGG.get(), ModEntityType.ORC_WARRIOR.get()),
          Map.entry(ModItems.PIG_NPC_SPAWN_EGG.get(), ModEntityType.PIG.get()),
          Map.entry(ModItems.PIGLIN_BRUTE_NPC_SPAWN_EGG.get(), ModEntityType.PIGLIN_BRUTE.get()),
          Map.entry(ModItems.PIGLIN_NPC_SPAWN_EGG.get(), ModEntityType.PIGLIN.get()),
          Map.entry(
              ModItems.PIGLIN_ZOMBIFIED_NPC_SPAWN_EGG.get(), ModEntityType.PIGLIN_ZOMBIFIED.get()),
          Map.entry(ModItems.PILLAGER_NPC_SPAWN_EGG.get(), ModEntityType.PILLAGER.get()),
          Map.entry(
              ModItems.SKELETON_HORSE_NPC_SPAWN_EGG.get(), ModEntityType.SKELETON_HORSE.get()),
          Map.entry(ModItems.SKELETON_NPC_SPAWN_EGG.get(), ModEntityType.SKELETON.get()),
          Map.entry(ModItems.STRAY_NPC_SPAWN_EGG.get(), ModEntityType.STRAY.get()),
          Map.entry(ModItems.VILLAGER_NPC_SPAWN_EGG.get(), ModEntityType.VILLAGER.get()),
          Map.entry(ModItems.VINDICATOR_NPC_SPAWN_EGG.get(), ModEntityType.VINDICATOR.get()),
          Map.entry(
              ModItems.WITHER_SKELETON_NPC_SPAWN_EGG.get(), ModEntityType.WITHER_SKELETON.get()),
          Map.entry(ModItems.WOLF_NPC_SPAWN_EGG.get(), ModEntityType.WOLF.get()),
          Map.entry(ModItems.ZOMBIE_HORSE_NPC_SPAWN_EGG.get(), ModEntityType.ZOMBIE_HORSE.get()),
          Map.entry(ModItems.ZOMBIE_NPC_SPAWN_EGG.get(), ModEntityType.ZOMBIE.get()),
          Map.entry(
              ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG.get(), ModEntityType.ZOMBIE_VILLAGER.get()));

  public ModSpawnEggItemTest() {}

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useSpawnEggItem(GameTestHelper helper) {
    for (Entry<Item, EntityType<? extends PathfinderMob>> item : spawnEggMap.entrySet()) {
      ModSpawnEggItemTestHelper.useSpawnEggItem(helper, item.getKey(), item.getValue());
    }
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public static void useHumanoidSpawnEggItem(GameTestHelper helper) {
    ModSpawnEggItemTestHelper.useSpawnEggItem(
        helper, ModItems.HUMANOID_NPC_SPAWN_EGG.get(), ModEntityType.HUMANOID.get());
    helper.succeed();
  }
}
