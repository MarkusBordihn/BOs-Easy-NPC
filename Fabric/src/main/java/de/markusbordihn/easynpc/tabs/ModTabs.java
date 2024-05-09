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

package de.markusbordihn.easynpc.tabs;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.item.ModItems;
import net.fabricmc.fabric.api.client.itemgroup.FabricItemGroupBuilder;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModTabs {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModTabs() {
  }

  public static void registerModTabs() {
    log.info("{} Registering Mod Tabs ...", Constants.LOG_REGISTER_PREFIX);

    FabricItemGroupBuilder.create(new ResourceLocation(Constants.MOD_ID, "spawn_eggs"))
        .icon(ModItems.FAIRY_NPC_SPAWN_EGG::getDefaultInstance)
        .appendItems(
            stack -> {
              stack.add(ModItems.ALLEY_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.CAT_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.CHICKEN_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.FAIRY_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.HUMANOID_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.IRON_GOLEM_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.SKELETON_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.VILLAGER_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.DROWNED_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.HUSK_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.WITHER_SKELETON_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.STRAY_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.ZOMBIE_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.PIG_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.EVOKER_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.ILLUSIONER_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.PILLAGER_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.VINDICATOR_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.ORC_NPC_SPAWN_EGG.getDefaultInstance());
              stack.add(ModItems.ORC_WARRIOR_NPC_SPAWN_EGG.getDefaultInstance());
            })
        .build();

    FabricItemGroupBuilder.create(new ResourceLocation(Constants.MOD_ID, "config_items"))
        .icon(ModItems.EASY_NPC_WAND::getDefaultInstance)
        .appendItems(
            stack -> {
              stack.add(ModItems.EASY_NPC_WAND.getDefaultInstance());
              stack.add(ModItems.MOVE_EASY_NPC.getDefaultInstance());
              stack.add(ModItems.EASY_NPC_PRESET_ITEM.getDefaultInstance());
              stack.add(ModItems.EASY_NPC_PRESET_EMPTY_ITEM.getDefaultInstance());
              stack.add(ModItems.EASY_NPC_SPAWNER.getDefaultInstance());
            })
        .build();
  }
}
