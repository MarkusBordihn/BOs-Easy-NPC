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
import net.fabricmc.fabric.api.itemgroup.v1.FabricItemGroup;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.flag.FeatureFlagSet;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.CreativeModeTab.ItemDisplayParameters;
import net.minecraft.world.item.CreativeModeTab.Output;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModTabs {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static CreativeModeTab TAB_CONFIG_ITEMS;
  public static CreativeModeTab TAB_SPAWN_EGGS;

  private ModTabs() {}

  public static void handleCreativeModeTabRegister() {

    log.info("{} creative mod tabs ...", Constants.LOG_REGISTER_PREFIX);

    TAB_CONFIG_ITEMS =
        FabricItemGroup.builder()
            .icon(() -> ModItems.EASY_NPC_WAND.asItem().getDefaultInstance())
            .displayItems(ModTabs::addConfigItemsTabItems)
            .build();
    TAB_SPAWN_EGGS =
        FabricItemGroup.builder()
            .icon(() -> ModItems.FAIRY_NPC_SPAWN_EGG.asItem().getDefaultInstance())
            .displayItems(ModTabs::addSpawnEggsTabItems)
            .build();
  }

  private static void addConfigItemsTabItems(
      ItemDisplayParameters itemDisplayParameters, Output output) {
    output.accept(ModItems.EASY_NPC_WAND.asItem().getDefaultInstance());
    output.accept(ModItems.MOVE_EASY_NPC.asItem().getDefaultInstance());
    output.accept(ModItems.EASY_NPC_PRESET_EMPTY_ITEM.asItem().getDefaultInstance());
  }

  private static void addSpawnEggsTabItems(
      ItemDisplayParameters itemDisplayParameters, Output output) {
    output.accept(ModItems.ALLEY_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.CAT_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.CHICKEN_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.FAIRY_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.HUMANOID_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.IRON_GOLEM_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.SKELETON_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.VILLAGER_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.DROWNED_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.HUSK_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.WITHER_SKELETON_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.STRAY_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.ZOMBIE_NPC_SPAWN_EGG.asItem().getDefaultInstance());
    output.accept(ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG.asItem().getDefaultInstance());
  }
}
