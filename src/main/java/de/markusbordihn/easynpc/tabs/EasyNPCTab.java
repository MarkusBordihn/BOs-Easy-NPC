/**
 * Copyright 2023 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.tabs;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.item.ModItems;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.flag.FeatureFlagSet;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.CreativeModeTab.Output;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.event.CreativeModeTabEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCTab {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  public static CreativeModeTab TAB_CONFIG_ITEMS;
  public static CreativeModeTab TAB_SPAWN_EGGS;

  protected EasyNPCTab() {}

  public static void handleCreativeModeTabRegister(CreativeModeTabEvent.Register event) {

    log.info("{} creative mod tabs ...", Constants.LOG_REGISTER_PREFIX);

    TAB_CONFIG_ITEMS =
        event.registerCreativeModeTab(
            new ResourceLocation(Constants.MOD_ID, "config_items"),
            builder -> {
              builder
                  .icon(() -> new ItemStack(ModItems.EASY_NPC_WAND.get()))
                  .displayItems(EasyNPCTab::addConfigItemsTabItems)
                  .title(Component.translatable("itemGroup.easy_npc.config_items"))
                  .build();
            });

    TAB_SPAWN_EGGS =
        event.registerCreativeModeTab(
            new ResourceLocation(Constants.MOD_ID, "spawn_eggs"),
            builder -> {
              builder
                  .icon(() -> new ItemStack(ModItems.VILLAGER_NPC_SPAWN_EGG.get()))
                  .displayItems(EasyNPCTab::addSpawnEggsTabItems)
                  .title(Component.translatable("itemGroup.easy_npc.spawn_eggs"))
                  .build();
            });
  }

  private static void addConfigItemsTabItems(
      FeatureFlagSet featureFlagSet, Output outputTab, boolean hasPermissions) {
    outputTab.accept(ModItems.EASY_NPC_WAND.get());
  }

  private static void addSpawnEggsTabItems(
      FeatureFlagSet featureFlagSet, Output outputTab, boolean hasPermissions) {
    outputTab.accept(ModItems.ALLAY_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.CAT_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.CHICKEN_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.FAIRY_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.HUMANOID_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.IRON_GOLEM_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.SKELETON_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.VILLAGER_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.ZOMBIE_NPC_SPAWN_EGG.get());
    outputTab.accept(ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG.get());
  }
}
