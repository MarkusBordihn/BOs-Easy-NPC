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
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModTabs {

  public static final DeferredRegister<CreativeModeTab> CREATIVE_TABS =
      DeferredRegister.create(Registries.CREATIVE_MODE_TAB, Constants.MOD_ID);
  public static final RegistryObject<CreativeModeTab> TAB_CONFIG_ITEMS =
      CREATIVE_TABS.register(
          "config_items",
          () ->
              CreativeModeTab.builder()
                  .icon(() -> ModItems.EASY_NPC_WAND.get().getDefaultInstance())
                  .displayItems(new ConfigItems())
                  .title(Component.translatable("itemGroup.easy_npc.config_items"))
                  .build());
  public static final RegistryObject<CreativeModeTab> TAB_SPAWN_EGGS =
      CREATIVE_TABS.register(
          "spawn_eggs",
          () ->
              CreativeModeTab.builder()
                  .icon(() -> ModItems.VILLAGER_NPC_SPAWN_EGG.get().getDefaultInstance())
                  .displayItems(new SpawnEggs())
                  .title(Component.translatable("itemGroup.easy_npc.spawn_eggs"))
                  .build());
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected ModTabs() {}
}
