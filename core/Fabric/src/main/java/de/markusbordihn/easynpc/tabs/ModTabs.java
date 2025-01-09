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
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModTabs {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModTabs() {}

  public static void handleCreativeModeTabRegister() {

    log.info("{} creative mod tabs ...", Constants.LOG_REGISTER_PREFIX);

    Registry.register(
        BuiltInRegistries.CREATIVE_MODE_TAB,
        Constants.MOD_ID + ":config_items",
        FabricItemGroup.builder()
            .icon(() -> ModItems.EASY_NPC_WAND.asItem().getDefaultInstance())
            .title(Component.translatable("itemGroup.easy_npc.config_items"))
            .displayItems(new ConfigItems())
            .build());

    Registry.register(
        BuiltInRegistries.CREATIVE_MODE_TAB,
        Constants.MOD_ID + ":spawn_eggs",
        FabricItemGroup.builder()
            .icon(() -> ModItems.FAIRY_NPC_SPAWN_EGG.asItem().getDefaultInstance())
            .title(Component.translatable("itemGroup.easy_npc.spawn_eggs"))
            .displayItems(new SpawnEggs())
            .build());
  }
}
