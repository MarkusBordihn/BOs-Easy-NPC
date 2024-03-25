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

import de.markusbordihn.easynpc.item.ModItems;
import net.minecraft.world.item.CreativeModeTab.DisplayItemsGenerator;
import net.minecraft.world.item.CreativeModeTab.ItemDisplayParameters;
import net.minecraft.world.item.CreativeModeTab.Output;

public class SpawnEggs implements DisplayItemsGenerator {

  protected SpawnEggs() {}

  @Override
  public void accept(ItemDisplayParameters itemDisplayParameters, Output output) {
    output.accept(ModItems.ALLAY_NPC_SPAWN_EGG.get());
    output.accept(ModItems.CAT_NPC_SPAWN_EGG.get());
    output.accept(ModItems.CHICKEN_NPC_SPAWN_EGG.get());
    output.accept(ModItems.DROWNED_NPC_SPAWN_EGG.get());
    output.accept(ModItems.FAIRY_NPC_SPAWN_EGG.get());
    output.accept(ModItems.HUMANOID_NPC_SPAWN_EGG.get());
    output.accept(ModItems.HUMANOID_SLIM_NPC_SPAWN_EGG.get());
    output.accept(ModItems.HUSK_NPC_SPAWN_EGG.get());
    output.accept(ModItems.IRON_GOLEM_NPC_SPAWN_EGG.get());
    output.accept(ModItems.SKELETON_NPC_SPAWN_EGG.get());
    output.accept(ModItems.STRAY_NPC_SPAWN_EGG.get());
    output.accept(ModItems.VILLAGER_NPC_SPAWN_EGG.get());
    output.accept(ModItems.WITHER_SKELETON_NPC_SPAWN_EGG.get());
    output.accept(ModItems.ZOMBIE_NPC_SPAWN_EGG.get());
    output.accept(ModItems.ZOMBIE_VILLAGER_NPC_SPAWN_EGG.get());
    output.accept(ModItems.PIG_NPC_SPAWN_EGG.get());
    output.accept(ModItems.EVOKER_NPC_SPAWN_EGG.get());
    output.accept(ModItems.ILLUSIONER_NPC_SPAWN_EGG.get());
    output.accept(ModItems.PILLAGER_NPC_SPAWN_EGG.get());
    output.accept(ModItems.VINDICATOR_NPC_SPAWN_EGG.get());
  }
}
