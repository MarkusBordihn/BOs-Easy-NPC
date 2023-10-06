/**
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

package de.markusbordihn.easynpc.item;

import net.minecraft.world.item.Item;
import net.minecraft.world.item.Rarity;

import net.minecraftforge.registries.RegistryObject;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.npc.Cat;
import de.markusbordihn.easynpc.entity.npc.Chicken;
import de.markusbordihn.easynpc.entity.npc.Fairy;
import de.markusbordihn.easynpc.entity.npc.Humanoid;
import de.markusbordihn.easynpc.entity.npc.HumanoidSlim;
import de.markusbordihn.easynpc.entity.npc.ModEntityType;
import de.markusbordihn.easynpc.entity.npc.Skeleton;
import de.markusbordihn.easynpc.entity.npc.Villager;
import de.markusbordihn.easynpc.entity.npc.Zombie;
import de.markusbordihn.easynpc.entity.npc.ZombieVillager;
import de.markusbordihn.easynpc.item.configuration.EasyNPCWandItem;
import de.markusbordihn.easynpc.tabs.EasyNPCTab;

public class ModItems {

  protected ModItems() {

  }

  public static final DeferredRegister<Item> ITEMS =
      DeferredRegister.create(ForgeRegistries.ITEMS, Constants.MOD_ID);

  // Config Items
  public static final RegistryObject<Item> EASY_NPC_WAND = ITEMS.register(EasyNPCWandItem.ID,
      () -> new EasyNPCWandItem(new Item.Properties().tab(EasyNPCTab.TAB_CONFIG_ITEMS)));

  // Default NPC Entity Spawn Eggs
  private static final String SPAWN_EGG_PREFIX = "_spawn_egg";

  public static final RegistryObject<Item> CAT_NPC_SPAWN_EGG = ITEMS
      .register(Cat.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.CAT::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> CHICKEN_NPC_SPAWN_EGG = ITEMS
      .register(Chicken.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.CHICKEN::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> FAIRY_NPC_SPAWN_EGG = ITEMS
      .register(Fairy.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.FAIRY::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> HUMANOID_NPC_SPAWN_EGG = ITEMS.register(
      Humanoid.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.HUMANOID::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> HUMANOID_SLIM_NPC_SPAWN_EGG =
      ITEMS.register(HumanoidSlim.ID + SPAWN_EGG_PREFIX,
          () -> new EasyNPCSpawnEggItem(ModEntityType.HUMANOID_SLIM::get,
              new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> SKELETON_NPC_SPAWN_EGG = ITEMS.register(
      Skeleton.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.SKELETON::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> VILLAGER_NPC_SPAWN_EGG = ITEMS.register(
      Villager.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.VILLAGER::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> ZOMBIE_NPC_SPAWN_EGG = ITEMS.register(
      Zombie.ID + SPAWN_EGG_PREFIX, () -> new EasyNPCSpawnEggItem(ModEntityType.ZOMBIE::get,
          new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

  public static final RegistryObject<Item> ZOMBIE_VILLAGER_NPC_SPAWN_EGG =
      ITEMS.register(ZombieVillager.ID + SPAWN_EGG_PREFIX,
          () -> new EasyNPCSpawnEggItem(ModEntityType.ZOMBIE_VILLAGER::get,
              new Item.Properties().rarity(Rarity.EPIC).tab(EasyNPCTab.TAB_SPAWN_EGGS)));

}
