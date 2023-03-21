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
import net.minecraft.world.level.material.MaterialColor;

import net.minecraftforge.registries.RegistryObject;
import net.minecraftforge.common.ForgeSpawnEggItem;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.npc.Allay;
import de.markusbordihn.easynpc.entity.npc.Fairy;
import de.markusbordihn.easynpc.entity.npc.Humanoid;
import de.markusbordihn.easynpc.entity.npc.HumanoidSlim;
import de.markusbordihn.easynpc.entity.npc.ModEntityType;
import de.markusbordihn.easynpc.entity.npc.Skeleton;
import de.markusbordihn.easynpc.entity.npc.Villager;
import de.markusbordihn.easynpc.entity.npc.Zombie;
import de.markusbordihn.easynpc.entity.npc.ZombieVillager;
import de.markusbordihn.easynpc.item.configuration.EasyNPCWandItem;

public class ModItems {

  protected ModItems() {

  }

  public static final DeferredRegister<Item> ITEMS =
      DeferredRegister.create(ForgeRegistries.ITEMS, Constants.MOD_ID);

  // Config Items
  public static final RegistryObject<Item> EASY_NPC_WAND = ITEMS.register(EasyNPCWandItem.ID,
      () -> new EasyNPCWandItem(new Item.Properties().rarity(Rarity.EPIC)));

  // Default NPC Entity Spawn Eggs
  private static final String SPAWN_EGG_PREFIX = "_spawn_egg";

  public static final RegistryObject<Item> ALLAY_NPC_SPAWN_EGG =
      ITEMS.register(Allay.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.ALLAY::get, MaterialColor.COLOR_BLUE.col,
              MaterialColor.COLOR_CYAN.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> FAIRY_NPC_SPAWN_EGG =
      ITEMS.register(Fairy.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.FAIRY::get, MaterialColor.COLOR_RED.col,
              MaterialColor.COLOR_GREEN.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> HUMANOID_NPC_SPAWN_EGG =
      ITEMS.register(Humanoid.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.HUMANOID::get, MaterialColor.COLOR_RED.col,
              MaterialColor.COLOR_GREEN.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> HUMANOID_SLIM_NPC_SPAWN_EGG =
      ITEMS.register(HumanoidSlim.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.HUMANOID_SLIM::get, MaterialColor.COLOR_RED.col,
              MaterialColor.COLOR_PINK.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> SKELETON_NPC_SPAWN_EGG =
      ITEMS.register(Skeleton.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.SKELETON::get, MaterialColor.COLOR_BLACK.col,
              MaterialColor.COLOR_GRAY.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> VILLAGER_NPC_SPAWN_EGG =
      ITEMS.register(Villager.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.VILLAGER::get, MaterialColor.COLOR_RED.col,
              MaterialColor.COLOR_YELLOW.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> ZOMBIE_NPC_SPAWN_EGG =
      ITEMS.register(Zombie.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.ZOMBIE::get, MaterialColor.COLOR_RED.col,
              MaterialColor.COLOR_YELLOW.col, new Item.Properties().rarity(Rarity.EPIC)));

  public static final RegistryObject<Item> ZOMBIE_VILLAGER_NPC_SPAWN_EGG =
      ITEMS.register(ZombieVillager.ID + SPAWN_EGG_PREFIX,
          () -> new ForgeSpawnEggItem(ModEntityType.ZOMBIE_VILLAGER::get,
              MaterialColor.COLOR_RED.col, +MaterialColor.COLOR_YELLOW.col,
              new Item.Properties().rarity(Rarity.EPIC)));

}
