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

package de.markusbordihn.easynpc.item;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.block.EasyNPCSpawnerBlock;
import de.markusbordihn.easynpc.block.ModBlocks;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.easynpc.npc.Allay;
import de.markusbordihn.easynpc.entity.easynpc.npc.Cat;
import de.markusbordihn.easynpc.entity.easynpc.npc.Chicken;
import de.markusbordihn.easynpc.entity.easynpc.npc.Fairy;
import de.markusbordihn.easynpc.entity.easynpc.npc.Humanoid;
import de.markusbordihn.easynpc.entity.easynpc.npc.HumanoidSlim;
import de.markusbordihn.easynpc.entity.easynpc.npc.Illager;
import de.markusbordihn.easynpc.entity.easynpc.npc.IronGolem;
import de.markusbordihn.easynpc.entity.easynpc.npc.Pig;
import de.markusbordihn.easynpc.entity.easynpc.npc.Skeleton;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetEmptyItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCWandItem;
import de.markusbordihn.easynpc.item.configuration.MoveEasyNPCItem;
import de.markusbordihn.easynpc.tabs.ModTabs;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Rarity;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

public class ModItems {

  public static final DeferredRegister<Item> ITEMS =
      DeferredRegister.create(ForgeRegistries.ITEMS, Constants.MOD_ID);

  public static final RegistryObject<Item> EASY_NPC_WAND =
      ITEMS.register(
          EasyNPCWandItem.ID,
          () -> new EasyNPCWandItem(new Item.Properties().tab(ModTabs.TAB_CONFIG_ITEMS)));
  public static final RegistryObject<Item> MOVE_EASY_NPC =
      ITEMS.register(
          MoveEasyNPCItem.ID,
          () -> new MoveEasyNPCItem(new Item.Properties().tab(ModTabs.TAB_CONFIG_ITEMS)));

  public static final RegistryObject<Item> EASY_NPC_PRESET_ITEM =
      ITEMS.register(EasyNPCPresetItem.NAME, () -> new EasyNPCPresetItem(new Item.Properties()));

  public static final RegistryObject<Item> EASY_NPC_PRESET_EMPTY_ITEM =
      ITEMS.register(
          EasyNPCPresetEmptyItem.NAME,
          () -> new EasyNPCPresetEmptyItem(new Item.Properties().tab(ModTabs.TAB_CONFIG_ITEMS)));

  public static final RegistryObject<Item> EASY_NPC_SPAWNER =
      ITEMS.register(
          EasyNPCSpawnerBlock.NAME,
          () ->
              new BlockItem(
                  ModBlocks.EASY_NPC_SPAWNER.get(),
                  new Item.Properties().tab(ModTabs.TAB_CONFIG_ITEMS)));

  public static final RegistryObject<Item> ALLAY_NPC_SPAWN_EGG =
      ITEMS.register(
          Allay.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.ALLAY,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> CAT_NPC_SPAWN_EGG =
      ITEMS.register(
          Cat.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.CAT,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> CHICKEN_NPC_SPAWN_EGG =
      ITEMS.register(
          Chicken.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.CHICKEN,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> FAIRY_NPC_SPAWN_EGG =
      ITEMS.register(
          Fairy.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.FAIRY,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> HUMANOID_NPC_SPAWN_EGG =
      ITEMS.register(
          Humanoid.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.HUMANOID,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> HUMANOID_SLIM_NPC_SPAWN_EGG =
      ITEMS.register(
          HumanoidSlim.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.HUMANOID_SLIM,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> IRON_GOLEM_NPC_SPAWN_EGG =
      ITEMS.register(
          IronGolem.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.IRON_GOLEM,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> SKELETON_NPC_SPAWN_EGG =
      ITEMS.register(
          Skeleton.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.SKELETON,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> VILLAGER_NPC_SPAWN_EGG =
      ITEMS.register(
          Villager.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.VILLAGER,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> DROWNED_NPC_SPAWN_EGG =
      ITEMS.register(
          Zombie.ID_DROWNED + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.DROWNED,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> HUSK_NPC_SPAWN_EGG =
      ITEMS.register(
          Zombie.ID_HUSK + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.HUSK,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> WITHER_SKELETON_NPC_SPAWN_EGG =
      ITEMS.register(
          Skeleton.ID_WITHER_SKELETON + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.WITHER_SKELETON,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> STRAY_NPC_SPAWN_EGG =
      ITEMS.register(
          Skeleton.ID_STRAY + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.STRAY,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> ZOMBIE_NPC_SPAWN_EGG =
      ITEMS.register(
          Zombie.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.ZOMBIE,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> ZOMBIE_VILLAGER_NPC_SPAWN_EGG =
      ITEMS.register(
          ZombieVillager.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.ZOMBIE_VILLAGER,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> PIG_NPC_SPAWN_EGG =
      ITEMS.register(
          Pig.ID + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.PIG,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> EVOKER_NPC_SPAWN_EGG =
      ITEMS.register(
          Illager.ID_EVOKER + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.EVOKER,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> ILLUSIONER_NPC_SPAWN_EGG =
      ITEMS.register(
          Illager.ID_ILLUSIONER + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.ILLUSIONER,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> PILLAGER_NPC_SPAWN_EGG =
      ITEMS.register(
          Illager.ID_PILLAGER + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.PILLAGER,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));
  public static final RegistryObject<Item> VINDICATOR_NPC_SPAWN_EGG =
      ITEMS.register(
          Illager.ID_VINDICATOR + ModSpawnEggItem.SUFFIX,
          () ->
              new ModSpawnEggItem(
                  ModEntityType.VINDICATOR,
                  new Item.Properties().rarity(Rarity.EPIC).tab(ModTabs.TAB_SPAWN_EGGS)));

  protected ModItems() {
  }
}
