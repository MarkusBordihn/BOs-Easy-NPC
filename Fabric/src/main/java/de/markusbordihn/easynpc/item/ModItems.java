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
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.easynpc.npc.Allay;
import de.markusbordihn.easynpc.entity.easynpc.npc.Cat;
import de.markusbordihn.easynpc.entity.easynpc.npc.Chicken;
import de.markusbordihn.easynpc.entity.easynpc.npc.Fairy;
import de.markusbordihn.easynpc.entity.easynpc.npc.Humanoid;
import de.markusbordihn.easynpc.entity.easynpc.npc.HumanoidSlim;
import de.markusbordihn.easynpc.entity.easynpc.npc.IronGolem;
import de.markusbordihn.easynpc.entity.easynpc.npc.Skeleton;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetEmptyItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCWandItem;
import de.markusbordihn.easynpc.item.configuration.MoveEasyNPCItem;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Rarity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModItems {

  public static final Item EASY_NPC_WAND = new EasyNPCWandItem(new Item.Properties());
  public static final Item MOVE_EASY_NPC = new MoveEasyNPCItem(new Item.Properties());
  public static final Item EASY_NPC_PRESET_ITEM = new EasyNPCPresetItem(new Item.Properties());
  public static final Item EASY_NPC_PRESET_EMPTY_ITEM =
      new EasyNPCPresetEmptyItem(new Item.Properties());

  public static final Item ALLEY_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ALLAY, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item CAT_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.CAT, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item CHICKEN_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.CHICKEN, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item FAIRY_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.FAIRY, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HUMANOID_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HUMANOID, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HUMANOID_SLIM_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HUMANOID_SLIM, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item IRON_GOLEM_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.IRON_GOLEM, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item SKELETON_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.SKELETON, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item VILLAGER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.VILLAGER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item DROWNED_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.DROWNED, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HUSK_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HUSK, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item WITHER_SKELETON_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.WITHER_SKELETON, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item STRAY_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.STRAY, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ZOMBIE_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ZOMBIE, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ZOMBIE_VILLAGER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ZOMBIE_VILLAGER, new Item.Properties().rarity(Rarity.EPIC));
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String SPAWN_EGG_PREFIX = "_spawn_egg";

  private ModItems() {}

  public static void registerModItems() {

    log.info("{} Configuration Items ...", Constants.LOG_REGISTER_PREFIX);
    Registry.register(
        BuiltInRegistries.ITEM, Constants.MOD_ID + ":" + EasyNPCWandItem.ID, EASY_NPC_WAND);
    Registry.register(
        BuiltInRegistries.ITEM, Constants.MOD_ID + ":" + MoveEasyNPCItem.ID, MOVE_EASY_NPC);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + EasyNPCPresetItem.NAME,
        EASY_NPC_PRESET_ITEM);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + EasyNPCPresetEmptyItem.NAME,
        EASY_NPC_PRESET_EMPTY_ITEM);

    log.info("{} Spawn Egg Items ...", Constants.LOG_REGISTER_PREFIX);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Allay.ID + SPAWN_EGG_PREFIX,
        ALLEY_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Cat.ID + SPAWN_EGG_PREFIX,
        CAT_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Chicken.ID + SPAWN_EGG_PREFIX,
        CHICKEN_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Fairy.ID + SPAWN_EGG_PREFIX,
        FAIRY_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Humanoid.ID + SPAWN_EGG_PREFIX,
        HUMANOID_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + HumanoidSlim.ID + SPAWN_EGG_PREFIX,
        HUMANOID_SLIM_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + IronGolem.ID + SPAWN_EGG_PREFIX,
        IRON_GOLEM_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Skeleton.ID + SPAWN_EGG_PREFIX,
        SKELETON_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Villager.ID + SPAWN_EGG_PREFIX,
        VILLAGER_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Zombie.ID_DROWNED + SPAWN_EGG_PREFIX,
        DROWNED_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Zombie.ID_HUSK + SPAWN_EGG_PREFIX,
        HUSK_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Skeleton.ID_WITHER_SKELETON + SPAWN_EGG_PREFIX,
        WITHER_SKELETON_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Skeleton.ID_STRAY + SPAWN_EGG_PREFIX,
        STRAY_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + Zombie.ID + SPAWN_EGG_PREFIX,
        ZOMBIE_NPC_SPAWN_EGG);
    Registry.register(
        BuiltInRegistries.ITEM,
        Constants.MOD_ID + ":" + ZombieVillager.ID + SPAWN_EGG_PREFIX,
        ZOMBIE_VILLAGER_NPC_SPAWN_EGG);
  }
}
