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
import de.markusbordihn.easynpc.entity.easynpc.npc.Illager;
import de.markusbordihn.easynpc.entity.easynpc.npc.IronGolem;
import de.markusbordihn.easynpc.entity.easynpc.npc.Orc;
import de.markusbordihn.easynpc.entity.easynpc.npc.Pig;
import de.markusbordihn.easynpc.entity.easynpc.npc.Skeleton;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetEmptyItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCWandItem;
import de.markusbordihn.easynpc.item.configuration.MoveEasyNPCItem;
import net.minecraft.core.Registry;
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
  public static final Item PIG_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.PIG, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item EVOKER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.EVOKER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ILLUSIONER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ILLUSIONER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item PILLAGER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.PILLAGER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item VINDICATOR_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.VINDICATOR, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ORC_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ORC, new Item.Properties().rarity(Rarity.EPIC));

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String SPAWN_EGG_PREFIX = "_spawn_egg";

  private ModItems() {
  }

  public static void registerModItems() {

    log.info("{} Configuration Items ...", Constants.LOG_REGISTER_PREFIX);
    Registry.register(Registry.ITEM, Constants.MOD_ID + ":" + EasyNPCWandItem.ID, EASY_NPC_WAND);
    Registry.register(Registry.ITEM, Constants.MOD_ID + ":" + MoveEasyNPCItem.ID, MOVE_EASY_NPC);
    Registry.register(
        Registry.ITEM, Constants.MOD_ID + ":" + EasyNPCPresetItem.NAME, EASY_NPC_PRESET_ITEM);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + EasyNPCPresetEmptyItem.NAME,
        EASY_NPC_PRESET_EMPTY_ITEM);

    log.info("{} Spawn Egg Items ...", Constants.LOG_REGISTER_PREFIX);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Allay.ID + ModSpawnEggItem.SUFFIX,
        ALLEY_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM, Constants.MOD_ID + ":" + Cat.ID + ModSpawnEggItem.SUFFIX, CAT_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Chicken.ID + ModSpawnEggItem.SUFFIX,
        CHICKEN_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Fairy.ID + ModSpawnEggItem.SUFFIX,
        FAIRY_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Humanoid.ID + ModSpawnEggItem.SUFFIX,
        HUMANOID_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + HumanoidSlim.ID + ModSpawnEggItem.SUFFIX,
        HUMANOID_SLIM_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + IronGolem.ID + ModSpawnEggItem.SUFFIX,
        IRON_GOLEM_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Skeleton.ID + ModSpawnEggItem.SUFFIX,
        SKELETON_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Villager.ID + ModSpawnEggItem.SUFFIX,
        VILLAGER_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Zombie.ID_DROWNED + ModSpawnEggItem.SUFFIX,
        DROWNED_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Zombie.ID_HUSK + ModSpawnEggItem.SUFFIX,
        HUSK_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Skeleton.ID_WITHER_SKELETON + ModSpawnEggItem.SUFFIX,
        WITHER_SKELETON_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Skeleton.ID_STRAY + ModSpawnEggItem.SUFFIX,
        STRAY_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Zombie.ID + ModSpawnEggItem.SUFFIX,
        ZOMBIE_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + ZombieVillager.ID + ModSpawnEggItem.SUFFIX,
        ZOMBIE_VILLAGER_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM, Constants.MOD_ID + ":" + Pig.ID + ModSpawnEggItem.SUFFIX, PIG_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Illager.ID_EVOKER + ModSpawnEggItem.SUFFIX,
        EVOKER_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Illager.ID_ILLUSIONER + ModSpawnEggItem.SUFFIX,
        ILLUSIONER_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Illager.ID_PILLAGER + ModSpawnEggItem.SUFFIX,
        PILLAGER_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM,
        Constants.MOD_ID + ":" + Illager.ID_VINDICATOR + ModSpawnEggItem.SUFFIX,
        VINDICATOR_NPC_SPAWN_EGG);
    Registry.register(
        Registry.ITEM, Constants.MOD_ID + ":" + Orc.ID + ModSpawnEggItem.SUFFIX, ORC_NPC_SPAWN_EGG);
  }
}
