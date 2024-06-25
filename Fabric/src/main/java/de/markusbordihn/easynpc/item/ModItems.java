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
import de.markusbordihn.easynpc.entity.easynpc.npc.Horse;
import de.markusbordihn.easynpc.entity.easynpc.npc.Humanoid;
import de.markusbordihn.easynpc.entity.easynpc.npc.HumanoidSlim;
import de.markusbordihn.easynpc.entity.easynpc.npc.Illager;
import de.markusbordihn.easynpc.entity.easynpc.npc.IronGolem;
import de.markusbordihn.easynpc.entity.easynpc.npc.Orc;
import de.markusbordihn.easynpc.entity.easynpc.npc.Pig;
import de.markusbordihn.easynpc.entity.easynpc.npc.Skeleton;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Wolf;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager;
import de.markusbordihn.easynpc.item.attack.BulletItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetEmptyItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCWandItem;
import de.markusbordihn.easynpc.item.configuration.MoveEasyNPCItem;
import net.minecraft.core.Registry;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Rarity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModItems {

  public static final Item ALLEY_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ALLAY, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item BULLET_ITEM = new Item(new Item.Properties());
  public static final Item CAT_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.CAT, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item CHICKEN_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.CHICKEN, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item DROWNED_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.DROWNED, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item EASY_NPC_PRESET_EMPTY_ITEM =
      new EasyNPCPresetEmptyItem(new Item.Properties());
  public static final Item EASY_NPC_PRESET_ITEM = new EasyNPCPresetItem(new Item.Properties());
  public static final Item EASY_NPC_SPAWNER =
      new BlockItem(ModBlocks.EASY_NPC_SPAWNER, new Item.Properties());
  public static final Item EASY_NPC_WAND = new EasyNPCWandItem(new Item.Properties());
  public static final Item EVOKER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.EVOKER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item FAIRY_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.FAIRY, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HORSE_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HORSE, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HUMANOID_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HUMANOID, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HUMANOID_SLIM_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HUMANOID_SLIM, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item HUSK_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.HUSK, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ILLUSIONER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ILLUSIONER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item IRON_GOLEM_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.IRON_GOLEM, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item MOVE_EASY_NPC = new MoveEasyNPCItem(new Item.Properties());
  public static final Item ORC_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ORC, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ORC_WARRIOR_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ORC_WARRIOR, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item PIG_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.PIG, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item PILLAGER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.PILLAGER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item SKELETON_HORSE_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.SKELETON_HORSE, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item SKELETON_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.SKELETON, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item STRAY_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.STRAY, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item VILLAGER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.VILLAGER, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item VINDICATOR_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.VINDICATOR, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item WITHER_SKELETON_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.WITHER_SKELETON, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item WOLF_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.WOLF, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ZOMBIE_HORSE_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ZOMBIE_HORSE, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ZOMBIE_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ZOMBIE, new Item.Properties().rarity(Rarity.EPIC));
  public static final Item ZOMBIE_VILLAGER_NPC_SPAWN_EGG =
      new ModSpawnEggItem(ModEntityType.ZOMBIE_VILLAGER, new Item.Properties().rarity(Rarity.EPIC));

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModItems() {}

  public static void registerModItems() {

    log.info("{} Configuration Items ...", Constants.LOG_REGISTER_PREFIX);
    registerItem(EasyNPCWandItem.ID, EASY_NPC_WAND);
    registerItem(MoveEasyNPCItem.ID, MOVE_EASY_NPC);
    registerItem(EasyNPCPresetItem.NAME, EASY_NPC_PRESET_ITEM);
    registerItem(EasyNPCPresetEmptyItem.NAME, EASY_NPC_PRESET_EMPTY_ITEM);

    log.info("{} Block Items ...", Constants.LOG_REGISTER_PREFIX);
    registerItem(EasyNPCSpawnerBlock.NAME, EASY_NPC_SPAWNER);

    log.info("{} Weapon Items ...", Constants.LOG_REGISTER_PREFIX);
    registerItem(BulletItem.ID, BULLET_ITEM);

    log.info("{} Spawn Egg Items ...", Constants.LOG_REGISTER_PREFIX);
    registerSpawnEgg(Allay.ID, ALLEY_NPC_SPAWN_EGG);
    registerSpawnEgg(Cat.ID, CAT_NPC_SPAWN_EGG);
    registerSpawnEgg(Chicken.ID, CHICKEN_NPC_SPAWN_EGG);
    registerSpawnEgg(Fairy.ID, FAIRY_NPC_SPAWN_EGG);
    registerSpawnEgg(Horse.ID, HORSE_NPC_SPAWN_EGG);
    registerSpawnEgg(Horse.ID_SKELETON, SKELETON_HORSE_NPC_SPAWN_EGG);
    registerSpawnEgg(Horse.ID_ZOMBIE, ZOMBIE_HORSE_NPC_SPAWN_EGG);
    registerSpawnEgg(Humanoid.ID, HUMANOID_NPC_SPAWN_EGG);
    registerSpawnEgg(HumanoidSlim.ID, HUMANOID_SLIM_NPC_SPAWN_EGG);
    registerSpawnEgg(Illager.ID_EVOKER, EVOKER_NPC_SPAWN_EGG);
    registerSpawnEgg(Illager.ID_ILLUSIONER, ILLUSIONER_NPC_SPAWN_EGG);
    registerSpawnEgg(Illager.ID_PILLAGER, PILLAGER_NPC_SPAWN_EGG);
    registerSpawnEgg(Illager.ID_VINDICATOR, VINDICATOR_NPC_SPAWN_EGG);
    registerSpawnEgg(IronGolem.ID, IRON_GOLEM_NPC_SPAWN_EGG);
    registerSpawnEgg(Orc.ID, ORC_NPC_SPAWN_EGG);
    registerSpawnEgg(Orc.ID_WARRIOR, ORC_WARRIOR_NPC_SPAWN_EGG);
    registerSpawnEgg(Pig.ID, PIG_NPC_SPAWN_EGG);
    registerSpawnEgg(Skeleton.ID, SKELETON_NPC_SPAWN_EGG);
    registerSpawnEgg(Skeleton.ID_STRAY, STRAY_NPC_SPAWN_EGG);
    registerSpawnEgg(Skeleton.ID_WITHER_SKELETON, WITHER_SKELETON_NPC_SPAWN_EGG);
    registerSpawnEgg(Villager.ID, VILLAGER_NPC_SPAWN_EGG);
    registerSpawnEgg(Wolf.ID, WOLF_NPC_SPAWN_EGG);
    registerSpawnEgg(Zombie.ID, ZOMBIE_NPC_SPAWN_EGG);
    registerSpawnEgg(Zombie.ID_DROWNED, DROWNED_NPC_SPAWN_EGG);
    registerSpawnEgg(Zombie.ID_HUSK, HUSK_NPC_SPAWN_EGG);
    registerSpawnEgg(ZombieVillager.ID, ZOMBIE_VILLAGER_NPC_SPAWN_EGG);
  }

  private static void registerItem(String id, Item item) {
    Registry.register(Registry.ITEM, Constants.MOD_ID + ":" + id, item);
  }

  private static void registerSpawnEgg(String id, Item item) {
    registerItem(id + ModSpawnEggItem.SUFFIX, item);
  }
}
