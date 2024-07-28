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
import de.markusbordihn.easynpc.block.BaseEasyNPCSpawnerBlock;
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
import java.util.function.Supplier;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Rarity;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

public class ModItems {

  public static final DeferredRegister<Item> ITEMS =
      DeferredRegister.create(ForgeRegistries.ITEMS, Constants.MOD_ID);

  public static final RegistryObject<Item> ALLAY_NPC_SPAWN_EGG =
      registerSpawnEgg(Allay.ID, ModEntityType.ALLAY);
  public static final RegistryObject<Item> BULLET_ITEM =
      ITEMS.register(BulletItem.ID, () -> new BulletItem(new Item.Properties()));
  public static final RegistryObject<Item> BOGGED_NPC_SPAWN_EGG =
      registerSpawnEgg(Skeleton.ID_BOGGED, ModEntityType.BOGGED);
  public static final RegistryObject<Item> CAT_NPC_SPAWN_EGG =
      registerSpawnEgg(Cat.ID, ModEntityType.CAT);
  public static final RegistryObject<Item> CHICKEN_NPC_SPAWN_EGG =
      registerSpawnEgg(Chicken.ID, ModEntityType.CHICKEN);
  public static final RegistryObject<Item> DROWNED_NPC_SPAWN_EGG =
      registerSpawnEgg(Zombie.ID_DROWNED, ModEntityType.DROWNED);
  public static final RegistryObject<Item> EASY_NPC_PRESET_EMPTY_ITEM =
      ITEMS.register(
          EasyNPCPresetEmptyItem.NAME, () -> new EasyNPCPresetEmptyItem(new Item.Properties()));
  public static final RegistryObject<Item> EASY_NPC_PRESET_ITEM =
      ITEMS.register(EasyNPCPresetItem.NAME, () -> new EasyNPCPresetItem(new Item.Properties()));
  public static final RegistryObject<Item> EASY_NPC_SPAWNER =
      ITEMS.register(
          BaseEasyNPCSpawnerBlock.NAME,
          () -> new BlockItem(ModBlocks.EASY_NPC_SPAWNER.get(), new Item.Properties()));
  public static final RegistryObject<Item> EASY_NPC_WAND =
      ITEMS.register(EasyNPCWandItem.ID, () -> new EasyNPCWandItem(new Item.Properties()));
  public static final RegistryObject<Item> EVOKER_NPC_SPAWN_EGG =
      registerSpawnEgg(Illager.ID_EVOKER, ModEntityType.EVOKER);
  public static final RegistryObject<Item> FAIRY_NPC_SPAWN_EGG =
      registerSpawnEgg(Fairy.ID, ModEntityType.FAIRY);
  public static final RegistryObject<Item> HORSE_NPC_SPAWN_EGG =
      registerSpawnEgg(Horse.ID, ModEntityType.HORSE);
  public static final RegistryObject<Item> HUMANOID_NPC_SPAWN_EGG =
      registerSpawnEgg(Humanoid.ID, ModEntityType.HUMANOID);
  public static final RegistryObject<Item> HUMANOID_SLIM_NPC_SPAWN_EGG =
      registerSpawnEgg(HumanoidSlim.ID, ModEntityType.HUMANOID_SLIM);
  public static final RegistryObject<Item> HUSK_NPC_SPAWN_EGG =
      registerSpawnEgg(Zombie.ID_HUSK, ModEntityType.HUSK);
  public static final RegistryObject<Item> ILLUSIONER_NPC_SPAWN_EGG =
      registerSpawnEgg(Illager.ID_ILLUSIONER, ModEntityType.ILLUSIONER);
  public static final RegistryObject<Item> IRON_GOLEM_NPC_SPAWN_EGG =
      registerSpawnEgg(IronGolem.ID, ModEntityType.IRON_GOLEM);
  public static final RegistryObject<Item> MOVE_EASY_NPC =
      ITEMS.register(MoveEasyNPCItem.ID, () -> new MoveEasyNPCItem(new Item.Properties()));
  public static final RegistryObject<Item> ORC_NPC_SPAWN_EGG =
      registerSpawnEgg(Orc.ID, ModEntityType.ORC);
  public static final RegistryObject<Item> ORC_WARRIOR_NPC_SPAWN_EGG =
      registerSpawnEgg(Orc.ID_WARRIOR, ModEntityType.ORC_WARRIOR);
  public static final RegistryObject<Item> PIG_NPC_SPAWN_EGG =
      registerSpawnEgg(Pig.ID, ModEntityType.PIG);
  public static final RegistryObject<Item> PILLAGER_NPC_SPAWN_EGG =
      registerSpawnEgg(Illager.ID_PILLAGER, ModEntityType.PILLAGER);
  public static final RegistryObject<Item> SKELETON_HORSE_NPC_SPAWN_EGG =
      registerSpawnEgg(Horse.ID_SKELETON, ModEntityType.SKELETON_HORSE);
  public static final RegistryObject<Item> SKELETON_NPC_SPAWN_EGG =
      registerSpawnEgg(Skeleton.ID, ModEntityType.SKELETON);
  public static final RegistryObject<Item> STRAY_NPC_SPAWN_EGG =
      registerSpawnEgg(Skeleton.ID_STRAY, ModEntityType.STRAY);
  public static final RegistryObject<Item> VILLAGER_NPC_SPAWN_EGG =
      registerSpawnEgg(Villager.ID, ModEntityType.VILLAGER);
  public static final RegistryObject<Item> VINDICATOR_NPC_SPAWN_EGG =
      registerSpawnEgg(Illager.ID_VINDICATOR, ModEntityType.VINDICATOR);
  public static final RegistryObject<Item> WITHER_SKELETON_NPC_SPAWN_EGG =
      registerSpawnEgg(Skeleton.ID_WITHER_SKELETON, ModEntityType.WITHER_SKELETON);
  public static final RegistryObject<Item> WOLF_NPC_SPAWN_EGG =
      registerSpawnEgg(Wolf.ID, ModEntityType.WOLF);
  public static final RegistryObject<Item> ZOMBIE_HORSE_NPC_SPAWN_EGG =
      registerSpawnEgg(Horse.ID_ZOMBIE, ModEntityType.ZOMBIE_HORSE);
  public static final RegistryObject<Item> ZOMBIE_NPC_SPAWN_EGG =
      registerSpawnEgg(Zombie.ID, ModEntityType.ZOMBIE);
  public static final RegistryObject<Item> ZOMBIE_VILLAGER_NPC_SPAWN_EGG =
      registerSpawnEgg(ZombieVillager.ID, ModEntityType.ZOMBIE_VILLAGER);

  private ModItems() {}

  private static RegistryObject<Item> registerSpawnEgg(
      String id, Supplier<? extends EntityType<? extends Mob>> entityTypeSupplier) {
    return ITEMS.register(
        id + ModSpawnEggItem.SUFFIX,
        () -> new ModSpawnEggItem(entityTypeSupplier, new Item.Properties().rarity(Rarity.EPIC)));
  }
}
