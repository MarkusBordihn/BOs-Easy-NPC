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
import de.markusbordihn.easynpc.block.ModBlocks;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import de.markusbordihn.easynpc.entity.EpicFightEntityType;
import de.markusbordihn.easynpc.entity.ModCustomEntityType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import de.markusbordihn.easynpc.item.attack.BulletItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetEmptyItem;
import de.markusbordihn.easynpc.item.configuration.EasyNPCPresetItem;
import de.markusbordihn.easynpc.item.configuration.MoveEasyNPCItem;
import java.util.EnumMap;
import java.util.Map;
import java.util.function.Supplier;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Rarity;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModItems {

  public static final DeferredRegister<Item> ITEMS =
      DeferredRegister.create(ForgeRegistries.ITEMS, Constants.MOD_ID);
  public static final Map<ModNPCEntityType, RegistryObject<Item>> NPC_SPAWN_EGGS =
      new EnumMap<>(ModNPCEntityType.class);
  public static final Map<ModCustomEntityType, RegistryObject<Item>> CUSTOM_NPC_SPAWN_EGGS =
      new EnumMap<>(ModCustomEntityType.class);
  public static final Map<EpicFightEntityType, RegistryObject<Item>> EPIC_FIGHT_SPAWN_EGGS =
      new EnumMap<>(EpicFightEntityType.class);
  public static final RegistryObject<Item> BULLET_ITEM =
      ITEMS.register(BulletItem.ID, () -> new BulletItem(new Item.Properties()));
  public static final RegistryObject<Item> EASY_NPC_PRESET_EMPTY_ITEM =
      ITEMS.register(
          EasyNPCPresetEmptyItem.NAME, () -> new EasyNPCPresetEmptyItem(new Item.Properties()));
  public static final RegistryObject<Item> EASY_NPC_PRESET_ITEM =
      ITEMS.register(EasyNPCPresetItem.NAME, () -> new EasyNPCPresetItem(new Item.Properties()));
  public static final RegistryObject<Item> EASY_NPC_SPAWNER_BOSS =
      ITEMS.register(
          SpawnerType.BOSS_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockItem(
                  ModBlocks.EASY_NPC_SPAWNER_BOSS.get(),
                  new Item.Properties(),
                  SpawnerType.BOSS_SPAWNER));
  public static final RegistryObject<Item> EASY_NPC_SPAWNER_DEFAULT =
      ITEMS.register(
          SpawnerType.DEFAULT_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockItem(
                  ModBlocks.EASY_NPC_SPAWNER_DEFAULT.get(),
                  new Item.Properties(),
                  SpawnerType.DEFAULT_SPAWNER));
  public static final RegistryObject<Item> EASY_NPC_SPAWNER_GROUP =
      ITEMS.register(
          SpawnerType.GROUP_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockItem(
                  ModBlocks.EASY_NPC_SPAWNER_GROUP.get(),
                  new Item.Properties(),
                  SpawnerType.GROUP_SPAWNER));
  public static final RegistryObject<Item> EASY_NPC_SPAWNER_SINGLE =
      ITEMS.register(
          SpawnerType.SINGLE_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockItem(
                  ModBlocks.EASY_NPC_SPAWNER_SINGLE.get(),
                  new Item.Properties(),
                  SpawnerType.SINGLE_SPAWNER));
  public static final RegistryObject<Item> MOVE_EASY_NPC =
      ITEMS.register(MoveEasyNPCItem.ID, () -> new MoveEasyNPCItem(new Item.Properties()));
  public static final RegistryObject<Item> CUSTOM_DATA_TEST_ITEM =
      ITEMS.register(
          CustomDataTestItem.ID, () -> new CustomDataTestItem(new Item.Properties().stacksTo(1)));
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    // Register spawn eggs for all NPC entity types.
    for (ModNPCEntityType entityType : ModNPCEntityType.values()) {
      RegistryObject<EntityType<?>> entityTypeObject = ModEntityType.NPC_TYPE.get(entityType);
      if (entityTypeObject == null) {
        log.error("Unable to register NPC spawn egg with id {}.", entityType.getId());
        continue;
      }
      log.info(
          "Registering NPC spawn egg for {} with id {}.", entityTypeObject, entityType.getId());
      NPC_SPAWN_EGGS.put(entityType, registerSpawnEgg(entityType.getId(), entityTypeObject));
    }

    // Register spawn eggs for all custom entity types.
    for (ModCustomEntityType entityType : ModCustomEntityType.values()) {
      RegistryObject<EntityType<?>> entityTypeObject = ModEntityType.CUSTOM_TYPE.get(entityType);
      if (entityTypeObject == null) {
        log.error("Unable to register custom spawn egg with id {}.", entityType.getId());
        continue;
      }
      log.info(
          "Registering custom spawn egg for {} with id {}.", entityTypeObject, entityType.getId());
      CUSTOM_NPC_SPAWN_EGGS.put(entityType, registerSpawnEgg(entityType.getId(), entityTypeObject));
    }

    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      for (EpicFightEntityType entityType : EpicFightEntityType.values()) {
        RegistryObject<EntityType<?>> entityTypeObject =
            ModEntityType.EPIC_FIGHT_TYPE.get(entityType);
        if (entityTypeObject == null) {
          log.error("Unable to register Epic Fight spawn egg with id {}.", entityType.getId());
          continue;
        }
        log.info(
            "Registering Epic Fight spawn egg for {} with id {}.",
            entityTypeObject,
            entityType.getId());
        EPIC_FIGHT_SPAWN_EGGS.put(
            entityType, registerEpicFightSpawnEgg(entityType.getId(), entityTypeObject));
      }
    }
  }

  private ModItems() {}

  private static RegistryObject<Item> registerEpicFightSpawnEgg(
      String id, Supplier<? extends EntityType<?>> entityTypeSupplier) {
    String spawnEggId = id + ModSpawnEggItem.SUFFIX;
    return ITEMS.register(
        spawnEggId,
        () ->
            new ModEpicFightSpawnEggItem(
                (Supplier<? extends EntityType<? extends Mob>>) entityTypeSupplier,
                new Item.Properties().rarity(Rarity.EPIC)));
  }

  private static RegistryObject<Item> registerSpawnEgg(
      String id, Supplier<? extends EntityType<?>> entityTypeSupplier) {
    String spawnEggId = id + ModSpawnEggItem.SUFFIX;
    return ITEMS.register(
        spawnEggId,
        () ->
            new ModSpawnEggItem(
                (Supplier<? extends EntityType<? extends Mob>>) entityTypeSupplier,
                new Item.Properties().rarity(Rarity.EPIC)));
  }
}
