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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.epicfight.entity.EpicFightEntityTypes;
import de.markusbordihn.easynpc.compat.epicfight.entity.EpicFightZombie;
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
import de.markusbordihn.easynpc.entity.easynpc.npc.Piglin;
import de.markusbordihn.easynpc.entity.easynpc.npc.Skeleton;
import de.markusbordihn.easynpc.entity.easynpc.npc.Villager;
import de.markusbordihn.easynpc.entity.easynpc.npc.Wolf;
import de.markusbordihn.easynpc.entity.easynpc.npc.Zombie;
import de.markusbordihn.easynpc.entity.easynpc.npc.ZombieVillager;
import de.markusbordihn.easynpc.entity.easynpc.raw.ZombieRaw;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricDefaultAttributeRegistry;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModEntityType {

  public static final EntityType<Allay> ALLAY =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Allay.ID, ModEntityTypes.ALLAY);
  public static final EntityType<Skeleton> BOGGED =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Skeleton.ID_BOGGED,
          ModEntityTypes.BOGGED);
  public static final EntityType<Cat> CAT =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Cat.ID, ModEntityTypes.CAT);
  public static final EntityType<Chicken> CHICKEN =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Chicken.ID,
          ModEntityTypes.CHICKEN);
  public static final EntityType<Zombie> DROWNED =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Zombie.ID_DROWNED,
          ModEntityTypes.DROWNED);
  public static final EntityType<Illager> EVOKER =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Illager.ID_EVOKER,
          ModEntityTypes.EVOKER);
  public static final EntityType<Fairy> FAIRY =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Fairy.ID, ModEntityTypes.FAIRY);
  public static final EntityType<Horse> HORSE =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Horse.ID, ModEntityTypes.HORSE);
  public static final EntityType<Humanoid> HUMANOID =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Humanoid.ID,
          ModEntityTypes.HUMANOID);
  public static final EntityType<HumanoidSlim> HUMANOID_SLIM =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + HumanoidSlim.ID,
          ModEntityTypes.HUMANOID_SLIM);
  public static final EntityType<Zombie> HUSK =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Zombie.ID_HUSK,
          ModEntityTypes.HUSK);
  public static final EntityType<Illager> ILLUSIONER =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Illager.ID_ILLUSIONER,
          ModEntityTypes.ILLUSIONER);
  public static final EntityType<IronGolem> IRON_GOLEM =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + IronGolem.ID,
          ModEntityTypes.IRON_GOLEM);
  public static final EntityType<Orc> ORC =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Orc.ID, ModEntityTypes.ORC);
  public static final EntityType<Orc> ORC_WARRIOR =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Orc.ID_WARRIOR,
          ModEntityTypes.ORC_WARRIOR);
  public static final EntityType<Pig> PIG =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Pig.ID, ModEntityTypes.PIG);
  public static final EntityType<Piglin> PIGLIN =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Piglin.ID, ModEntityTypes.PIGLIN);
  public static final EntityType<Piglin> PIGLIN_BRUTE =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Piglin.ID_BRUTE,
          ModEntityTypes.PIGLIN_BRUTE);
  public static final EntityType<Piglin> PIGLIN_ZOMBIFIED =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Piglin.ID_ZOMBIFIED,
          ModEntityTypes.PIGLIN_ZOMBIFIED);
  public static final EntityType<Illager> PILLAGER =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Illager.ID_PILLAGER,
          ModEntityTypes.PILLAGER);
  public static final EntityType<Skeleton> SKELETON =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Skeleton.ID,
          ModEntityTypes.SKELETON);
  public static final EntityType<Horse> SKELETON_HORSE =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Horse.ID_SKELETON,
          ModEntityTypes.SKELETON_HORSE);
  public static final EntityType<Skeleton> STRAY =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Skeleton.ID_STRAY,
          ModEntityTypes.STRAY);
  public static final EntityType<Villager> VILLAGER =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Villager.ID,
          ModEntityTypes.VILLAGER);
  public static final EntityType<Illager> VINDICATOR =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Illager.ID_VINDICATOR,
          ModEntityTypes.VINDICATOR);
  public static final EntityType<Skeleton> WITHER_SKELETON =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Skeleton.ID_WITHER_SKELETON,
          ModEntityTypes.WITHER_SKELETON);
  public static final EntityType<Wolf> WOLF =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Wolf.ID, ModEntityTypes.WOLF);
  public static final EntityType<Zombie> ZOMBIE =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID + ":" + Zombie.ID, ModEntityTypes.ZOMBIE);
  public static final EntityType<Horse> ZOMBIE_HORSE =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + Horse.ID_ZOMBIE,
          ModEntityTypes.ZOMBIE_HORSE);
  public static final EntityType<ZombieVillager> ZOMBIE_VILLAGER =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + ZombieVillager.ID,
          ModEntityTypes.ZOMBIE_VILLAGER);
  // Raw entities (for modding only)
  public static final EntityType<ZombieRaw> ZOMBIE_RAW =
      Registry.register(
          BuiltInRegistries.ENTITY_TYPE,
          Constants.MOD_ID + ":" + ZombieRaw.ID,
          ModEntityTypes.ZOMBIE_RAW);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Optional: Epic Fight entities
  public static EntityType<EpicFightZombie> EPIC_FIGHT_ZOMBIE;

  static {
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      EPIC_FIGHT_ZOMBIE =
          Registry.register(
              BuiltInRegistries.ENTITY_TYPE, EpicFightZombie.ID, EpicFightEntityTypes.ZOMBIE);
    }
  }

  private ModEntityType() {}

  public static void registerEntitiesAttributes() {
    log.info("{} Registering Entities Attributes ...", Constants.LOG_REGISTER_PREFIX);

    FabricDefaultAttributeRegistry.register(ALLAY, Allay.createAttributes());
    FabricDefaultAttributeRegistry.register(BOGGED, Skeleton.createAttributes());
    FabricDefaultAttributeRegistry.register(CAT, Cat.createAttributes());
    FabricDefaultAttributeRegistry.register(CHICKEN, Chicken.createAttributes());
    FabricDefaultAttributeRegistry.register(DROWNED, Zombie.createAttributes());
    FabricDefaultAttributeRegistry.register(EVOKER, Illager.createAttributes());
    FabricDefaultAttributeRegistry.register(FAIRY, Fairy.createAttributes());
    FabricDefaultAttributeRegistry.register(HORSE, Horse.createAttributes());
    FabricDefaultAttributeRegistry.register(HUMANOID, Humanoid.createAttributes());
    FabricDefaultAttributeRegistry.register(HUMANOID_SLIM, HumanoidSlim.createAttributes());
    FabricDefaultAttributeRegistry.register(HUSK, Zombie.createAttributes());
    FabricDefaultAttributeRegistry.register(ILLUSIONER, Illager.createAttributes());
    FabricDefaultAttributeRegistry.register(IRON_GOLEM, IronGolem.createAttributes());
    FabricDefaultAttributeRegistry.register(ORC, Orc.createAttributes());
    FabricDefaultAttributeRegistry.register(ORC_WARRIOR, Orc.createAttributes());
    FabricDefaultAttributeRegistry.register(PIG, Pig.createAttributes());
    FabricDefaultAttributeRegistry.register(PIGLIN, Piglin.createAttributes());
    FabricDefaultAttributeRegistry.register(PIGLIN_BRUTE, Piglin.createAttributes());
    FabricDefaultAttributeRegistry.register(PIGLIN_ZOMBIFIED, Piglin.createAttributes());
    FabricDefaultAttributeRegistry.register(PILLAGER, Illager.createAttributes());
    FabricDefaultAttributeRegistry.register(SKELETON, Skeleton.createAttributes());
    FabricDefaultAttributeRegistry.register(SKELETON_HORSE, Horse.createAttributes());
    FabricDefaultAttributeRegistry.register(STRAY, Skeleton.createAttributes());
    FabricDefaultAttributeRegistry.register(VILLAGER, Villager.createAttributes());
    FabricDefaultAttributeRegistry.register(VINDICATOR, Illager.createAttributes());
    FabricDefaultAttributeRegistry.register(WITHER_SKELETON, Skeleton.createAttributes());
    FabricDefaultAttributeRegistry.register(WOLF, Wolf.createAttributes());
    FabricDefaultAttributeRegistry.register(ZOMBIE, Zombie.createAttributes());
    FabricDefaultAttributeRegistry.register(ZOMBIE_HORSE, Horse.createAttributes());
    FabricDefaultAttributeRegistry.register(ZOMBIE_VILLAGER, ZombieVillager.createAttributes());

    // Raw entities (for modding only)
    FabricDefaultAttributeRegistry.register(
        ZOMBIE_RAW, net.minecraft.world.entity.monster.Zombie.createAttributes());

    // Optional: Epic Fight entities
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      FabricDefaultAttributeRegistry.register(
          EPIC_FIGHT_ZOMBIE, net.minecraft.world.entity.monster.Zombie.createAttributes());
    }
  }
}
