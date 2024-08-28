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
import java.util.HashSet;
import java.util.Set;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityType.EntityFactory;
import net.minecraft.world.entity.MobCategory;

public class ModEntityTypes {

  private static final Set<EntityType<?>> REGISTERED_ENTITY_TYPES = new HashSet<>();

  public static final EntityType<Allay> ALLAY =
      registerEntityType(Allay::new, Allay.ID, 0.6F, 0.90F);
  public static final EntityType<Cat> CAT = registerEntityType(Cat::new, Cat.ID, 0.6F, 0.6F);
  public static final EntityType<Chicken> CHICKEN =
      registerEntityType(Chicken::new, Chicken.ID, 0.4F, 0.7F);
  public static final EntityType<Fairy> FAIRY =
      registerEntityType(Fairy::new, Fairy.ID, 0.6F, 2.0F);
  public static final EntityType<Horse> HORSE =
      registerEntityType(Horse::new, Horse.ID, 1.4F, 1.6F);
  public static final EntityType<IronGolem> IRON_GOLEM =
      registerEntityType(IronGolem::new, IronGolem.ID, 1.4F, 2.7F);
  public static final EntityType<Pig> PIG = registerEntityType(Pig::new, Pig.ID, 0.9F, 0.9F);
  public static final EntityType<Piglin> PIGLIN = registerEntityType(Piglin::new, Piglin.ID);
  public static final EntityType<Piglin> PIGLIN_BRUTE =
      registerEntityType(
          (entityType, level) -> new Piglin(entityType, level, Piglin.Variant.BRUTE),
          Piglin.ID_BRUTE);
  public static final EntityType<Piglin> PIGLIN_ZOMBIFIED =
      registerEntityType(
          (entityType, level) -> new Piglin(entityType, level, Piglin.Variant.ZOMBIFIED),
          Piglin.ID_ZOMBIFIED);
  public static final EntityType<Horse> SKELETON_HORSE =
      registerEntityType(
          (entityType, level) -> new Horse(entityType, level, Horse.Variant.SKELETON),
          Horse.ID_SKELETON,
          1.4F,
          1.6F);
  public static final EntityType<Wolf> WOLF = registerEntityType(Wolf::new, Wolf.ID, 0.6F, 0.85F);
  public static final EntityType<Horse> ZOMBIE_HORSE =
      registerEntityType(
          (entityType, level) -> new Horse(entityType, level, Horse.Variant.ZOMBIE),
          Horse.ID_ZOMBIE,
          1.4F,
          1.6F);
  public static final EntityType<Zombie> DROWNED =
      registerEntityType(
          (entityType, level) -> new Zombie(entityType, level, Zombie.Variant.DROWNED),
          Zombie.ID_DROWNED);
  public static final EntityType<Illager> EVOKER =
      registerEntityType(
          (entityType, level) ->
              new Illager(entityType, level, Illager.Variant.EVOKER_CROSSED_ARMS),
          Illager.ID_EVOKER);
  public static final EntityType<Humanoid> HUMANOID =
      registerEntityType(Humanoid::new, Humanoid.ID);
  public static final EntityType<HumanoidSlim> HUMANOID_SLIM =
      registerEntityType(HumanoidSlim::new, HumanoidSlim.ID);
  public static final EntityType<Zombie> HUSK =
      registerEntityType(
          (entityType, level) -> new Zombie(entityType, level, Zombie.Variant.HUSK),
          Zombie.ID_HUSK);
  public static final EntityType<Illager> ILLUSIONER =
      registerEntityType(
          (entityType, level) ->
              new Illager(entityType, level, Illager.Variant.ILLUSIONER_CROSSED_ARMS),
          Illager.ID_ILLUSIONER);
  public static final EntityType<Orc> ORC = registerEntityType(Orc::new, Orc.ID, 0.6F, 1.9F);
  public static final EntityType<Orc> ORC_WARRIOR =
      registerEntityType(
          (entityType, level) -> new Orc(entityType, level, Orc.Variant.WARRIOR),
          Orc.ID_WARRIOR,
          0.6F,
          1.9F);
  public static final EntityType<Illager> PILLAGER =
      registerEntityType(
          (entityType, level) -> new Illager(entityType, level, Illager.Variant.PILLAGER),
          Illager.ID_PILLAGER);
  public static final EntityType<Skeleton> SKELETON =
      registerEntityType(Skeleton::new, Skeleton.ID);
  public static final EntityType<Skeleton> STRAY =
      registerEntityType(
          (entityType, level) -> new Skeleton(entityType, level, Skeleton.Variant.STRAY),
          Skeleton.ID_STRAY);
  public static final EntityType<Villager> VILLAGER =
      registerEntityType(Villager::new, Villager.ID);
  public static final EntityType<Illager> VINDICATOR =
      registerEntityType(
          (entityType, level) ->
              new Illager(entityType, level, Illager.Variant.VINDICATOR_CROSSED_ARMS),
          Illager.ID_VINDICATOR);
  public static final EntityType<Skeleton> WITHER_SKELETON =
      registerEntityType(
          (entityType, level) -> new Skeleton(entityType, level, Skeleton.Variant.WITHER_SKELETON),
          Skeleton.ID_WITHER_SKELETON);
  public static final EntityType<Zombie> ZOMBIE = registerEntityType(Zombie::new, Zombie.ID);
  public static final EntityType<ZombieVillager> ZOMBIE_VILLAGER =
      registerEntityType(ZombieVillager::new, ZombieVillager.ID);

  // Raw entities (for modding only)
  public static final EntityType<ZombieRaw> ZOMBIE_RAW =
      registerEntityType(ZombieRaw::new, ZombieRaw.ID);

  private ModEntityTypes() {}

  private static <T extends Entity> EntityType<T> registerEntityType(
      EntityFactory<T> entityFactory, String id) {
    return registerEntityType(entityFactory, id, 0.6F, 1.95F);
  }

  private static <T extends Entity> EntityType<T> registerEntityType(
      EntityFactory<T> entityFactory, String id, float width, float height) {
    EntityType<T> entityType =
        EntityType.Builder.of(entityFactory, MobCategory.MISC)
            .sized(width, height)
            .clientTrackingRange(12)
            .build(id);
    REGISTERED_ENTITY_TYPES.add(entityType);
    return entityType;
  }

  public static Set<EntityType<?>> getRegisteredEntityTypes() {
    return REGISTERED_ENTITY_TYPES;
  }
}
