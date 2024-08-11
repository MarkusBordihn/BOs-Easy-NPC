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
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;

public class ModEntityTypes {
  private static final MobCategory CATEGORY = MobCategory.MISC;
  private static final int CLIENT_TRACKING_RANGE = 12;
  public static final EntityType<Allay> ALLAY =
      EntityType.Builder.of(Allay::new, CATEGORY)
          .sized(0.6F, 0.90F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Allay.ID);
  public static final EntityType<Cat> CAT =
      EntityType.Builder.of(Cat::new, CATEGORY)
          .sized(0.6F, 0.6F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Cat.ID);
  public static final EntityType<Chicken> CHICKEN =
      EntityType.Builder.of(Chicken::new, CATEGORY)
          .sized(0.6F, 0.9F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Chicken.ID);
  public static final EntityType<Fairy> FAIRY =
      EntityType.Builder.<Fairy>of(Fairy::new, CATEGORY)
          .sized(0.6F, 2.0F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Fairy.ID);
  public static final EntityType<Horse> HORSE =
      EntityType.Builder.<Horse>of(Horse::new, CATEGORY)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Horse.ID);
  public static final EntityType<IronGolem> IRON_GOLEM =
      EntityType.Builder.of(IronGolem::new, CATEGORY)
          .sized(1.4F, 2.7F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(IronGolem.ID);
  public static final EntityType<Pig> PIG =
      EntityType.Builder.of(Pig::new, CATEGORY)
          .sized(0.9F, 0.9F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Pig.ID);
  public static final EntityType<Piglin> PIGLIN =
      EntityType.Builder.<Piglin>of(Piglin::new, CATEGORY)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Piglin.ID);
  public static final EntityType<Piglin> PIGLIN_BRUTE =
      EntityType.Builder.<Piglin>of(
              (entityType, level) -> new Piglin(entityType, level, Piglin.Variant.BRUTE), CATEGORY)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Piglin.ID_BRUTE);
  public static final EntityType<Piglin> PIGLIN_ZOMBIFIED =
      EntityType.Builder.<Piglin>of(
              (entityType, level) -> new Piglin(entityType, level, Piglin.Variant.ZOMBIFIED),
              CATEGORY)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Piglin.ID_ZOMBIFIED);
  public static final EntityType<Horse> SKELETON_HORSE =
      EntityType.Builder.<Horse>of(
              (entityType, level) -> new Horse(entityType, level, Horse.Variant.SKELETON), CATEGORY)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Horse.ID_SKELETON);
  public static final EntityType<Wolf> WOLF =
      EntityType.Builder.of(Wolf::new, CATEGORY)
          .sized(0.6F, 0.85F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Wolf.ID);
  public static final EntityType<Horse> ZOMBIE_HORSE =
      EntityType.Builder.<Horse>of(
              (entityType, level) -> new Horse(entityType, level, Horse.Variant.ZOMBIE), CATEGORY)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Horse.ID_ZOMBIE);
  private static final float HUMANOID_SIZE_HEIGHT = 1.95F;
  private static final float HUMANOID_SIZE_WIDTH = 0.6F;
  public static final EntityType<Zombie> DROWNED =
      EntityType.Builder.<Zombie>of(
              (entityType, level) -> new Zombie(entityType, level, Zombie.Variant.DROWNED),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Zombie.ID_DROWNED);
  public static final EntityType<Illager> EVOKER =
      EntityType.Builder.<Illager>of(
              (entityType, level) ->
                  new Illager(entityType, level, Illager.Variant.EVOKER_CROSSED_ARMS),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Illager.ID_EVOKER);
  public static final EntityType<Humanoid> HUMANOID =
      EntityType.Builder.<Humanoid>of(Humanoid::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Humanoid.ID);
  public static final EntityType<HumanoidSlim> HUMANOID_SLIM =
      EntityType.Builder.<HumanoidSlim>of(HumanoidSlim::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(HumanoidSlim.ID);
  public static final EntityType<Zombie> HUSK =
      EntityType.Builder.<Zombie>of(
              (entityType, level) -> new Zombie(entityType, level, Zombie.Variant.HUSK), CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Zombie.ID_HUSK);
  public static final EntityType<Illager> ILLUSIONER =
      EntityType.Builder.<Illager>of(
              (entityType, level) ->
                  new Illager(entityType, level, Illager.Variant.ILLUSIONER_CROSSED_ARMS),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Illager.ID_ILLUSIONER);
  public static final EntityType<Orc> ORC =
      EntityType.Builder.<Orc>of(Orc::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, 1.9F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Orc.ID);
  public static final EntityType<Orc> ORC_WARRIOR =
      EntityType.Builder.<Orc>of(
              (entityType, level) -> new Orc(entityType, level, Orc.Variant.WARRIOR), CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, 1.9F)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Orc.ID_WARRIOR);
  public static final EntityType<Illager> PILLAGER =
      EntityType.Builder.<Illager>of(
              (entityType, level) -> new Illager(entityType, level, Illager.Variant.PILLAGER),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Illager.ID_PILLAGER);
  public static final EntityType<Skeleton> SKELETON =
      EntityType.Builder.<Skeleton>of(Skeleton::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Skeleton.ID);
  public static final EntityType<Skeleton> STRAY =
      EntityType.Builder.<Skeleton>of(
              (entityType, level) -> new Skeleton(entityType, level, Skeleton.Variant.STRAY),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Skeleton.ID_STRAY);
  public static final EntityType<Villager> VILLAGER =
      EntityType.Builder.of(Villager::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Villager.ID);
  public static final EntityType<Illager> VINDICATOR =
      EntityType.Builder.<Illager>of(
              (entityType, level) ->
                  new Illager(entityType, level, Illager.Variant.VINDICATOR_CROSSED_ARMS),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Illager.ID_VINDICATOR);
  public static final EntityType<Skeleton> WITHER_SKELETON =
      EntityType.Builder.<Skeleton>of(
              (entityType, level) ->
                  new Skeleton(entityType, level, Skeleton.Variant.WITHER_SKELETON),
              CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Skeleton.ID_WITHER_SKELETON);
  public static final EntityType<Zombie> ZOMBIE =
      EntityType.Builder.<Zombie>of(Zombie::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(Zombie.ID);
  public static final EntityType<ZombieVillager> ZOMBIE_VILLAGER =
      EntityType.Builder.of(ZombieVillager::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(ZombieVillager.ID);

  // Raw entities (for modding only)
  public static final EntityType<ZombieRaw> ZOMBIE_RAW =
      EntityType.Builder.of(ZombieRaw::new, CATEGORY)
          .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
          .clientTrackingRange(CLIENT_TRACKING_RANGE)
          .build(ZombieRaw.ID);

  private ModEntityTypes() {}
}
