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
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.EntityType;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.EntityAttributeCreationEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class ModEntityType {

  public static final DeferredRegister<EntityType<?>> ENTITY_TYPES =
      DeferredRegister.create(BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID);

  public static final DeferredHolder<EntityType<?>, EntityType<Allay>> ALLAY =
      ENTITY_TYPES.register(Allay.ID, () -> ModEntityTypes.ALLAY);
  public static final DeferredHolder<EntityType<?>, EntityType<Cat>> CAT =
      ENTITY_TYPES.register(Cat.ID, () -> ModEntityTypes.CAT);
  public static final DeferredHolder<EntityType<?>, EntityType<Chicken>> CHICKEN =
      ENTITY_TYPES.register(Chicken.ID, () -> ModEntityTypes.CHICKEN);
  public static final DeferredHolder<EntityType<?>, EntityType<Zombie>> DROWNED =
      ENTITY_TYPES.register(Zombie.ID_DROWNED, () -> ModEntityTypes.DROWNED);
  public static final DeferredHolder<EntityType<?>, EntityType<Illager>> EVOKER =
      ENTITY_TYPES.register(Illager.ID_EVOKER, () -> ModEntityTypes.EVOKER);
  public static final DeferredHolder<EntityType<?>, EntityType<Fairy>> FAIRY =
      ENTITY_TYPES.register(Fairy.ID, () -> ModEntityTypes.FAIRY);
  public static final DeferredHolder<EntityType<?>, EntityType<Horse>> HORSE =
      ENTITY_TYPES.register(Horse.ID, () -> ModEntityTypes.HORSE);
  public static final DeferredHolder<EntityType<?>, EntityType<Humanoid>> HUMANOID =
      ENTITY_TYPES.register(Humanoid.ID, () -> ModEntityTypes.HUMANOID);
  public static final DeferredHolder<EntityType<?>, EntityType<HumanoidSlim>> HUMANOID_SLIM =
      ENTITY_TYPES.register(HumanoidSlim.ID, () -> ModEntityTypes.HUMANOID_SLIM);
  public static final DeferredHolder<EntityType<?>, EntityType<Zombie>> HUSK =
      ENTITY_TYPES.register(Zombie.ID_HUSK, () -> ModEntityTypes.HUSK);
  public static final DeferredHolder<EntityType<?>, EntityType<Illager>> ILLUSIONER =
      ENTITY_TYPES.register(Illager.ID_ILLUSIONER, () -> ModEntityTypes.ILLUSIONER);
  public static final DeferredHolder<EntityType<?>, EntityType<IronGolem>> IRON_GOLEM =
      ENTITY_TYPES.register(IronGolem.ID, () -> ModEntityTypes.IRON_GOLEM);
  public static final DeferredHolder<EntityType<?>, EntityType<Orc>> ORC =
      ENTITY_TYPES.register(Orc.ID, () -> ModEntityTypes.ORC);
  public static final DeferredHolder<EntityType<?>, EntityType<Orc>> ORC_WARRIOR =
      ENTITY_TYPES.register(Orc.ID_WARRIOR, () -> ModEntityTypes.ORC_WARRIOR);
  public static final DeferredHolder<EntityType<?>, EntityType<Pig>> PIG =
      ENTITY_TYPES.register(Pig.ID, () -> ModEntityTypes.PIG);
  public static final DeferredHolder<EntityType<?>, EntityType<Piglin>> PIGLIN =
      ENTITY_TYPES.register(Piglin.ID, () -> ModEntityTypes.PIGLIN);
  public static final DeferredHolder<EntityType<?>, EntityType<Piglin>> PIGLIN_BRUTE =
      ENTITY_TYPES.register(Piglin.ID_BRUTE, () -> ModEntityTypes.PIGLIN_BRUTE);
  public static final DeferredHolder<EntityType<?>, EntityType<Piglin>> PIGLIN_ZOMBIFIED =
      ENTITY_TYPES.register(Piglin.ID_ZOMBIFIED, () -> ModEntityTypes.PIGLIN_ZOMBIFIED);
  public static final DeferredHolder<EntityType<?>, EntityType<Illager>> PILLAGER =
      ENTITY_TYPES.register(Illager.ID_PILLAGER, () -> ModEntityTypes.PILLAGER);
  public static final DeferredHolder<EntityType<?>, EntityType<Skeleton>> SKELETON =
      ENTITY_TYPES.register(Skeleton.ID, () -> ModEntityTypes.SKELETON);
  public static final DeferredHolder<EntityType<?>, EntityType<Horse>> SKELETON_HORSE =
      ENTITY_TYPES.register(Horse.ID_SKELETON, () -> ModEntityTypes.SKELETON_HORSE);
  public static final DeferredHolder<EntityType<?>, EntityType<Skeleton>> STRAY =
      ENTITY_TYPES.register(Skeleton.ID_STRAY, () -> ModEntityTypes.STRAY);
  public static final DeferredHolder<EntityType<?>, EntityType<Villager>> VILLAGER =
      ENTITY_TYPES.register(Villager.ID, () -> ModEntityTypes.VILLAGER);
  public static final DeferredHolder<EntityType<?>, EntityType<Illager>> VINDICATOR =
      ENTITY_TYPES.register(Illager.ID_VINDICATOR, () -> ModEntityTypes.VINDICATOR);
  public static final DeferredHolder<EntityType<?>, EntityType<Skeleton>> WITHER_SKELETON =
      ENTITY_TYPES.register(Skeleton.ID_WITHER_SKELETON, () -> ModEntityTypes.WITHER_SKELETON);
  public static final DeferredHolder<EntityType<?>, EntityType<Wolf>> WOLF =
      ENTITY_TYPES.register(Wolf.ID, () -> ModEntityTypes.WOLF);
  public static final DeferredHolder<EntityType<?>, EntityType<Zombie>> ZOMBIE =
      ENTITY_TYPES.register(Zombie.ID, () -> ModEntityTypes.ZOMBIE);
  public static final DeferredHolder<EntityType<?>, EntityType<Horse>> ZOMBIE_HORSE =
      ENTITY_TYPES.register(Horse.ID_ZOMBIE, () -> ModEntityTypes.ZOMBIE_HORSE);
  public static final DeferredHolder<EntityType<?>, EntityType<ZombieVillager>> ZOMBIE_VILLAGER =
      ENTITY_TYPES.register(ZombieVillager.ID, () -> ModEntityTypes.ZOMBIE_VILLAGER);

  // Raw entities (for modding only)
  public static final DeferredHolder<EntityType<?>, EntityType<ZombieRaw>> ZOMBIE_RAW =
      ENTITY_TYPES.register(ZombieRaw.ID, () -> ModEntityTypes.ZOMBIE_RAW);

  // Optional: Epic Fight entities
  public static DeferredHolder<EntityType<?>, EntityType<EpicFightZombie>> EPIC_FIGHT_ZOMBIE;

  static {
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      EPIC_FIGHT_ZOMBIE =
          ENTITY_TYPES.register(EpicFightZombie.ID, () -> EpicFightEntityTypes.ZOMBIE);
    }
  }

  private ModEntityType() {}

  @SubscribeEvent
  public static void entityAttributeCreation(EntityAttributeCreationEvent event) {
    event.put(ALLAY.get(), Allay.createAttributes().build());
    event.put(CAT.get(), Cat.createAttributes().build());
    event.put(CHICKEN.get(), Chicken.createAttributes().build());
    event.put(DROWNED.get(), Zombie.createAttributes().build());
    event.put(EVOKER.get(), Illager.createAttributes().build());
    event.put(FAIRY.get(), Fairy.createAttributes().build());
    event.put(HORSE.get(), Horse.createAttributes().build());
    event.put(HUMANOID.get(), Humanoid.createAttributes().build());
    event.put(HUMANOID_SLIM.get(), HumanoidSlim.createAttributes().build());
    event.put(HUSK.get(), Zombie.createAttributes().build());
    event.put(ILLUSIONER.get(), Illager.createAttributes().build());
    event.put(IRON_GOLEM.get(), IronGolem.createAttributes().build());
    event.put(ORC.get(), Orc.createAttributes().build());
    event.put(ORC_WARRIOR.get(), Orc.createAttributes().build());
    event.put(PIG.get(), Pig.createAttributes().build());
    event.put(PIGLIN.get(), Piglin.createAttributes().build());
    event.put(PIGLIN_BRUTE.get(), Piglin.createAttributes().build());
    event.put(PIGLIN_ZOMBIFIED.get(), Piglin.createAttributes().build());
    event.put(PILLAGER.get(), Illager.createAttributes().build());
    event.put(SKELETON.get(), Skeleton.createAttributes().build());
    event.put(SKELETON_HORSE.get(), Horse.createAttributes().build());
    event.put(STRAY.get(), Skeleton.createAttributes().build());
    event.put(VILLAGER.get(), Villager.createAttributes().build());
    event.put(VINDICATOR.get(), Illager.createAttributes().build());
    event.put(WITHER_SKELETON.get(), Skeleton.createAttributes().build());
    event.put(WOLF.get(), Wolf.createAttributes().build());
    event.put(ZOMBIE.get(), Zombie.createAttributes().build());
    event.put(ZOMBIE_HORSE.get(), Horse.createAttributes().build());
    event.put(ZOMBIE_VILLAGER.get(), ZombieVillager.createAttributes().build());

    // Raw entities (for modding only)
    event.put(
        ZOMBIE_RAW.get(), net.minecraft.world.entity.monster.Zombie.createAttributes().build());

    // Optional: Epic Fight entities
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      event.put(
          EPIC_FIGHT_ZOMBIE.get(),
          net.minecraft.world.entity.monster.Zombie.createAttributes().build());
    }
  }
}
