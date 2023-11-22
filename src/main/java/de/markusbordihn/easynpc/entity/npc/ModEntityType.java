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

package de.markusbordihn.easynpc.entity.npc;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.world.entity.EntityType;
import net.minecraftforge.event.entity.EntityAttributeCreationEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

@EventBusSubscriber(bus = Mod.EventBusSubscriber.Bus.MOD)
public class ModEntityType {

  public static final int CLIENT_TRACKING_RANGE = 12;
  public static final float HUMANOID_SIZE_WIDTH = 0.6F;
  public static final float HUMANOID_SIZE_HEIGHT = 1.8F;
  public static final DeferredRegister<EntityType<?>> ENTITY_TYPES =
      DeferredRegister.create(ForgeRegistries.ENTITY_TYPES, Constants.MOD_ID);
  // Humanoid NPC Entity
  public static final RegistryObject<EntityType<Humanoid>> HUMANOID =
      registerHumanoid(Humanoid.ID, Humanoid.Variant.STEVE);
  public static final RegistryObject<EntityType<HumanoidSlim>> HUMANOID_SLIM =
      registerHumanoidSlim(HumanoidSlim.ID, HumanoidSlim.Variant.ALEX);
  // Default NPC Entity
  public static final RegistryObject<EntityType<Allay>> ALLAY =
      ENTITY_TYPES.register(
          Allay.ID,
          () ->
              EntityType.Builder.of(Allay::new, EasyNPCEntity.CATEGORY)
                  .sized(0.6F, 0.95F)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Allay.ID));
  public static final RegistryObject<EntityType<Cat>> CAT =
      ENTITY_TYPES.register(
          Cat.ID,
          () ->
              EntityType.Builder.of(Cat::new, EasyNPCEntity.CATEGORY)
                  .sized(0.6F, 0.6F)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Cat.ID));
  public static final RegistryObject<EntityType<Chicken>> CHICKEN =
      ENTITY_TYPES.register(
          Chicken.ID,
          () ->
              EntityType.Builder.of(Chicken::new, EasyNPCEntity.CATEGORY)
                  .sized(0.6F, 0.6F)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Chicken.ID));
  public static final RegistryObject<EntityType<Fairy>> FAIRY =
      ENTITY_TYPES.register(
          Fairy.ID,
          () ->
              EntityType.Builder.of(Fairy::new, EasyNPCEntity.CATEGORY)
                  .sized(0.6F, 0.6F)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Fairy.ID));

  public static final RegistryObject<EntityType<IronGolem>> IRON_GOLEM =
      ENTITY_TYPES.register(
          IronGolem.ID,
          () ->
              EntityType.Builder.<IronGolem>of(IronGolem::new, EasyNPCEntity.CATEGORY)
                  .sized(1.4F, 2.7F)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(IronGolem.ID));

  public static final RegistryObject<EntityType<Skeleton>> SKELETON =
      ENTITY_TYPES.register(
          Skeleton.ID,
          () ->
              EntityType.Builder.<Skeleton>of(Skeleton::new, EasyNPCEntity.CATEGORY)
                  .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Skeleton.ID));
  public static final RegistryObject<EntityType<Villager>> VILLAGER =
      ENTITY_TYPES.register(
          Villager.ID,
          () ->
              EntityType.Builder.of(Villager::new, EasyNPCEntity.CATEGORY)
                  .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Villager.ID));
  public static final RegistryObject<EntityType<Zombie>> ZOMBIE =
      ENTITY_TYPES.register(
          Zombie.ID,
          () ->
              EntityType.Builder.<Zombie>of(Zombie::new, EasyNPCEntity.CATEGORY)
                  .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(Zombie.ID));
  public static final RegistryObject<EntityType<ZombieVillager>> ZOMBIE_VILLAGER =
      ENTITY_TYPES.register(
          ZombieVillager.ID,
          () ->
              EntityType.Builder.of(ZombieVillager::new, EasyNPCEntity.CATEGORY)
                  .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
                  .clientTrackingRange(CLIENT_TRACKING_RANGE)
                  .build(ZombieVillager.ID));

  protected ModEntityType() {
  }

  @SubscribeEvent
  public static void entityAttributeCreation(EntityAttributeCreationEvent event) {

    // Default NPC Entities
    event.put(ALLAY.get(), Allay.createAttributes().build());
    event.put(CAT.get(), Cat.createAttributes().build());
    event.put(CHICKEN.get(), Chicken.createAttributes().build());
    event.put(FAIRY.get(), Fairy.createAttributes().build());
    event.put(HUMANOID.get(), Humanoid.createAttributes().build());
    event.put(HUMANOID_SLIM.get(), HumanoidSlim.createAttributes().build());
    event.put(IRON_GOLEM.get(), IronGolem.createAttributes().build());
    event.put(SKELETON.get(), Skeleton.createAttributes().build());
    event.put(VILLAGER.get(), Villager.createAttributes().build());
    event.put(ZOMBIE.get(), Zombie.createAttributes().build());
    event.put(ZOMBIE_VILLAGER.get(), ZombieVillager.createAttributes().build());
  }

  // Register Handler
  public static RegistryObject<EntityType<Humanoid>> registerHumanoid(String id, Enum<?> variant) {
    return ENTITY_TYPES.register(
        id,
        () ->
            EntityType.Builder.<Humanoid>of(
                    (entityType, level) -> new Humanoid(entityType, level, variant),
                    EasyNPCEntity.CATEGORY)
                .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
                .clientTrackingRange(CLIENT_TRACKING_RANGE)
                .build(id));
  }

  public static RegistryObject<EntityType<HumanoidSlim>> registerHumanoidSlim(
      String id, Enum<?> variant) {
    return ENTITY_TYPES.register(
        id,
        () ->
            EntityType.Builder.<HumanoidSlim>of(
                    (entityType, level) -> new HumanoidSlim(entityType, level, variant),
                    EasyNPCEntity.CATEGORY)
                .sized(HUMANOID_SIZE_WIDTH, HUMANOID_SIZE_HEIGHT)
                .clientTrackingRange(CLIENT_TRACKING_RANGE)
                .build(id));
  }
}
