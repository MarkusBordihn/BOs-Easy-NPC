/**
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

import net.minecraft.world.entity.EntityType;

import net.minecraftforge.event.entity.EntityAttributeCreationEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;

@EventBusSubscriber(bus = Mod.EventBusSubscriber.Bus.MOD)
public class ModEntityType {

  protected ModEntityType() {

  }

  public static final DeferredRegister<EntityType<?>> ENTITIES =
      DeferredRegister.create(ForgeRegistries.ENTITIES, Constants.MOD_ID);

  // Default NPC Entity
  public static final RegistryObject<EntityType<Fairy>> FAIRY = ENTITIES.register(Fairy.ID,
      () -> EntityType.Builder.<Fairy>of(Fairy::new, EasyNPCEntity.CATEGORY).sized(1.0F, 2.0F)
          .clientTrackingRange(8).build(Fairy.ID));

  public static final RegistryObject<EntityType<Humanoid>> HUMANOID =
      registerHumanoid(Humanoid.ID, Humanoid.Variant.STEVE);

  public static final RegistryObject<EntityType<HumanoidSlim>> HUMANOID_SLIM =
      registerHumanoidSlim(HumanoidSlim.ID, HumanoidSlim.Variant.ALEX);

  public static final RegistryObject<EntityType<Skeleton>> SKELETON = ENTITIES.register(Skeleton.ID,
      () -> EntityType.Builder.<Skeleton>of(Skeleton::new, EasyNPCEntity.CATEGORY).sized(1.0F, 2.0F)
          .clientTrackingRange(8).build(Skeleton.ID));

  public static final RegistryObject<EntityType<Villager>> VILLAGER = ENTITIES.register(Villager.ID,
      () -> EntityType.Builder.<Villager>of(Villager::new, EasyNPCEntity.CATEGORY).sized(1.0F, 2.0F)
          .clientTrackingRange(8).build(Villager.ID));

  public static final RegistryObject<EntityType<Zombie>> ZOMBIE = ENTITIES.register(Zombie.ID,
      () -> EntityType.Builder.<Zombie>of(Zombie::new, EasyNPCEntity.CATEGORY).sized(1.0F, 2.0F)
          .clientTrackingRange(8).build(Zombie.ID));

  public static final RegistryObject<EntityType<ZombieVillager>> ZOMBIE_VILLAGER =
      ENTITIES.register(ZombieVillager.ID,
          () -> EntityType.Builder.<ZombieVillager>of(ZombieVillager::new, EasyNPCEntity.CATEGORY)
              .sized(1.0F, 2.0F).clientTrackingRange(8).build(ZombieVillager.ID));

  @SubscribeEvent
  public static final void entityAttributeCreation(EntityAttributeCreationEvent event) {
    // Default NPC Entities
    event.put(FAIRY.get(), Fairy.createAttributes().build());
    event.put(HUMANOID.get(), Humanoid.createAttributes().build());
    event.put(HUMANOID_SLIM.get(), HumanoidSlim.createAttributes().build());
    event.put(SKELETON.get(), Skeleton.createAttributes().build());
    event.put(VILLAGER.get(), Villager.createAttributes().build());
    event.put(ZOMBIE.get(), Zombie.createAttributes().build());
    event.put(ZOMBIE_VILLAGER.get(), ZombieVillager.createAttributes().build());
  }

  // Register Handler
  public static RegistryObject<EntityType<Humanoid>> registerHumanoid(String id, Enum<?> variant) {
    return ENTITIES.register(id, () -> EntityType.Builder.<Humanoid>of((entityType, level) -> {
      return new Humanoid(entityType, level, variant);
    }, EasyNPCEntity.CATEGORY).sized(1.0F, 2.0F).clientTrackingRange(8).build(id));
  }

  public static RegistryObject<EntityType<HumanoidSlim>> registerHumanoidSlim(String id,
      Enum<?> variant) {
    return ENTITIES.register(id, () -> EntityType.Builder.<HumanoidSlim>of((entityType, level) -> {
      return new HumanoidSlim(entityType, level, variant);
    }, EasyNPCEntity.CATEGORY).sized(1.0F, 2.0F).clientTrackingRange(8).build(id));
  }
}
