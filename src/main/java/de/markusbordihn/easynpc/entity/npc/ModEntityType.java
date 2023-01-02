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

  // NPC Entity
  public static final RegistryObject<EntityType<Humanoid>> HUMANOID = ENTITIES.register(Humanoid.ID,
      () -> EntityType.Builder.<Humanoid>of(Humanoid::new, EasyNPCEntity.CATEGORY).sized(1.0F, 1.2F)
          .clientTrackingRange(8).build(Humanoid.ID));

  public static final RegistryObject<EntityType<Villager>> VILLAGER = ENTITIES.register(Villager.ID,
      () -> EntityType.Builder.<Villager>of(Villager::new, EasyNPCEntity.CATEGORY).sized(1.0F, 1.2F)
          .clientTrackingRange(8).build(Villager.ID));

  @SubscribeEvent
  public static final void entityAttributeCreation(EntityAttributeCreationEvent event) {
    // Create Attributes for Entities
    event.put(HUMANOID.get(), Humanoid.createAttributes().build());
    event.put(VILLAGER.get(), Villager.createAttributes().build());
  }
}
