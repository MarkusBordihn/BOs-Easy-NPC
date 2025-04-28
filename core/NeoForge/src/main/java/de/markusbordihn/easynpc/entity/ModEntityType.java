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
import de.markusbordihn.easynpc.compat.epicfight.entity.EpicFightZombie;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.EntityAttributeCreationEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class ModEntityType {

  public static final DeferredRegister<EntityType<?>> ENTITY_TYPES =
      DeferredRegister.create(BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID);

  public static final Map<ModRawEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> RAW_TYPE =
      new EnumMap<>(ModRawEntityType.class);
  public static final Map<ModNPCEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> NPC_TYPE =
      new EnumMap<>(ModNPCEntityType.class);
  public static final Map<ModCustomEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      CUSTOM_TYPE = new EnumMap<>(ModCustomEntityType.class);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Optional: Epic Fight entities
  public static DeferredHolder<EntityType<?>, EntityType<EpicFightZombie>> EPIC_FIGHT_ZOMBIE;

  static {
    // Raw entities (for modding only)
    for (ModRawEntityType type : ModRawEntityType.values()) {
      log.info("Registering raw entity type {}", type.getResourceKey());
      RAW_TYPE.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey())));
    }

    // Pre-defined NPCs
    for (ModNPCEntityType type : ModNPCEntityType.values()) {
      log.info("Registering NPC entity type {}", type.getResourceKey());
      NPC_TYPE.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey())));
    }

    // Custom NPCs
    for (ModCustomEntityType type : ModCustomEntityType.values()) {
      log.info("Registering custom entity type {}", type.getResourceKey());
      CUSTOM_TYPE.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey())));
    }
  }

  static {
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      // EPIC_FIGHT_ZOMBIE =
      //    ENTITY_TYPES.register(EpicFightZombie.ID, () -> EpicFightEntityTypes.ZOMBIE);
    }
  }

  private ModEntityType() {}

  public static <T extends Entity> EntityType<T> getEntityType(ModRawEntityType type) {
    if (!RAW_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid raw entity type '" + type + "'! Supported types are " + RAW_TYPE.keySet());
    }
    return (EntityType<T>) RAW_TYPE.get(type).get();
  }

  public static <T extends Entity> EntityType<T> getEntityType(ModNPCEntityType type) {
    if (!NPC_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid NPC entity type '" + type + "'! Supported types are " + NPC_TYPE.keySet());
    }
    return (EntityType<T>) NPC_TYPE.get(type).get();
  }

  public static <T extends Entity> EntityType<T> getEntityType(ModCustomEntityType type) {
    if (!CUSTOM_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid NPC entity type '" + type + "'! Supported types are " + CUSTOM_TYPE.keySet());
    }
    return (EntityType<T>) CUSTOM_TYPE.get(type).get();
  }

  @SubscribeEvent
  public static void entityAttributeCreation(EntityAttributeCreationEvent event) {

    // Raw entities (for modding only)
    for (ModRawEntityType type : ModRawEntityType.values()) {
      if (type.getAttributes() != null) {
        event.put(
            (EntityType<? extends LivingEntity>) RAW_TYPE.get(type).get(),
            type.getAttributes().build());
      } else {
        log.warn("Raw entity type {} does not have attributes defined!", type.getResourceKey());
      }
    }

    // Pre-defined NPCs
    for (ModNPCEntityType type : ModNPCEntityType.values()) {
      if (type.getAttributes() != null) {
        event.put(
            (EntityType<? extends LivingEntity>) NPC_TYPE.get(type).get(),
            type.getAttributes().build());
      } else {
        log.warn("NPC entity type {} does not have attributes defined!", type.getResourceKey());
      }
    }

    // Custom NPCs
    for (ModCustomEntityType type : ModCustomEntityType.values()) {
      if (type.getAttributes() != null) {
        event.put(
            (EntityType<? extends LivingEntity>) CUSTOM_TYPE.get(type).get(),
            type.getAttributes().build());
      } else {
        log.warn("Custom entity type {} does not have attributes defined!", type.getResourceKey());
      }
    }

    // Optional: Epic Fight entities
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      event.put(
          EPIC_FIGHT_ZOMBIE.get(),
          net.minecraft.world.entity.monster.Zombie.createAttributes().build());
    }
  }
}
