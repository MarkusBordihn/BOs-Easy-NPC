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
import java.util.EnumMap;
import java.util.HashMap;
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

@EventBusSubscriber
public class ModEntityType {

  public static final DeferredRegister<EntityType<?>> ENTITY_TYPES =
      DeferredRegister.create(BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID);

  public static final Map<ModRawEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> RAW_TYPE =
      new EnumMap<>(ModRawEntityType.class);
  public static final Map<ModNPCEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> NPC_TYPE =
      new EnumMap<>(ModNPCEntityType.class);
  public static final Map<ModCustomEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      CUSTOM_TYPE = new EnumMap<>(ModCustomEntityType.class);
  public static final Map<EpicFightEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      EPIC_FIGHT_TYPE = new HashMap<>();
  public static final Map<CobblemonEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      COBBLEMON_TYPE = new HashMap<>();
  public static final Map<EasyModelEntitiesEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      EASY_MODEL_ENTITIES_TYPE = new HashMap<>();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    registerEntityTypes(ModRawEntityType.values(), RAW_TYPE, "raw");
    registerEntityTypes(ModNPCEntityType.values(), NPC_TYPE, "NPC");
    registerEntityTypes(ModCustomEntityType.values(), CUSTOM_TYPE, "custom");
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      registerEntityTypes(EpicFightEntityType.values(), EPIC_FIGHT_TYPE, "Epic Fight");
    }
    if (CompatConstants.MOD_COBBLEMON_LOADED) {
      registerEntityTypes(CobblemonEntityType.values(), COBBLEMON_TYPE, "Cobblemon");
    }
    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      registerEntityTypes(
          EasyModelEntitiesEntityType.values(), EASY_MODEL_ENTITIES_TYPE, "Easy Model Entities");
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
    registerAttributes(event, ModRawEntityType.values(), RAW_TYPE, "Raw");
    registerAttributes(event, ModNPCEntityType.values(), NPC_TYPE, "NPC");
    registerAttributes(event, ModCustomEntityType.values(), CUSTOM_TYPE, "Custom");
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      registerAttributes(event, EpicFightEntityType.values(), EPIC_FIGHT_TYPE, "Epic Fight");
    }
    if (CompatConstants.MOD_COBBLEMON_LOADED) {
      registerAttributes(event, CobblemonEntityType.values(), COBBLEMON_TYPE, "Cobblemon");
    }
    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      registerAttributes(
          event,
          EasyModelEntitiesEntityType.values(),
          EASY_MODEL_ENTITIES_TYPE,
          "Easy Model Entities");
    }
  }

  private static <E extends ModEntityTypeProvider> void registerEntityTypes(
      E[] types,
      Map<E, DeferredHolder<EntityType<?>, EntityType<?>>> registeredTypes,
      String typeName) {
    for (E type : types) {
      log.debug("Registering {} entity type {}", typeName, type.getResourceKey());
      registeredTypes.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey())));
    }
    log.info("Registered {} {} entity types.", registeredTypes.size(), typeName);
  }

  private static <E extends ModEntityTypeProvider> void registerAttributes(
      EntityAttributeCreationEvent event,
      E[] types,
      Map<E, DeferredHolder<EntityType<?>, EntityType<?>>> registeredTypes,
      String typeName) {
    for (E type : types) {
      if (type.getAttributes() != null) {
        event.put(
            (EntityType<? extends LivingEntity>) registeredTypes.get(type).get(),
            ModEntityAttributes.buildWithNavigationAttributes(type));
      } else {
        log.warn(
            "{} entity type {} does not have attributes defined!", typeName, type.getResourceKey());
      }
    }
  }

  public static <T extends Entity> EntityType<T> getEntityType(EpicFightEntityType type) {
    if (!EPIC_FIGHT_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid Epic Fight entity type '"
              + type
              + "'! Supported types are "
              + EPIC_FIGHT_TYPE.keySet());
    }

    return (EntityType<T>) EPIC_FIGHT_TYPE.get(type).get();
  }

  public static <T extends Entity> EntityType<T> getEntityType(CobblemonEntityType type) {
    if (!COBBLEMON_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid Cobblemon entity type '"
              + type
              + "'! Supported types are "
              + COBBLEMON_TYPE.keySet());
    }

    return (EntityType<T>) COBBLEMON_TYPE.get(type).get();
  }

  public static <T extends Entity> EntityType<T> getEntityType(EasyModelEntitiesEntityType type) {
    if (!EASY_MODEL_ENTITIES_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid Easy Model Entities entity type '"
              + type
              + "'! Supported types are "
              + EASY_MODEL_ENTITIES_TYPE.keySet());
    }

    return (EntityType<T>) EASY_MODEL_ENTITIES_TYPE.get(type).get();
  }
}
