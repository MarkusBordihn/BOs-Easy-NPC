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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.entity.EntityAttributeCreationEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber(modid = Constants.MOD_ID)
public class ModEntityType {

  public static final DeferredRegister<EntityType<?>> ENTITY_TYPES =
      DeferredRegister.create(BuiltInRegistries.ENTITY_TYPE, Constants.MOD_ID);

  public static final Map<ModRawEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> RAW_TYPE =
      new EnumMap<>(ModRawEntityType.class);
  public static final Map<ModNPCEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> NPC_TYPE =
      new EnumMap<>(ModNPCEntityType.class);
  public static final Map<ModCustomEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      CUSTOM_TYPE = new EnumMap<>(ModCustomEntityType.class);
  public static final Map<UserDefinedEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      USER_DEFINED_TYPE = new ConcurrentHashMap<>();
  public static final Map<EpicFightEntityType, DeferredHolder<EntityType<?>, EntityType<?>>>
      EPIC_FIGHT_TYPE = new EnumMap<>(EpicFightEntityType.class);
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    // Raw entities (for modding only)
    for (ModRawEntityType type : ModRawEntityType.values()) {
      log.info("Registering raw entity type {}", type.getResourceKey());
      RAW_TYPE.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey().toString())));
    }

    // Pre-defined NPCs
    for (ModNPCEntityType type : ModNPCEntityType.values()) {
      log.info("Registering NPC entity type {}", type.getResourceKey());
      NPC_TYPE.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey().toString())));
    }

    // Custom NPCs
    for (ModCustomEntityType type : ModCustomEntityType.values()) {
      log.info("Registering custom entity type {}", type.getResourceKey());
      CUSTOM_TYPE.put(
          type,
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey().toString())));
    }

    // Initialize user-defined NPC registry
    UserDefinedEntityRegistry.initialize();

    // Register user-defined NPCs from configuration file
    for (UserDefinedEntityType type : UserDefinedEntityRegistry.getAvailableEntityTypes()) {
      log.info("Registering user-defined entity type {}", type.getResourceKey());
      DeferredHolder<EntityType<?>, EntityType<?>> registryObject =
          ENTITY_TYPES.register(
              type.getId(), () -> type.getBuilder().build(type.getResourceKey().toString()));

      USER_DEFINED_TYPE.put(type, registryObject);
    }

    // Register Epic Fight entity types if the mod is loaded
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      for (EpicFightEntityType type : EpicFightEntityType.values()) {
        log.info("Registering Epic Fight entity type {}", type.getResourceKey());
        EPIC_FIGHT_TYPE.put(
            type,
            ENTITY_TYPES.register(
                type.getId(), () -> type.getBuilder().build(type.getResourceKey().toString())));
      }
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

  public static <T extends Entity> EntityType<T> getConfigurableEntityType(
      UserDefinedEntityType type) {
    if (!USER_DEFINED_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid configured entity type '"
              + type
              + "'! Supported types are "
              + USER_DEFINED_TYPE.keySet());
    }
    return (EntityType<T>) USER_DEFINED_TYPE.get(type).get();
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

    // User-defined NPCs from configuration file - now safe to access .get()
    for (Map.Entry<UserDefinedEntityType, DeferredHolder<EntityType<?>, EntityType<?>>> entry :
        USER_DEFINED_TYPE.entrySet()) {
      UserDefinedEntityType type = entry.getKey();
      EntityType<?> entityType = entry.getValue().get(); // Safe to call .get() now
      EntityType<?> baseEntityType = type.getBaseEntityType();

      // Register the resolved entity type with the UserDefinedEntityRegistry
      UserDefinedEntityRegistry.registerEntityType(type, entityType);

      // Determine which attributes to use based on the base entity type
      try {
        boolean attributesFound = false;

        // Try to find matching raw entity type
        for (ModRawEntityType rawType : ModRawEntityType.values()) {
          if (RAW_TYPE.get(rawType).get() == baseEntityType) {
            if (rawType.getAttributes() != null) {
              event.put(
                  (EntityType<? extends LivingEntity>) entityType, rawType.getAttributes().build());
              attributesFound = true;
              log.debug(
                  "Used raw entity type {} attributes for user-defined entity {}",
                  rawType.getId(),
                  type.getId());
              break;
            }
          }
        }

        // Try to find matching NPC entity type
        if (!attributesFound) {
          for (ModNPCEntityType npcType : ModNPCEntityType.values()) {
            if (NPC_TYPE.get(npcType).get() == baseEntityType) {
              if (npcType.getAttributes() != null) {
                event.put(
                    (EntityType<? extends LivingEntity>) entityType,
                    npcType.getAttributes().build());
                attributesFound = true;
                log.debug(
                    "Used NPC entity type {} attributes for user-defined entity {}",
                    npcType.getId(),
                    type.getId());
                break;
              }
            }
          }
        }

        // Try to find matching custom entity type
        if (!attributesFound) {
          for (ModCustomEntityType customType : ModCustomEntityType.values()) {
            if (CUSTOM_TYPE.get(customType).get() == baseEntityType) {
              if (customType.getAttributes() != null) {
                event.put(
                    (EntityType<? extends LivingEntity>) entityType,
                    customType.getAttributes().build());
                attributesFound = true;
                log.debug(
                    "Used custom entity type {} attributes for user-defined entity {}",
                    customType.getId(),
                    type.getId());
                break;
              }
            }
          }
        }

        // Fallback: Use vanilla entity attributes if available
        if (!attributesFound) {
          AttributeSupplier.Builder vanillaAttributes =
              VanillaEntityAttributeHelper.getVanillaAttributesForEntityType(baseEntityType);
          if (vanillaAttributes != null) {
            event.put((EntityType<? extends LivingEntity>) entityType, vanillaAttributes.build());
            attributesFound = true;
            log.info(
                "Used vanilla attributes for user-defined entity {} with base type {}",
                type.getId(),
                baseEntityType);
          }
        }

        if (!attributesFound) {
          log.error(
              "No attributes found for user-defined entity {} with base type {} - this will cause crashes!",
              type.getId(),
              baseEntityType);

          // Emergency fallback: Use generic living entity attributes
          event.put(
              (EntityType<? extends LivingEntity>) entityType,
              net.minecraft.world.entity.LivingEntity.createLivingAttributes().build());
          log.warn("Using emergency fallback attributes for user-defined entity {}", type.getId());
        }

      } catch (Exception e) {
        log.error(
            "Failed to set attributes for user-defined entity {}: {}",
            type.getId(),
            e.getMessage(),
            e);

        // Emergency fallback in case of any error
        try {
          event.put(
              (EntityType<? extends LivingEntity>) entityType,
              net.minecraft.world.entity.LivingEntity.createLivingAttributes().build());
          log.warn(
              "Applied emergency fallback attributes for user-defined entity {} due to error",
              type.getId());
        } catch (Exception fallbackError) {
          log.error(
              "Even emergency fallback failed for user-defined entity {}: {}",
              type.getId(),
              fallbackError.getMessage());
        }
      }
    }

    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      for (EpicFightEntityType type : EpicFightEntityType.values()) {
        if (type.getAttributes() != null) {
          event.put(
              (EntityType<? extends LivingEntity>) EPIC_FIGHT_TYPE.get(type).get(),
              type.getAttributes().build());
        } else {
          log.warn(
              "Epic Fight entity type {} does not have attributes defined!", type.getResourceKey());
        }
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
}
