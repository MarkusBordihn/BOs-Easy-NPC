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
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricDefaultAttributeRegistry;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModEntityType {

  public static final Map<ModRawEntityType, EntityType<?>> RAW_TYPE =
      new EnumMap<>(ModRawEntityType.class);
  public static final Map<ModNPCEntityType, EntityType<?>> NPC_TYPE =
      new EnumMap<>(ModNPCEntityType.class);
  public static final Map<ModCustomEntityType, EntityType<?>> CUSTOM_TYPE =
      new EnumMap<>(ModCustomEntityType.class);
  public static final Map<EpicFightEntityType, EntityType<?>> EPIC_FIGHT_TYPE = new HashMap<>();
  public static final Map<CobblemonEntityType, EntityType<?>> COBBLEMON_TYPE = new HashMap<>();
  public static final Map<EasyModelEntitiesEntityType, EntityType<?>> EASY_MODEL_ENTITIES_TYPE =
      new HashMap<>();
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    // Raw entities (for modding only)
    for (ModRawEntityType type : ModRawEntityType.values()) {
      log.debug("Registering raw entity type {}", type.getResourceKey());
      RAW_TYPE.put(
          type,
          Registry.register(
              BuiltInRegistries.ENTITY_TYPE,
              Constants.MOD_PREFIX_ID + type.getId(),
              type.getBuilder().build(type.getResourceKey())));
    }
    log.info("Registered {} raw entity types.", RAW_TYPE.size());

    // Pre-defined NPCs
    for (ModNPCEntityType type : ModNPCEntityType.values()) {
      log.debug("Registering NPC entity type {}", type.getResourceKey());
      NPC_TYPE.put(
          type,
          Registry.register(
              BuiltInRegistries.ENTITY_TYPE,
              Constants.MOD_PREFIX_ID + type.getId(),
              type.getBuilder().build(type.getResourceKey())));
    }
    log.info("Registered {} NPC entity types.", NPC_TYPE.size());

    // Custom NPCs
    for (ModCustomEntityType type : ModCustomEntityType.values()) {
      log.debug("Registering custom entity type {}", type.getResourceKey());
      CUSTOM_TYPE.put(
          type,
          Registry.register(
              BuiltInRegistries.ENTITY_TYPE,
              Constants.MOD_PREFIX_ID + type.getId(),
              type.getBuilder().build(type.getResourceKey())));
    }
    log.info("Registered {} custom entity types.", CUSTOM_TYPE.size());

    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      for (EpicFightEntityType type : EpicFightEntityType.values()) {
        log.debug("Registering Epic Fight entity type {}", type.getResourceKey());
        EPIC_FIGHT_TYPE.put(
            type,
            Registry.register(
                BuiltInRegistries.ENTITY_TYPE,
                Constants.MOD_PREFIX_ID + type.getId(),
                type.getBuilder().build(type.getResourceKey())));
      }
      log.info("Registered {} Epic Fight entity types.", EPIC_FIGHT_TYPE.size());
    }

    if (CompatConstants.MOD_COBBLEMON_LOADED) {
      for (CobblemonEntityType type : CobblemonEntityType.values()) {
        log.debug("Registering Cobblemon entity type {}", type.getResourceKey());
        COBBLEMON_TYPE.put(
            type,
            Registry.register(
                BuiltInRegistries.ENTITY_TYPE,
                Constants.MOD_PREFIX_ID + type.getId(),
                type.getBuilder().build(type.getResourceKey())));
      }
      log.info("Registered {} Cobblemon entity types.", COBBLEMON_TYPE.size());
    }

    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      for (EasyModelEntitiesEntityType type : EasyModelEntitiesEntityType.values()) {
        log.debug("Registering Easy Model Entities entity type {}", type.getResourceKey());
        EASY_MODEL_ENTITIES_TYPE.put(
            type,
            Registry.register(
                BuiltInRegistries.ENTITY_TYPE,
                Constants.MOD_PREFIX_ID + type.getId(),
                type.getBuilder().build(type.getResourceKey())));
      }
      log.info("Registered {} Easy Model Entities entity types.", EASY_MODEL_ENTITIES_TYPE.size());
    }
  }

  private ModEntityType() {}

  public static <T extends Entity> EntityType<T> getEntityType(ModRawEntityType type) {
    if (!RAW_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid raw entity type '" + type + "'! Supported types are " + RAW_TYPE.keySet());
    }
    return (EntityType<T>) RAW_TYPE.get(type);
  }

  public static <T extends Entity> EntityType<T> getEntityType(ModNPCEntityType type) {
    if (!NPC_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid NPC entity type '" + type + "'! Supported types are " + NPC_TYPE.keySet());
    }
    return (EntityType<T>) NPC_TYPE.get(type);
  }

  public static <T extends Entity> EntityType<T> getEntityType(ModCustomEntityType type) {
    if (!CUSTOM_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid NPC entity type '" + type + "'! Supported types are " + CUSTOM_TYPE.keySet());
    }
    return (EntityType<T>) CUSTOM_TYPE.get(type);
  }

  public static void registerEntityAttributes() {

    // Raw entities (for modding only)
    for (ModRawEntityType type : ModRawEntityType.values()) {
      if (type.getAttributes() != null) {
        FabricDefaultAttributeRegistry.register(
            (EntityType<? extends LivingEntity>) RAW_TYPE.get(type),
            ModEntityAttributes.buildWithNavigationAttributes(type));
      } else {
        log.warn("Raw entity type {} does not have attributes defined!", type.getResourceKey());
      }
    }

    // Pre-defined NPCs
    for (ModNPCEntityType type : ModNPCEntityType.values()) {
      if (type.getAttributes() != null) {
        FabricDefaultAttributeRegistry.register(
            (EntityType<? extends LivingEntity>) NPC_TYPE.get(type),
            ModEntityAttributes.buildWithNavigationAttributes(type));
      } else {
        log.warn("NPC entity type {} does not have attributes defined!", type.getResourceKey());
      }
    }

    // Custom NPCs
    for (ModCustomEntityType type : ModCustomEntityType.values()) {
      if (type.getAttributes() != null) {
        FabricDefaultAttributeRegistry.register(
            (EntityType<? extends LivingEntity>) CUSTOM_TYPE.get(type),
            ModEntityAttributes.buildWithNavigationAttributes(type));
      } else {
        log.warn("Custom entity type {} does not have attributes defined!", type.getResourceKey());
      }
    }

    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      for (EpicFightEntityType type : EpicFightEntityType.values()) {
        if (type.getAttributes() != null) {
          FabricDefaultAttributeRegistry.register(
              (EntityType<? extends LivingEntity>) EPIC_FIGHT_TYPE.get(type),
              ModEntityAttributes.buildWithNavigationAttributes(type));
        } else {
          log.warn(
              "Epic Fight entity type {} does not have attributes defined!", type.getResourceKey());
        }
      }
    }

    if (CompatConstants.MOD_COBBLEMON_LOADED) {
      for (CobblemonEntityType type : CobblemonEntityType.values()) {
        if (type.getAttributes() != null) {
          FabricDefaultAttributeRegistry.register(
              (EntityType<? extends LivingEntity>) COBBLEMON_TYPE.get(type),
              ModEntityAttributes.buildWithNavigationAttributes(type));
        } else {
          log.warn(
              "Cobblemon entity type {} does not have attributes defined!", type.getResourceKey());
        }
      }
    }

    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      for (EasyModelEntitiesEntityType type : EasyModelEntitiesEntityType.values()) {
        if (type.getAttributes() != null) {
          FabricDefaultAttributeRegistry.register(
              (EntityType<? extends LivingEntity>) EASY_MODEL_ENTITIES_TYPE.get(type),
              ModEntityAttributes.buildWithNavigationAttributes(type));
        } else {
          log.warn(
              "Easy Model Entities entity type {} does not have attributes defined!",
              type.getResourceKey());
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
    return (EntityType<T>) EPIC_FIGHT_TYPE.get(type);
  }

  public static <T extends Entity> EntityType<T> getEntityType(CobblemonEntityType type) {
    if (!COBBLEMON_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid Cobblemon entity type '"
              + type
              + "'! Supported types are "
              + COBBLEMON_TYPE.keySet());
    }
    return (EntityType<T>) COBBLEMON_TYPE.get(type);
  }

  public static <T extends Entity> EntityType<T> getEntityType(EasyModelEntitiesEntityType type) {
    if (!EASY_MODEL_ENTITIES_TYPE.containsKey(type)) {
      throw new IllegalArgumentException(
          "Invalid Easy Model Entities entity type '"
              + type
              + "'! Supported types are "
              + EASY_MODEL_ENTITIES_TYPE.keySet());
    }
    return (EntityType<T>) EASY_MODEL_ENTITIES_TYPE.get(type);
  }
}
