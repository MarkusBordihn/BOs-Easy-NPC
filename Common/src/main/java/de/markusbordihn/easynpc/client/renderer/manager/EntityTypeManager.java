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

package de.markusbordihn.easynpc.client.renderer.manager;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.RenderEntityTypeSupportConfig;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.core.Registry;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EntityTypeManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Entity Type Manager]";

  private static final Set<EntityType<? extends Entity>> unknownEntityTypes = new HashSet<>();
  private static final Set<EntityType<? extends Entity>> supportedEntityTypes = new HashSet<>();
  private static final Set<EntityType<? extends Entity>> unsupportedEntityTypes = new HashSet<>();
  private static final Map<EntityType<? extends Entity>, PathfinderMob> pathfinderMobMap =
      new ConcurrentHashMap<>();
  private static final Map<EntityType<? extends Entity>, Float> scaleFactorMap =
      new ConcurrentHashMap<>();

  private EntityTypeManager() {}

  public static void register() {

    // Add known supported entity types and exclude unsupported entity types.
    Registry.ENTITY_TYPE.forEach(
        entityType -> {
          if (entityType == null) {
            return;
          }

          // Exclude our own entity types and other unsupported entity types.
          String entityTypeLocation = Registry.ENTITY_TYPE.getKey(entityType).toString();
          if (entityTypeLocation.startsWith(Constants.MOD_ID)
              || entityTypeLocation.startsWith("mythicmounts:")
              || entityTypeLocation.endsWith("_arrow")
              || entityTypeLocation.endsWith("_projectile")
              || entityTypeLocation.endsWith("_thrown")
              || entityTypeLocation.endsWith("_ball")
              || entityTypeLocation.endsWith("_bullet")
              || entityTypeLocation.endsWith("_fireball")
              || entityTypeLocation.endsWith("_boat")
              || entityTypeLocation.endsWith("_part")
              || entityTypeLocation.endsWith("effect")
              || entityTypeLocation.contains(":projectile")
              || entityTypeLocation.contains("_attack")
              || entityTypeLocation.contains("multi_part")
              || entityTypeLocation.contains("effect_")
              || entityTypeLocation.contains("flash_")
              || entityTypeLocation.contains(":spell_")) {
            return;
          }

          // Check if configuration covers the entity type to avoid expensive testing.
          if (RenderEntityTypeSupportConfig.isSupportedEntityType(entityTypeLocation)) {
            addSupportedEntityType(entityType);
          } else if (RenderEntityTypeSupportConfig.isUnsupportedEntityType(entityTypeLocation)) {
            addUnsupportedEntityType(entityType);
          } else {
            addUnknownEntityType(entityType);
          }
        });

    log.info(
        LOG_PREFIX + " Found {} supported, {} unsupported and {} unknown entity types.",
        supportedEntityTypes.size(),
        unsupportedEntityTypes.size(),
        unknownEntityTypes.size());
  }

  public static void addSupportedEntityType(EntityType<?> entityType) {
    supportedEntityTypes.add(entityType);
    unsupportedEntityTypes.remove(entityType);
    unknownEntityTypes.remove(entityType);
    scaleFactorMap.computeIfAbsent(entityType, EntityTypeManager::calculateScaleFactor);
  }

  public static void addUnsupportedEntityType(EntityType<?> entityType) {
    unsupportedEntityTypes.add(entityType);
    supportedEntityTypes.remove(entityType);
    unknownEntityTypes.remove(entityType);
    scaleFactorMap.remove(entityType);
  }

  public static void addUnknownEntityType(EntityType<?> entityType) {
    unknownEntityTypes.add(entityType);
    supportedEntityTypes.remove(entityType);
    unsupportedEntityTypes.remove(entityType);
    scaleFactorMap.computeIfAbsent(entityType, EntityTypeManager::calculateScaleFactor);
  }

  public static boolean isSupportedEntityType(EntityType<?> entityType) {
    return supportedEntityTypes.contains(entityType);
  }

  public static boolean isUnsupportedEntityType(EntityType<?> entityType) {
    return unsupportedEntityTypes.contains(entityType);
  }

  public static Set<EntityType<? extends Entity>> getUnknownEntityTypes() {
    return unknownEntityTypes;
  }

  public static Set<EntityType<? extends Entity>> getSupportedEntityTypes() {
    return supportedEntityTypes;
  }

  public static Set<EntityType<? extends Entity>> getUnsupportedEntityTypes() {
    return unsupportedEntityTypes;
  }

  public static List<EntityType<? extends Entity>> getUnknownAndSupportedEntityTypes() {
    ArrayList<EntityType<? extends Entity>> arrayList = new ArrayList<>();
    arrayList.addAll(supportedEntityTypes);
    arrayList.addAll(unknownEntityTypes);
    return arrayList;
  }

  public static float calculateScaleFactor(EntityType<? extends Entity> entityType) {
    EntityDimensions entityDimensions = entityType.getDimensions();
    float scaleFactor = 1.0f;
    float defaultWidth = 0.8f;
    float defaultHeight = 2f;
    float width = entityDimensions.width;
    float height = entityDimensions.height;
    if (width > defaultWidth || height > defaultHeight) {
      scaleFactor = Math.max(width / defaultWidth, height / defaultHeight);
    }
    return scaleFactor;
  }

  public static void updateUnknownEntityType(Level level) {
    // Process unknown entity but only one of a time to avoid performance issues.
    if (unknownEntityTypes.isEmpty()) {
      return;
    }

    EntityType<? extends Entity> entityType = unknownEntityTypes.iterator().next();
    if (entityType != null) {
      checkEntityType(entityType, level);
    }
  }

  public static boolean checkEntityType(EntityType<?> entityType, Level level) {
    return getPathfinderMob(entityType, level) != null;
  }

  public static PathfinderMob getPathfinderMob(EntityType<?> entityType, Level level) {
    if (entityType == null) {
      return null;
    }

    // Check if entity type is already registered and still valid.
    PathfinderMob pathfinderMob = pathfinderMobMap.get(entityType);
    if (pathfinderMob != null) {
      if (pathfinderMob.isAlive()) {
        // Update level if it has changed to avoid dimension issues.
        if (pathfinderMob.level != level) {
          pathfinderMob.level = level;
        }
        return pathfinderMob;
      } else {
        log.debug("{} PathfinderMob {} is removed, re-creating it.", LOG_PREFIX, pathfinderMob);
        pathfinderMobMap.remove(entityType);
      }
    }

    // Check if entity type is supported and created entity is a PathfinderMob.
    if (!isUnsupportedEntityType(entityType)) {
      Entity entity = entityType.create(level);
      if (entity instanceof PathfinderMob newPathfinderMob) {
        log.debug(
            "{} Registering PathfinderMob {} for {}", LOG_PREFIX, newPathfinderMob, entityType);

        // For better performance we disable AI, sound and physics for the fake entity.
        newPathfinderMob.setNoAi(true);
        newPathfinderMob.setSilent(true);
        newPathfinderMob.noPhysics = true;

        // Register new PathfinderMob for entity type.
        pathfinderMobMap.put(entityType, newPathfinderMob);

        // Make sure to add supported entity type if it was unknown before.
        if (!isSupportedEntityType(entityType)) {
          addSupportedEntityType(entityType);
        }
        return newPathfinderMob;
      } else {
        log.error(
            "{} Invalid Entity type {} is not extending PathfinderMob!", LOG_PREFIX, entityType);
        if (entity != null) {
          entity.discard();
        }
        addUnsupportedEntityType(entityType);
      }
    }

    return null;
  }

  public static float getScaleFactor(EntityType<? extends Entity> entityType) {
    return scaleFactorMap.getOrDefault(entityType, 1.0f);
  }
}
