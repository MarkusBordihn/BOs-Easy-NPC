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
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EntityTypeManager {

  public static final float GUI_PREVIEW_TARGET_HEIGHT = 1.365f;
  public static final float GUI_PREVIEW_MAX_SCALE_FACTOR = 3.0f;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Entity Type Manager]";
  private static final Set<String> FILTERED_ENTITY_TYPE_PREFIXES =
      Set.of(Constants.MOD_ID, "mythicmounts:");
  private static final Set<String> FILTERED_ENTITY_TYPE_SUFFIXES =
      Set.of(
          ":boat",
          ":seat",
          "_arrow",
          "_ball",
          "_beam",
          "_blast",
          "_blob",
          "_boat",
          "_bolt",
          "_bomb",
          "_bubble",
          "_bullet",
          "_charge",
          "_cloud",
          "_contraption",
          "_crystal",
          "_dart",
          "_display",
          "_egg",
          "_fireball",
          "_flare",
          "_item",
          "_marker",
          "_missile",
          "_mortar",
          "_needle",
          "_orb",
          "_painting",
          "_parachute",
          "_part",
          "_pearl",
          "_pellet",
          "_piece",
          "_projectile",
          "_rocket",
          "_seat",
          "_shake",
          "_shard",
          "_shot",
          "_snowball",
          "_spawner",
          "_spear",
          "_spike",
          "_tentacle",
          "_thrown",
          "_vortex",
          "effect");
  private static final Set<String> FILTERED_ENTITY_TYPE_PARTS =
      Set.of(
          ":projectile",
          ":spell_",
          "_attack",
          "effect_",
          "falling_",
          "flash_",
          "hitbox",
          "minecart",
          "multi_part",
          "throwable_",
          "thrown_");
  private static final Set<EntityType<? extends Entity>> unknownEntityTypes = new HashSet<>();
  private static final Set<EntityType<? extends Entity>> supportedEntityTypes = new HashSet<>();
  private static final Set<EntityType<? extends Entity>> unsupportedEntityTypes = new HashSet<>();
  private static final Map<EntityType<? extends Entity>, Mob> renderEntityMap =
      new ConcurrentHashMap<>();
  private static final Map<EntityType<? extends Entity>, Float> scaleFactorMap =
      new ConcurrentHashMap<>();
  private static final Map<EntityType<? extends Entity>, String> entityTypeNameMap =
      new ConcurrentHashMap<>();
  private static boolean isRegistered = false;

  private EntityTypeManager() {}

  public static void register() {
    if (isRegistered) {
      log.warn("{} Already registered, skipping.", LOG_PREFIX);
      return;
    }

    log.info("{} Registering Entity Type Manager ...", Constants.LOG_REGISTER_PREFIX);
    Set<String> configuredSupportedTypes = RenderEntityTypeSupportConfig.getSupportedEntityTypes();
    Set<String> configuredUnsupportedTypes =
        RenderEntityTypeSupportConfig.getUnsupportedEntityTypes();
    if (configuredSupportedTypes.isEmpty() && configuredUnsupportedTypes.isEmpty()) {
      log.error("{} Config appears to be empty! This may cause autocomplete issues.", LOG_PREFIX);
    } else {
      log.info(
          "{} Config loaded with {} supported and {} unsupported entity types.",
          LOG_PREFIX,
          configuredSupportedTypes.size(),
          configuredUnsupportedTypes.size());
    }

    int autoFilteredCount = 0;
    for (EntityType<?> entityType : BuiltInRegistries.ENTITY_TYPE) {
      if (entityType == null) {
        continue;
      }

      // Skip non-mob entities like projectiles, vehicles, items, and displays.
      if (entityType.getCategory() == MobCategory.MISC) {
        autoFilteredCount++;
        continue;
      }

      String entityTypeLocation = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();

      // Explicitly configured entities always take priority over pattern filtering.
      if (configuredSupportedTypes.contains(entityTypeLocation)) {
        entityTypeNameMap.put(entityType, entityTypeLocation);
        addSupportedEntityType(entityType);
        continue;
      } else if (configuredUnsupportedTypes.contains(entityTypeLocation)) {
        entityTypeNameMap.put(entityType, entityTypeLocation);
        addUnsupportedEntityType(entityType);
        continue;
      }

      // Auto-filter unknown entities by common non-mob name patterns.
      if (shouldFilterEntityTypeByName(entityTypeLocation)) {
        autoFilteredCount++;
        continue;
      }

      entityTypeNameMap.put(entityType, entityTypeLocation);
      addUnknownEntityType(entityType);
    }

    log.info(
        LOG_PREFIX
            + " Found {} supported, {} unsupported, {} unknown and {} auto-filtered entity types.",
        supportedEntityTypes.size(),
        unsupportedEntityTypes.size(),
        unknownEntityTypes.size(),
        autoFilteredCount);

    isRegistered = true;
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
    float width = entityDimensions.width();
    float height = entityDimensions.height();
    if (width > defaultWidth || height > defaultHeight) {
      scaleFactor = Math.max(width / defaultWidth, height / defaultHeight);
    }

    return scaleFactor;
  }

  public static float calculateGuiPreviewScaleFactor(float entityHeight) {
    if (entityHeight <= 0f) {
      throw new IllegalArgumentException("entityHeight must be > 0, got: " + entityHeight);
    }
    return Math.min(GUI_PREVIEW_TARGET_HEIGHT / entityHeight, GUI_PREVIEW_MAX_SCALE_FACTOR);
  }

  public static float calculateGuiPreviewYLift(float entityHeight) {
    if (entityHeight <= 0f) {
      return 0f;
    }
    float scaleFactor = calculateGuiPreviewScaleFactor(entityHeight);
    return Math.max(0f, (GUI_PREVIEW_TARGET_HEIGHT - scaleFactor * entityHeight) / 2f);
  }

  public static PathfinderMob getPathfinderMob(EntityType<?> entityType, Level level) {
    return getRenderEntity(entityType, level) instanceof PathfinderMob pathfinderMob
        ? pathfinderMob
        : null;
  }

  public static Mob getRenderEntity(EntityType<?> entityType, Level level) {
    if (entityType == null) {
      return null;
    }

    // Check if entity type is already registered and still valid.
    Mob renderEntity = renderEntityMap.get(entityType);
    if (renderEntity != null) {
      if (renderEntity.isAlive()) {
        if (renderEntity.level() != level) {
          try {
            Field levelField = Entity.class.getDeclaredField("level");
            levelField.setAccessible(true);
            levelField.set(renderEntity, level);
          } catch (Exception e) {
            log.error("{} Failed to update level for render entity {}", LOG_PREFIX, renderEntity);
          }
        }
        return renderEntity;
      } else {
        log.debug("{} Render entity {} is removed, re-creating it.", LOG_PREFIX, renderEntity);
        renderEntityMap.remove(entityType);
      }
    }

    if (!isUnsupportedEntityType(entityType)) {
      Entity entity;
      try {
        entity = entityType.create(level);
      } catch (Exception e) {
        log.warn(
            "{} Failed to create entity for type {}: {}", LOG_PREFIX, entityType, e.getMessage());
        addUnsupportedEntityType(entityType);
        return null;
      }

      if (entity instanceof Mob newRenderEntity) {
        log.debug("{} Registering render entity {} for {}", LOG_PREFIX, newRenderEntity, entityType);

        // For better performance we disable AI, sound and physics for the fake entity.
        newRenderEntity.setNoAi(true);
        newRenderEntity.setSilent(true);
        newRenderEntity.noPhysics = true;

        renderEntityMap.put(entityType, newRenderEntity);

        // Make sure to add supported entity type if it was unknown before.
        if (!isSupportedEntityType(entityType)) {
          addSupportedEntityType(entityType);
        }
        return newRenderEntity;
      } else {
        log.debug("{} Entity type {} is not a Mob, marking as unsupported.", LOG_PREFIX, entityType);
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

  public static String getEntityTypeName(EntityType<? extends Entity> entityType) {
    return entityTypeNameMap.getOrDefault(entityType, "Unknown");
  }

  public static boolean shouldFilterEntityTypeByName(String entityTypeLocation) {
    if (entityTypeLocation == null || entityTypeLocation.isEmpty()) {
      return true;
    }

    for (String prefix : FILTERED_ENTITY_TYPE_PREFIXES) {
      if (entityTypeLocation.startsWith(prefix)) {
        return true;
      }
    }

    for (String suffix : FILTERED_ENTITY_TYPE_SUFFIXES) {
      if (entityTypeLocation.endsWith(suffix)) {
        return true;
      }
    }

    for (String part : FILTERED_ENTITY_TYPE_PARTS) {
      if (entityTypeLocation.contains(part)) {
        return true;
      }
    }

    return false;
  }
}
