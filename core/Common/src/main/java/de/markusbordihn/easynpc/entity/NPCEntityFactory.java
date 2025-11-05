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
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.SkeletonRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.StrayRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.VillagerRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.WitherSkeletonRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.ZombieRaw;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NPCEntityFactory {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<EntityType<?>, BiFunction<EntityType<?>, Level, Entity>>
      entityConstructors = new HashMap<>();

  static {
    initializeEntityConstructors();
  }

  private NPCEntityFactory() {}

  public static Entity createEntityFromBaseType(
      EntityType<?> baseEntityType, EntityType<?> targetEntityType, Level level) {

    BiFunction<EntityType<?>, Level, Entity> constructor = entityConstructors.get(baseEntityType);
    if (constructor == null) {
      throw new IllegalArgumentException(
          "Unsupported base entity type: "
              + getEntityTypeId(baseEntityType)
              + ". Supported types are: "
              + getSupportedBaseTypeIds());
    }

    try {
      Entity entity = constructor.apply(targetEntityType, level);
      if (entity == null) {
        throw new IllegalStateException(
            "Constructor for base type "
                + getEntityTypeId(baseEntityType)
                + " returned null entity");
      }
      return entity;
    } catch (Exception e) {
      log.error(
          "Failed to create entity from base type {}: {}",
          getEntityTypeId(baseEntityType),
          e.getMessage(),
          e);
      throw new IllegalStateException(
          "Failed to create entity from base type " + getEntityTypeId(baseEntityType), e);
    }
  }

  public static boolean isBaseTypeSupported(EntityType<?> baseEntityType) {
    return entityConstructors.containsKey(baseEntityType);
  }

  public static String[] getSupportedBaseTypeIds() {
    return entityConstructors.keySet().stream()
        .map(NPCEntityFactory::getEntityTypeId)
        .toArray(String[]::new);
  }

  public static void registerEntityConstructor(
      EntityType<?> baseEntityType, BiFunction<EntityType<?>, Level, Entity> constructor) {
    entityConstructors.put(baseEntityType, constructor);
    log.debug("Registered entity constructor for base type: {}", getEntityTypeId(baseEntityType));
  }

  private static void initializeEntityConstructors() {
    registerEntityConstructor(
        EntityType.ZOMBIE, (type, level) -> new ZombieRaw((EntityType<ZombieRaw>) type, level));

    registerEntityConstructor(
        EntityType.VILLAGER,
        (type, level) -> new VillagerRaw((EntityType<VillagerRaw>) type, level));

    registerEntityConstructor(
        EntityType.SKELETON,
        (type, level) -> new SkeletonRaw((EntityType<SkeletonRaw>) type, level));

    registerEntityConstructor(
        EntityType.STRAY, (type, level) -> new StrayRaw((EntityType<StrayRaw>) type, level));

    registerEntityConstructor(
        EntityType.WITHER_SKELETON,
        (type, level) -> new WitherSkeletonRaw((EntityType<WitherSkeletonRaw>) type, level));
  }

  private static String getEntityTypeId(EntityType<?> entityType) {
    ResourceLocation resourceLocation = BuiltInRegistries.ENTITY_TYPE.getKey(entityType);
    return resourceLocation.toString();
  }
}
