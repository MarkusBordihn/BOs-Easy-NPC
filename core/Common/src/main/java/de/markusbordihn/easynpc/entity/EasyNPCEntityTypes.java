/*
 * Copyright 2026 Markus Bordihn
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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class EasyNPCEntityTypes {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Easy NPC Entity Types]";
  private static final String ENTITY_ID_TAG = "id";
  private static final Map<EntityType<?>, Boolean> easyNPCEntityTypes = new ConcurrentHashMap<>();

  private EasyNPCEntityTypes() {}

  public static boolean isEasyNPCEntityType(EntityType<?> entityType, Level level) {
    if (entityType == null) {
      return false;
    }

    Boolean cachedResult = easyNPCEntityTypes.get(entityType);
    if (cachedResult != null) {
      return cachedResult;
    }

    String namespace = EntityType.getKey(entityType).getNamespace();
    if (Identifier.DEFAULT_NAMESPACE.equals(namespace)) {
      return false;
    }

    if (Constants.MOD_ID.equals(namespace)) {
      return true;
    }

    if (level == null) {
      return false;
    }

    boolean isEasyNPC = createsEasyNPC(entityType, level);
    easyNPCEntityTypes.put(entityType, isEasyNPC);
    return isEasyNPC;
  }

  public static boolean isEasyNPCEntityTag(CompoundTag entityTag, Level level) {
    if (entityTag == null) {
      return false;
    }

    return entityTag
        .getString(ENTITY_ID_TAG)
        .map(Identifier::tryParse)
        .flatMap(BuiltInRegistries.ENTITY_TYPE::getOptional)
        .map(entityType -> isEasyNPCEntityType(entityType, level))
        .orElse(false);
  }

  public static void markEasyNPCEntityType(EntityType<?> entityType) {
    easyNPCEntityTypes.put(entityType, true);
  }

  public static void reset() {
    easyNPCEntityTypes.clear();
  }

  private static boolean createsEasyNPC(EntityType<?> entityType, Level level) {
    Entity entity;
    try {
      entity = entityType.create(level, EntitySpawnReason.LOAD);
    } catch (RuntimeException e) {
      log.warn("{} Unable to create {} for type check", LOG_PREFIX, entityType, e);
      return false;
    }
    if (entity == null) {
      return false;
    }

    boolean isEasyNPC = entity instanceof EasyNPC<?>;
    entity.discard();
    return isEasyNPC;
  }
}
