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
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class UserDefinedEntityType implements ModEntityTypeProvider {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final String entityId;
  private final EntityType<?> baseEntityType;
  private final EntityType.Builder<? extends Entity> entityBuilder;
  private final ResourceKey<EntityType<?>> resourceKey;

  public UserDefinedEntityType(
      String entityId, EntityType<?> baseEntityType, float width, float height) {
    this.entityId = entityId;
    this.baseEntityType = baseEntityType;
    this.entityBuilder = createEntityBuilder(width, height);
    this.resourceKey = createResourceKey(entityId);
  }

  @Override
  public String getId() {
    return entityId;
  }

  @Override
  public EntityType.Builder<? extends Entity> getBuilder() {
    return entityBuilder;
  }

  @Override
  public ResourceKey<EntityType<?>> getResourceKey() {
    return resourceKey;
  }

  public EntityType<?> getBaseEntityType() {
    return baseEntityType;
  }

  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    }
    if (!(other instanceof UserDefinedEntityType that)) {
      return false;
    }
    return entityId.equals(that.entityId);
  }

  @Override
  public int hashCode() {
    return entityId.hashCode();
  }

  @Override
  public String toString() {
    return "UserDefinedEntityType{"
        + "entityId='"
        + entityId
        + '\''
        + ", baseEntityType='"
        + baseEntityType
        + '\''
        + '}';
  }

  private EntityType.Builder<? extends Entity> createEntityBuilder(float width, float height) {
    return EntityType.Builder.of(
            (entityType, level) -> {
              try {
                return NPCEntityFactory.createEntityFromBaseType(baseEntityType, entityType, level);
              } catch (Exception e) {
                log.error(
                    "Failed to create entity of type {} based on {}: {}",
                    entityId,
                    baseEntityType,
                    e.getMessage(),
                    e);
                return null;
              }
            },
            MobCategory.MISC)
        .sized(width, height)
        .clientTrackingRange(12);
  }

  private ResourceKey<EntityType<?>> createResourceKey(String entityId) {
    return ResourceKey.create(
        Registries.ENTITY_TYPE,
        ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID_CUSTOM, entityId));
  }
}
