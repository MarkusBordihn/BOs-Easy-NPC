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

package de.markusbordihn.easynpc.data.synched;

import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SynchedEntityData {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final Entity entity;
  private final Class<? extends Entity> entityClass;
  private final Map<SynchedDataIndex, EntityDataAccessor<?>> entityDataAccessorMap;

  public SynchedEntityData(
      Entity entity, Map<SynchedDataIndex, EntityDataAccessor<?>> entityDataAccessorMap) {
    this.entity = entity;
    this.entityClass = entity.getClass();
    this.entityDataAccessorMap = entityDataAccessorMap;
    log.debug(
        "Initialized SynchedEntityData for {} ({}) with {}",
        entity,
        entityClass,
        this.entityDataAccessorMap);
  }

  public <T> void define(SynchedDataIndex synchedDataIndex, T defaultData) {
    EntityDataAccessor<T> entityDataAccessor =
        (EntityDataAccessor<T>) this.entityDataAccessorMap.get(synchedDataIndex);
    try {
      this.entity.getEntityData().define(entityDataAccessor, defaultData);
    } catch (IllegalArgumentException exception) {
      if (exception.getMessage() != null
          && exception.getMessage().startsWith("Duplicate id value for")) {
        log.error(
            "Unable to register synced Easy NPC data {} with ID {} for {} because this ID is "
                + "already in use. This is a mod conflict. Another mod most likely registered a "
                + "fixed entity data ID instead of using Minecraft's automatic ID allocation. "
                + "Easy NPC cannot create this entity.",
            synchedDataIndex,
            entityDataAccessor.getId(),
            this.entityClass.getName());
      }
      throw exception;
    }
  }

  public <T> T get(SynchedDataIndex synchedDataIndex) {
    EntityDataAccessor<T> entityDataAccessor =
        (EntityDataAccessor<T>) this.entityDataAccessorMap.get(synchedDataIndex);
    return this.entity.getEntityData().get(entityDataAccessor);
  }

  public <T> void set(SynchedDataIndex synchedDataIndex, T data) {
    this.set(synchedDataIndex, data, false);
  }

  public <T> void set(SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {
    EntityDataAccessor<T> entityDataAccessor =
        (EntityDataAccessor<T>) this.entityDataAccessorMap.get(synchedDataIndex);
    this.entity.getEntityData().set(entityDataAccessor, data, forceUpdate);
  }
}
