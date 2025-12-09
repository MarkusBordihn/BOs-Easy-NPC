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

package de.markusbordihn.easynpc.configui.client.renderer.manager;

import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import java.util.ArrayList;
import java.util.Set;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.Level;

public class EntityTypeValidator {

  private static final int DEFAULT_BATCH_SIZE = 20;

  private EntityTypeValidator() {}

  public static void validateUnknownEntityTypes(Level level) {
    validateUnknownEntityTypes(level, DEFAULT_BATCH_SIZE);
  }

  public static void validateUnknownEntityTypes(Level level, int batchSize) {
    Set<EntityType<? extends Entity>> unknownEntityTypes =
        EntityTypeManager.getUnknownEntityTypes();

    if (unknownEntityTypes.isEmpty()) {
      return;
    }

    int processed = 0;
    for (EntityType<? extends Entity> entityType : new ArrayList<>(unknownEntityTypes)) {
      if (processed >= batchSize) {
        break;
      }

      if (entityType != null) {
        EntityTypeManager.getPathfinderMob(entityType, level);
        processed++;
      }
    }
  }
}
