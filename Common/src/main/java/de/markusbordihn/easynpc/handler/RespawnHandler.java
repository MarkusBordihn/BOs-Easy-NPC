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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RespawnHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private RespawnHandler() {}

  public static boolean respawnNPC(EasyNPC<?> easyNPC, ServerLevel serverLevel) {
    if (easyNPC == null || serverLevel == null) {
      log.error("[{}] Error respawning NPC.", easyNPC);
      return false;
    }

    // Save entity and entity type
    CompoundTag compoundTag = easyNPC.getEntity().saveWithoutId(new CompoundTag());
    EntityType<?> entityType = easyNPC.getEntity().getType();

    // Create new entity with compoundTag
    Entity entity = entityType.create(serverLevel);
    if (entity == null) {
      log.error(
          "[{}] Unable to create new entity with type {} with {}",
          easyNPC,
          entityType,
          serverLevel);
      return false;
    }
    entity.load(compoundTag);

    // Remove old entity
    easyNPC.getEntity().discard();

    // Respawn new entity
    log.info("[{}] Respawn Easy NPC with {} into {}", easyNPC, entityType, serverLevel);
    serverLevel.addFreshEntity(entity);
    return true;
  }
}
