/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.condition;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.UUID;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class HealthConditionEvaluator {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private HealthConditionEvaluator() {}

  public static boolean evaluate(
      ConditionOperationType operationType, int value, LivingEntity target) {
    if (operationType == null || target == null) {
      return false;
    }

    int healthPercent = (int) ((target.getHealth() / target.getMaxHealth()) * 100);
    return operationType.evaluate(healthPercent, value);
  }

  public static LivingEntity resolveByUuid(LivingEntity context, String uuidName) {
    if (context == null || uuidName == null || uuidName.isEmpty()) {
      return null;
    }

    UUID targetUuid;
    try {
      targetUuid = UUID.fromString(uuidName.trim());
    } catch (IllegalArgumentException ignored) {
      log.debug("Invalid entity UUID {} for health condition", uuidName);
      return null;
    }

    // Server side: resolve any living entity (players, NPCs, mobs) from the level.
    if (context.level() instanceof ServerLevel serverLevel) {
      return LivingEntityManager.getLivingEntityByUUID(targetUuid, serverLevel);
    }

    // Client side: best-effort resolution of Easy NPC entities (UUIDs match across sides).
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(targetUuid);
    return easyNPC != null ? easyNPC.getLivingEntity() : null;
  }
}
