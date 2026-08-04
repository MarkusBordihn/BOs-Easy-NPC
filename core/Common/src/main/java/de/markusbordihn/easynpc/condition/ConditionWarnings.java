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

package de.markusbordihn.easynpc.condition;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ConditionWarnings {
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Set<String> reportedSources = ConcurrentHashMap.newKeySet();

  private ConditionWarnings() {}

  public static boolean shouldReport(String source) {
    return source != null && reportedSources.add(source);
  }

  public static void reportMissingPlayer(
      ConditionDataEntry conditionDataEntry, LivingEntity npcContext) {
    String entityType = npcContext != null ? npcContext.getType().toString() : "unknown";
    if (shouldReport("missingPlayer:" + entityType + ":" + conditionDataEntry.conditionType())) {
      log.warn(
          "Condition {} of {} needs a player and is never met for an event without one.",
          conditionDataEntry.conditionType(),
          entityType);
    }
  }

  public static void reset() {
    reportedSources.clear();
  }
}
