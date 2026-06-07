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

package de.markusbordihn.easynpc.condition;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionSubTypeEntry;
import de.markusbordihn.easynpc.data.condition.DurationType;
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import de.markusbordihn.easynpc.data.saveddata.ActionExecutionTracker;
import java.util.UUID;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ExecutionLimitCondition {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ExecutionLimitCondition() {}

  public static boolean evaluate(
      ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer, UUID actionUUID) {
    if (serverPlayer == null || actionUUID == null) {
      return false;
    }

    int limit = conditionDataEntry.value();
    ExecutionInterval interval = toInterval(conditionDataEntry.subType());
    ActionExecutionTracker tracker = ActionExecutionTracker.get(serverPlayer.level());
    boolean canExecute = tracker.canExecute(serverPlayer.getUUID(), actionUUID, limit, interval);
    log.debug(
        "Execution limit check for player {} action {}: limit={}, interval={}, canExecute={}",
        serverPlayer.getGameProfile().name(),
        actionUUID,
        limit,
        interval,
        canExecute);
    return canExecute;
  }

  public static void recordExecution(
      ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer, UUID actionUUID) {
    if (serverPlayer == null || actionUUID == null) {
      return;
    }

    ExecutionInterval interval = toInterval(conditionDataEntry.subType());
    ActionExecutionTracker tracker = ActionExecutionTracker.get(serverPlayer.level());
    tracker.recordExecution(serverPlayer.getUUID(), actionUUID, interval);
  }

  private static ExecutionInterval toInterval(ConditionSubTypeEntry conditionSubTypeEntry) {
    if (conditionSubTypeEntry instanceof DurationType durationType) {
      return switch (durationType) {
        case PER_MINUTE -> ExecutionInterval.PER_MINUTE;
        case PER_HOUR -> ExecutionInterval.PER_HOUR;
        case PER_WEEK -> ExecutionInterval.PER_WEEK;
        case PER_MONTH -> ExecutionInterval.PER_MONTH;
        case LIFETIME -> ExecutionInterval.LIFETIME;
        default -> ExecutionInterval.PER_DAY;
      };
    }
    return ExecutionInterval.PER_DAY;
  }
}
