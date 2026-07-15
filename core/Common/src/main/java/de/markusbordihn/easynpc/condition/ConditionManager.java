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
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import java.util.Set;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ConditionManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ConditionManager() {}

  public static boolean evaluate(ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer) {
    return evaluate(conditionDataEntry, serverPlayer, null, null);
  }

  public static boolean evaluate(
      ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer, ExecutionId executionId) {
    return evaluate(conditionDataEntry, serverPlayer, executionId, null);
  }

  public static boolean evaluate(
      ConditionDataEntry conditionDataEntry,
      ServerPlayer serverPlayer,
      ExecutionId executionId,
      LivingEntity npcContext) {
    if (conditionDataEntry == null || !conditionDataEntry.isValid() || serverPlayer == null) {
      return false;
    }

    return switch (conditionDataEntry.conditionType()) {
      case SCOREBOARD -> ScoreboardCondition.evaluate(conditionDataEntry, serverPlayer);
      case EXECUTION_LIMIT ->
          ExecutionLimitCondition.evaluate(conditionDataEntry, serverPlayer, executionId);
      case HAS_ITEM_IN_INVENTORY ->
          HasItemInInventoryCondition.evaluate(conditionDataEntry, serverPlayer);
      case HAS_ITEM_IN_HAND -> HasItemInHandCondition.evaluate(conditionDataEntry, serverPlayer);
      case ADVANCEMENT -> AdvancementCondition.evaluate(conditionDataEntry, serverPlayer);
      case EXPERIENCE_LEVEL -> ExperienceLevelCondition.evaluate(conditionDataEntry, serverPlayer);
      case PLAYER_HEALTH -> PlayerHealthCondition.evaluate(conditionDataEntry, serverPlayer);
      case NPC_HEALTH ->
          HealthConditionEvaluator.evaluate(
              conditionDataEntry.operationType(), conditionDataEntry.value(), npcContext);
      case ENTITY_HEALTH ->
          HealthConditionEvaluator.evaluate(
              conditionDataEntry.operationType(),
              conditionDataEntry.value(),
              HealthConditionEvaluator.resolveByUuid(npcContext, conditionDataEntry.name()));
      case PLAYER_TAG -> PlayerTagCondition.evaluate(conditionDataEntry, serverPlayer);
      case TEAM -> TeamCondition.evaluate(conditionDataEntry, serverPlayer);
      case GAMEMODE -> GamemodeCondition.evaluate(conditionDataEntry, serverPlayer);
      case TIME_OF_DAY -> TimeOfDayCondition.evaluate(conditionDataEntry, serverPlayer.level());
      case WEATHER -> WeatherCondition.evaluate(conditionDataEntry, serverPlayer.level());
      case FALLBACK -> true;
      case NONE -> true;
    };
  }

  public static boolean evaluateAll(
      Set<ConditionDataEntry> conditionDataEntries,
      ServerPlayer serverPlayer,
      ExecutionId executionId) {
    return evaluateAll(conditionDataEntries, serverPlayer, executionId, null);
  }

  public static boolean evaluateAll(
      Set<ConditionDataEntry> conditionDataEntries,
      ServerPlayer serverPlayer,
      ExecutionId executionId,
      LivingEntity npcContext) {
    if (conditionDataEntries == null || conditionDataEntries.isEmpty() || serverPlayer == null) {
      return true;
    }

    for (ConditionDataEntry conditionDataEntry : conditionDataEntries) {
      if (!evaluate(conditionDataEntry, serverPlayer, executionId, npcContext)) {
        log.debug("Condition not met: {}", conditionDataEntry);
        return false;
      }
    }
    return true;
  }

  public static void recordExecution(
      ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer, ExecutionId executionId) {
    if (conditionDataEntry == null
        || conditionDataEntry.conditionType() != ConditionType.EXECUTION_LIMIT
        || serverPlayer == null
        || executionId == null) {
      return;
    }

    ExecutionLimitCondition.recordExecution(conditionDataEntry, serverPlayer, executionId);
  }

  public static void recordExecutions(
      Set<ConditionDataEntry> conditionDataEntries,
      ServerPlayer serverPlayer,
      ExecutionId executionId) {
    if (conditionDataEntries == null || conditionDataEntries.isEmpty()) {
      return;
    }

    for (ConditionDataEntry conditionDataEntry : conditionDataEntries) {
      recordExecution(conditionDataEntry, serverPlayer, executionId);
    }
  }
}
