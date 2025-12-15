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

package de.markusbordihn.easynpc.data.condition;

import de.markusbordihn.easynpc.Constants;
import java.util.Set;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.ScoreAccess;
import net.minecraft.world.scores.Scoreboard;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ConditionUtils {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ConditionUtils() {}

  public static boolean evaluateCondition(
      ConditionDataEntry conditionDataEntry, ServerPlayer player) {
    if (conditionDataEntry == null || !conditionDataEntry.isValid() || player == null) {
      return false;
    }

    return switch (conditionDataEntry.conditionType()) {
      case SCOREBOARD -> evaluateScoreboardCondition(conditionDataEntry, player);
      case NONE -> true;
    };
  }

  public static boolean evaluateConditions(
      Set<ConditionDataEntry> conditions, ServerPlayer player) {
    if (conditions == null || conditions.isEmpty() || player == null) {
      return true;
    }

    for (ConditionDataEntry condition : conditions) {
      if (!evaluateCondition(condition, player)) {
        log.debug("Condition not met: {}", condition);
        return false;
      }
    }

    return true;
  }

  public static boolean evaluateScoreboardCondition(
      ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer) {
    if (!conditionDataEntry.hasName() || serverPlayer == null) {
      return false;
    }

    try {
      Scoreboard scoreboard = serverPlayer.getScoreboard();

      Objective objective = scoreboard.getObjective(conditionDataEntry.name());
      if (objective == null) {
        log.debug(
            "Scoreboard objective '{}' not found for player {}",
            conditionDataEntry.name(),
            serverPlayer.getName().getString());
        return false;
      }

      ScoreAccess scoreAccess = scoreboard.getOrCreatePlayerScore(serverPlayer, objective);
      int scoreValue = scoreAccess.get();
      int targetValue = conditionDataEntry.value();

      boolean result = conditionDataEntry.operationType().evaluate(scoreValue, targetValue);

      log.debug(
          "Scoreboard evaluation: {} {} {} = {} (actual: {})",
          conditionDataEntry.name(),
          conditionDataEntry.operationType().getSymbol(),
          targetValue,
          result,
          scoreValue);

      return result;
    } catch (Exception e) {
      log.error("Error evaluating scoreboard condition: {}", conditionDataEntry, e);
      return false;
    }
  }
}
