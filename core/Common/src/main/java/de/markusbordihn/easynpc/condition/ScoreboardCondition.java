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
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.ScoreAccess;
import net.minecraft.world.scores.Scoreboard;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ScoreboardCondition {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ScoreboardCondition() {}

  public static boolean evaluate(ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer) {
    if (!conditionDataEntry.hasName() || serverPlayer == null) {
      return false;
    }

    try {
      Scoreboard scoreboard = serverPlayer.level().getScoreboard();
      Objective objective = scoreboard.getObjective(conditionDataEntry.name());
      if (objective == null) {
        if (ConditionWarnings.shouldReport("scoreboard:" + conditionDataEntry.name())) {
          log.warn(
              "Scoreboard objective '{}' does not exist, every dialog and action using it stays hidden.",
              conditionDataEntry.name());
        }
        return false;
      }

      ScoreAccess score = scoreboard.getOrCreatePlayerScore(serverPlayer, objective);
      int scoreValue = score.get();
      boolean result =
          conditionDataEntry.operationType().evaluate(scoreValue, conditionDataEntry.value());
      log.debug(
          "Scoreboard evaluation: {} {} {} = {} (actual: {})",
          conditionDataEntry.name(),
          conditionDataEntry.operationType().getSymbol(),
          conditionDataEntry.value(),
          result,
          scoreValue);
      return result;
    } catch (Exception e) {
      log.error("Error evaluating scoreboard condition: {}", conditionDataEntry, e);
      return false;
    }
  }
}
