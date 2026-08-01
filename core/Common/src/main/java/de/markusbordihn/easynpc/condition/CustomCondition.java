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
import de.markusbordihn.easynpc.api.condition.ConditionEvaluator;
import de.markusbordihn.easynpc.api.condition.ConditionRegistry;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomCondition {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CustomCondition() {}

  public static boolean evaluate(
      ConditionDataEntry conditionDataEntry, ServerPlayer serverPlayer, LivingEntity npcContext) {
    ConditionEvaluator conditionEvaluator = resolve(conditionDataEntry);
    if (conditionEvaluator == null) {
      return false;
    }

    try {
      return conditionEvaluator.evaluate(conditionDataEntry, serverPlayer, npcContext);
    } catch (Exception e) {
      log.error("Error evaluating custom condition {}", conditionDataEntry, e);
      return false;
    }
  }

  public static boolean evaluateOnClient(
      ConditionDataEntry conditionDataEntry, Player player, LivingEntity npcContext) {
    ConditionEvaluator conditionEvaluator =
        ConditionRegistry.get(conditionDataEntry.customConditionId());
    if (conditionEvaluator == null || !conditionEvaluator.isAvailableOnClient()) {
      return true;
    }

    try {
      return conditionEvaluator.evaluateOnClient(conditionDataEntry, player, npcContext);
    } catch (Exception e) {
      log.error("Error evaluating custom condition {} on client", conditionDataEntry, e);
      return true;
    }
  }

  private static ConditionEvaluator resolve(ConditionDataEntry conditionDataEntry) {
    Identifier customConditionId = conditionDataEntry.customConditionId();
    ConditionEvaluator conditionEvaluator = ConditionRegistry.get(customConditionId);
    if (conditionEvaluator == null
        && ConditionWarnings.shouldReport("condition:" + customConditionId)) {
      log.warn(
          "Custom condition {} is not registered, every dialog and action using it stays hidden.",
          customConditionId);
    }

    return conditionEvaluator;
  }
}
