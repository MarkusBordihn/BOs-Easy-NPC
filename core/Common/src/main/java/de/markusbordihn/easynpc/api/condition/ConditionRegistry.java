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

package de.markusbordihn.easynpc.api.condition;

import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ConditionRegistry {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Map<ResourceLocation, ConditionEvaluator> conditionEvaluators =
      new ConcurrentHashMap<>();

  private ConditionRegistry() {}

  public static void register(ResourceLocation conditionId, ConditionEvaluator conditionEvaluator) {
    if (conditionId == null || conditionEvaluator == null) {
      log.error(
          "Unable to register condition {} with evaluator {}", conditionId, conditionEvaluator);
      return;
    }

    ConditionEvaluator previousEvaluator =
        conditionEvaluators.putIfAbsent(conditionId, conditionEvaluator);
    if (previousEvaluator != null) {
      log.error("Condition {} is already registered by {}", conditionId, previousEvaluator);
      return;
    }

    log.info("Registered custom condition {}", conditionId);
  }

  public static ConditionEvaluator get(ResourceLocation conditionId) {
    return conditionId != null ? conditionEvaluators.get(conditionId) : null;
  }

  public static boolean isRegistered(ResourceLocation conditionId) {
    return get(conditionId) != null;
  }

  public static boolean isEvaluatedOnClient(ResourceLocation conditionId) {
    ConditionEvaluator conditionEvaluator = get(conditionId);
    return conditionEvaluator != null && conditionEvaluator.isAvailableOnClient();
  }
}
