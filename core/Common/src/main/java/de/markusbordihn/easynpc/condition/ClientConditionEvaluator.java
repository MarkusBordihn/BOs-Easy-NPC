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

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import java.util.Set;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;

public class ClientConditionEvaluator {

  private ClientConditionEvaluator() {}

  public static boolean evaluateAll(Set<ConditionDataEntry> conditionDataEntries, Player player) {
    return evaluateAll(conditionDataEntries, player, null);
  }

  public static boolean evaluateAll(
      Set<ConditionDataEntry> conditionDataEntries, Player player, LivingEntity npcContext) {
    if (conditionDataEntries == null || conditionDataEntries.isEmpty() || player == null) {
      return true;
    }

    for (ConditionDataEntry conditionDataEntry : conditionDataEntries) {
      if (!evaluate(conditionDataEntry, player, npcContext)) {
        return false;
      }
    }
    return true;
  }

  private static boolean evaluate(
      ConditionDataEntry conditionDataEntry, Player player, LivingEntity npcContext) {
    if (conditionDataEntry == null || !conditionDataEntry.isValid()) {
      return true;
    }

    return switch (conditionDataEntry.conditionType()) {
      case HAS_ITEM_IN_INVENTORY ->
          HasItemInInventoryCondition.evaluate(conditionDataEntry, player);
      case HAS_ITEM_IN_HAND -> HasItemInHandCondition.evaluate(conditionDataEntry, player);
      case EXPERIENCE_LEVEL -> ExperienceLevelCondition.evaluate(conditionDataEntry, player);
      case PLAYER_HEALTH -> PlayerHealthCondition.evaluate(conditionDataEntry, player);
      case NPC_HEALTH ->
          npcContext == null
              || HealthConditionEvaluator.evaluate(
                  conditionDataEntry.operationType(), conditionDataEntry.value(), npcContext);
      case ENTITY_HEALTH -> evaluateEntityHealth(conditionDataEntry, npcContext);
      case TIME_OF_DAY -> TimeOfDayCondition.evaluate(conditionDataEntry, player.level());
      case WEATHER -> WeatherCondition.evaluate(conditionDataEntry, player.level());
      case RELATIONSHIP ->
          RelationshipCondition.evaluateOnClient(conditionDataEntry, player, npcContext);
      case CUSTOM -> CustomCondition.evaluateOnClient(conditionDataEntry, player, npcContext);
      default -> true;
    };
  }

  private static boolean evaluateEntityHealth(
      ConditionDataEntry conditionDataEntry, LivingEntity npcContext) {
    // Resolve the target Easy NPC by UUID on the client. If it is not loaded here, defer to the
    // server (do not lock) since the server remains authoritative for this condition.
    LivingEntity target =
        HealthConditionEvaluator.resolveByUuid(npcContext, conditionDataEntry.name());
    return target == null
        || HealthConditionEvaluator.evaluate(
            conditionDataEntry.operationType(), conditionDataEntry.value(), target);
  }
}
