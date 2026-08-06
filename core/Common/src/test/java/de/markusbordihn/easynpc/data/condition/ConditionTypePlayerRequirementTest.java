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

package de.markusbordihn.easynpc.data.condition;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.EnumSet;
import java.util.Set;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class ConditionTypePlayerRequirementTest {

  private static final Set<ConditionType> PLAYER_INDEPENDENT_TYPES =
      EnumSet.of(
          ConditionType.NONE,
          ConditionType.NPC_HEALTH,
          ConditionType.ENTITY_HEALTH,
          ConditionType.NPC_STATE,
          ConditionType.TIME_OF_DAY,
          ConditionType.WEATHER,
          ConditionType.CHANCE,
          ConditionType.FALLBACK,
          ConditionType.CUSTOM);

  @ParameterizedTest
  @EnumSource(ConditionType.class)
  @DisplayName("Every condition type states whether it needs a player")
  void requiresPlayer(ConditionType conditionType) {
    if (PLAYER_INDEPENDENT_TYPES.contains(conditionType)) {
      assertFalse(conditionType.requiresPlayer(), conditionType + " should work without a player");
    } else {
      assertTrue(conditionType.requiresPlayer(), conditionType + " should need a player");
    }
  }

  @Test
  @DisplayName("The relationship condition is answered against a player")
  void relationshipNeedsPlayer() {
    assertTrue(ConditionType.RELATIONSHIP.requiresPlayer());
  }
}
