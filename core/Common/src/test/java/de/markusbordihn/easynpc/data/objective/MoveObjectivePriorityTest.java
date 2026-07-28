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

package de.markusbordihn.easynpc.data.objective;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class MoveObjectivePriorityTest {

  private static void assertWins(ObjectiveType winner, ObjectiveType loser) {
    assertTrue(
        winner.getDefaultPriority() < loser.getDefaultPriority(),
        winner + " must win over " + loser);
  }

  @Test
  @DisplayName("Following the owner wins over returning home, which wins over strolling")
  void testMoveObjectivesAreOrderedByIntent() {
    assertWins(ObjectiveType.PANIC, ObjectiveType.FOLLOW_OWNER);
    assertWins(ObjectiveType.FOLLOW_OWNER, ObjectiveType.MOVE_BACK_TO_HOME);
    assertWins(ObjectiveType.FOLLOW_PLAYER, ObjectiveType.MOVE_BACK_TO_HOME);
    assertWins(ObjectiveType.MOVE_BACK_TO_HOME, ObjectiveType.RANDOM_STROLL);
    assertWins(ObjectiveType.MOVE_BACK_TO_HOME, ObjectiveType.RANDOM_STROLL_AROUND_HOME);
    assertWins(ObjectiveType.MOVE_BACK_TO_VILLAGE, ObjectiveType.RANDOM_STROLL_IN_VILLAGE);
    assertWins(ObjectiveType.RANDOM_STROLL_AROUND_HOME, ObjectiveType.RANDOM_STROLL);
    assertWins(ObjectiveType.RANDOM_STROLL_AROUND_HOME, ObjectiveType.WATER_AVOIDING_RANDOM_STROLL);
  }
}
