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

package de.markusbordihn.easynpc.gametest;

import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.humanoid;

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import net.minecraft.gametest.framework.GameTestHelper;

public final class ObjectiveApiTest {

  private ObjectiveApiTest() {}

  public static void testOwnerObjectiveWithoutOwnerCanBeRemoved(GameTestHelper helper) {
    ObjectiveApiTestHelper.assertOwnerObjectiveWithoutOwnerCanBeRemoved(helper, humanoid());
    helper.succeed();
  }

  public static void testFollowOwnerIsAddedAndRemovedOnce(GameTestHelper helper) {
    ObjectiveApiTestHelper.assertObjectiveIsAddedAndRemovedOnce(
        helper, humanoid(), ObjectiveType.FOLLOW_OWNER);
    helper.succeed();
  }

  public static void testFollowNearestPlayerIsAddedAndRemovedOnce(GameTestHelper helper) {
    ObjectiveApiTestHelper.assertObjectiveIsAddedAndRemovedOnce(
        helper, humanoid(), ObjectiveType.FOLLOW_NEAREST_PLAYER);
    helper.succeed();
  }

  public static void testObjectiveEventsFireOncePerChange(GameTestHelper helper) {
    ObjectiveApiTestHelper.assertObjectiveEventsFireOncePerChange(helper, humanoid());
    helper.succeed();
  }

  public static void testExplicitPriorityReachesGoalSelector(GameTestHelper helper) {
    ObjectiveApiTestHelper.assertExplicitPriorityReachesGoalSelector(helper, humanoid());
    helper.succeed();
  }

  public static void testMeleeAttackUsesAttackSpeed(GameTestHelper helper) {
    ObjectiveApiTestHelper.assertMeleeAttackUsesAttackSpeed(helper, humanoid());
    helper.succeed();
  }
}
