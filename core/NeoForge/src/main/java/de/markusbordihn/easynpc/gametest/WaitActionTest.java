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

import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

public final class WaitActionTest {

  private WaitActionTest() {}

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  public static void testWaitDelaysTheFollowingActions(GameTestHelper helper) {
    WaitActionTestHelper.assertWaitDelaysTheFollowingActions(helper, humanoid());
    helper.succeed();
  }

  public static void testRemainingWaitSurvivesSaveAndLoad(GameTestHelper helper) {
    WaitActionTestHelper.assertRemainingWaitSurvivesSaveAndLoad(helper, humanoid());
    helper.succeed();
  }

  public static void testSecondTriggerIsDiscarded(GameTestHelper helper) {
    WaitActionTestHelper.assertSecondTriggerIsDiscarded(helper, humanoid());
    helper.succeed();
  }

  public static void testDifferentEventsRunInParallel(GameTestHelper helper) {
    WaitActionTestHelper.assertDifferentEventsRunInParallel(helper, humanoid());
    helper.succeed();
  }

  public static void testIntervalSetWithWaitRunsInOrder(GameTestHelper helper) {
    WaitActionTestHelper.assertIntervalSetWithWaitRunsInOrder(helper, humanoid());
    helper.succeed();
  }

  public static void testIntervalSetWithoutWaitPicksOneEntry(GameTestHelper helper) {
    WaitActionTestHelper.assertIntervalSetWithoutWaitPicksOneEntry(helper, humanoid());
    helper.succeed();
  }

  public static void testScreenActionIsKeptOverTheWait(GameTestHelper helper) {
    WaitActionTestHelper.assertScreenActionIsKeptOverTheWait(helper, humanoid());
    helper.succeed();
  }

  public static void testFallbackRunsOnceAfterTheWait(GameTestHelper helper) {
    WaitActionTestHelper.assertFallbackRunsOnceAfterTheWait(helper, humanoid());
    helper.succeed();
  }

  public static void testPresetImportCancelsTheChain(GameTestHelper helper) {
    WaitActionTestHelper.assertPresetImportCancelsTheChain(helper, humanoid());
    helper.succeed();
  }

  public static void testDeathCancelsTheChain(GameTestHelper helper) {
    WaitActionTestHelper.assertDeathCancelsTheChain(helper, humanoid());
    helper.succeed();
  }
}
