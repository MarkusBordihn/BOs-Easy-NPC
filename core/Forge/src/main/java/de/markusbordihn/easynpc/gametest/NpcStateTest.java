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

public final class NpcStateTest {

  private NpcStateTest() {}

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  public static void testStateActionAppliesEveryOperation(GameTestHelper helper) {
    NpcStateTestHelper.assertStateActionAppliesEveryOperation(helper, humanoid());
    helper.succeed();
  }

  public static void testInvalidStateActionIsIgnored(GameTestHelper helper) {
    NpcStateTestHelper.assertInvalidStateActionIsIgnored(helper, humanoid());
    helper.succeed();
  }

  public static void testDebugActionStillWritesTheState(GameTestHelper helper) {
    NpcStateTestHelper.assertDebugActionStillWritesTheState(helper, humanoid());
    helper.succeed();
  }

  public static void testStateSurvivesSaveAndLoad(GameTestHelper helper) {
    NpcStateTestHelper.assertStateSurvivesSaveAndLoad(helper, humanoid());
    helper.succeed();
  }

  public static void testStateConditionLocksDialogButton(GameTestHelper helper) {
    NpcStateTestHelper.assertStateConditionLocksDialogButton(helper, humanoid());
    helper.succeed();
  }

  public static void testStateIsResetOnPresetImport(GameTestHelper helper) {
    NpcStateTestHelper.assertStateIsResetOnPresetImport(helper, humanoid());
    helper.succeed();
  }

  public static void testStateChangeNotifiesListener(GameTestHelper helper) {
    NpcStateTestHelper.assertStateChangeNotifiesListener(helper, humanoid());
    helper.succeed();
  }

  public static void testForgeKeeperPresetUsesStates(GameTestHelper helper) {
    NpcStateTestHelper.assertForgeKeeperPresetUsesStates(helper);
    helper.succeed();
  }
}
