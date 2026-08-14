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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.neoforged.neoforge.gametest.GameTestHolder;
import net.neoforged.neoforge.gametest.PrefixGameTestTemplate;

@SuppressWarnings("unused")
@PrefixGameTestTemplate(value = false)
@GameTestHolder(Constants.MOD_ID)
public class WaitActionTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "gametest.3x3x3")
  public void testWaitDelaysTheFollowingActions(GameTestHelper helper) {
    WaitActionTestHelper.assertWaitDelaysTheFollowingActions(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testRemainingWaitSurvivesSaveAndLoad(GameTestHelper helper) {
    WaitActionTestHelper.assertRemainingWaitSurvivesSaveAndLoad(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSecondTriggerIsDiscarded(GameTestHelper helper) {
    WaitActionTestHelper.assertSecondTriggerIsDiscarded(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDifferentEventsRunInParallel(GameTestHelper helper) {
    WaitActionTestHelper.assertDifferentEventsRunInParallel(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testIntervalSetWithWaitRunsInOrder(GameTestHelper helper) {
    WaitActionTestHelper.assertIntervalSetWithWaitRunsInOrder(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testIntervalSetWithoutWaitPicksOneEntry(GameTestHelper helper) {
    WaitActionTestHelper.assertIntervalSetWithoutWaitPicksOneEntry(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testScreenActionIsKeptOverTheWait(GameTestHelper helper) {
    WaitActionTestHelper.assertScreenActionIsKeptOverTheWait(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testFallbackRunsOnceAfterTheWait(GameTestHelper helper) {
    WaitActionTestHelper.assertFallbackRunsOnceAfterTheWait(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPresetImportCancelsTheChain(GameTestHelper helper) {
    WaitActionTestHelper.assertPresetImportCancelsTheChain(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDeathCancelsTheChain(GameTestHelper helper) {
    WaitActionTestHelper.assertDeathCancelsTheChain(helper, humanoid());
    helper.succeed();
  }
}
