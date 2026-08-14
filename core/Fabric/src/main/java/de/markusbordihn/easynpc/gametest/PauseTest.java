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
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

@SuppressWarnings("unused")
public class PauseTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testPauseAndResumeOfASingleNPC(GameTestHelper helper) {
    PauseTestHelper.assertPauseAndResumeOfASingleNPC(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testPausedNPCSkipsItsBaseTick(GameTestHelper helper) {
    PauseTestHelper.assertPausedNPCSkipsItsBaseTick(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testPauseSurvivesSaveAndLoad(GameTestHelper helper) {
    PauseTestHelper.assertPauseSurvivesSaveAndLoad(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testGlobalPauseCoversEveryNPC(GameTestHelper helper) {
    PauseTestHelper.assertGlobalPauseCoversEveryNPC(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testGlobalResumeKeepsIndividualPause(GameTestHelper helper) {
    PauseTestHelper.assertGlobalResumeKeepsIndividualPause(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testGlobalPauseEndsWithTheServer(GameTestHelper helper) {
    PauseTestHelper.assertGlobalPauseEndsWithTheServer(helper, humanoid());
    helper.succeed();
  }
}
