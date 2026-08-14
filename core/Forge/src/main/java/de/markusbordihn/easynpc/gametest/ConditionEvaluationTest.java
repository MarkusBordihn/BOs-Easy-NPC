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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraftforge.gametest.GameTestHolder;

@SuppressWarnings("unused")
@GameTestHolder(Constants.MOD_ID)
public class ConditionEvaluationTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testItemQuantityConditions(GameTestHelper helper) {
    ConditionEvaluationTestHelper.assertItemQuantityConditions(helper);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testHealthTargetConditions(GameTestHelper helper) {
    ConditionEvaluationTestHelper.assertHealthTargetConditions(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testButtonExecutionLimitEnforced(GameTestHelper helper) {
    ConditionEvaluationTestHelper.assertButtonExecutionLimitEnforced(helper);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testShortExecutionLimitIntervalsEnforced(GameTestHelper helper) {
    ConditionEvaluationTestHelper.assertShortExecutionLimitIntervalsEnforced(helper);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testConditionalDialogOpenRespectsConditions(GameTestHelper helper) {
    ConditionEvaluationTestHelper.assertConditionalDialogOpenRespectsConditions(helper);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDefaultDialogExecutionLimitEnforced(GameTestHelper helper) {
    ConditionEvaluationTestHelper.assertDefaultDialogExecutionLimitEnforced(helper, humanoid());
    helper.succeed();
  }
}
