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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.Constants;
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
public class DialogActionMessageTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogSetSave(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertDialogSetSave(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogSave(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertDialogSave(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testHarmlessDialogButtonSave(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertHarmlessDialogButtonSave(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testCommandDialogButtonSaveRequiresCreative(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertCommandDialogButtonSaveRequiresCreative(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogRemove(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertDialogRemove(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogButtonRemove(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertDialogButtonRemove(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testHarmlessActionEventChange(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertHarmlessActionEventChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testCommandActionEventChangeRequiresCreative(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertCommandActionEventChangeRequiresCreative(
        helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testCommandTradingOfferActionRequiresCreative(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertCommandTradingOfferActionRequiresCreative(
        helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testOwnExecutionLimitReset(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertOwnExecutionLimitReset(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testAllPlayersExecutionLimitResetRequiresPermission(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertAllPlayersExecutionLimitResetRequiresPermission(
        helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testExecutionLimitResetRequiresAccess(GameTestHelper helper) {
    DialogActionMessageTestHelper.assertExecutionLimitResetRequiresAccess(helper, humanoid());
    helper.succeed();
  }
}
