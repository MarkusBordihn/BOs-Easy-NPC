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
import net.minecraftforge.gametest.GameTestHolder;

@SuppressWarnings("unused")
@GameTestHolder(Constants.MOD_ID)
public class ModelMessageTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelPoseChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPoseChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelPartPositionChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartPositionChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelRootRotationChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelRootRotationChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelPartRotationChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartRotationChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelRootScaleChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelRootScaleChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelPartScaleChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartScaleChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelPartVisibilityChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartVisibilityChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testModelAnimationBehaviorChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelAnimationBehaviorChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testEquipmentVisibilityChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertEquipmentVisibilityChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testPoseChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertPoseChange(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testNamedPoseChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertNamedPoseChange(helper, humanoid());
    helper.succeed();
  }
}
