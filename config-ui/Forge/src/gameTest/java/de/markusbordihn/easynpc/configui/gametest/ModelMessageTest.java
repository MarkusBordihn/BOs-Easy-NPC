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

import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

public final class ModelMessageTest {

  private ModelMessageTest() {}

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  public static void testModelPoseChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPoseChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelPartPositionChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartPositionChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelRootRotationChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelRootRotationChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelPartRotationChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartRotationChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelRootScaleChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelRootScaleChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelPartScaleChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartScaleChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelPartVisibilityChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelPartVisibilityChange(helper, humanoid());
    helper.succeed();
  }

  public static void testModelAnimationBehaviorChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertModelAnimationBehaviorChange(helper, humanoid());
    helper.succeed();
  }

  public static void testEquipmentVisibilityChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertEquipmentVisibilityChange(helper, humanoid());
    helper.succeed();
  }

  public static void testPoseChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertPoseChange(helper, humanoid());
    helper.succeed();
  }

  public static void testNamedPoseChange(GameTestHelper helper) {
    ModelMessageTestHelper.assertNamedPoseChange(helper, humanoid());
    helper.succeed();
  }
}
