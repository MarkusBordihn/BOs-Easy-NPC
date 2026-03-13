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
public class NPCDataIsolationTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPoseIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertPoseIsolation(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPoseNameIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertPoseNameIsolation(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testRotationIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertRotationIsolation(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testAnimationIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertAnimationIsolation(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testScaleIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertScaleIsolation(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPositionIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertPositionIsolation(helper, humanoid());
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testVisibilityIsolation(GameTestHelper helper) {
    NPCDataIsolationTestHelper.assertVisibilityIsolation(helper, humanoid());
    helper.succeed();
  }
}
