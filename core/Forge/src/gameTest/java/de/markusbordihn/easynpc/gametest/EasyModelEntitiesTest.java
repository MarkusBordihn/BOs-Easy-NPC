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

import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.entity.EasyModelEntitiesEntityType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

@SuppressWarnings("unused")
@EnabledIfModLoaded(CompatConstants.MOD_EASY_MODEL_ENTITIES_ID)
public class EasyModelEntitiesTest {

  private static EntityType<?> easyModelNPC() {
    return ModEntityType.getEntityType(EasyModelEntitiesEntityType.EASY_MODEL_NPC);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testIntegrationListsExampleProfiles(GameTestHelper helper) {
    EasyModelEntitiesTestHelper.assertIntegrationListsExampleProfiles(helper);
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDefaultProfileDefinesTheNPC(GameTestHelper helper) {
    EasyModelEntitiesTestHelper.assertDefaultProfileDefinesTheNPC(helper, easyModelNPC());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testFloatingProfileFlies(GameTestHelper helper) {
    EasyModelEntitiesTestHelper.assertFloatingProfileFlies(helper, easyModelNPC());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAquaticProfileSwims(GameTestHelper helper) {
    EasyModelEntitiesTestHelper.assertAquaticProfileSwims(helper, easyModelNPC());
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testProfileSurvivesPresetImport(GameTestHelper helper) {
    EasyModelEntitiesTestHelper.assertProfileSurvivesPresetImport(helper, easyModelNPC());
    helper.succeed();
  }
}
