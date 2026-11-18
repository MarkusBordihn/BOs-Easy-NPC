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
public class VanillaMobQuirkTest {

  private static EntityType<?> entityType(ModNPCEntityType npcEntityType) {
    return ModEntityType.getEntityType(npcEntityType);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testVillagerNPCSurvivesLightning(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertNPCSurvivesLightning(
        helper, entityType(ModNPCEntityType.VILLAGER));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testPigNPCSurvivesLightning(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertNPCSurvivesLightning(helper, entityType(ModNPCEntityType.PIG));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testSlimeNPCDoesNotSplitOnDeath(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertSlimeNPCDoesNotSplitOnDeath(
        helper, entityType(ModNPCEntityType.SLIME));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testSlimeNPCDealsNoTouchDamage(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertSlimeNPCDealsNoTouchDamage(
        helper, entityType(ModNPCEntityType.SLIME));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testChickenNPCLaysNoEggs(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertChickenNPCLaysNoEggs(
        helper, entityType(ModNPCEntityType.CHICKEN));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testFoxNPCKeepsFoodInItsMouth(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertFoxNPCKeepsFoodInItsMouth(
        helper, entityType(ModNPCEntityType.FOX));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testFoxNPCGetsNoRandomItemOnSpawn(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertFoxNPCGetsNoRandomItemOnSpawn(
        helper, entityType(ModNPCEntityType.FOX));
    helper.succeed();
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testCatNPCKeepsItsVanillaPose(GameTestHelper helper) {
    VanillaMobQuirkTestHelper.assertCatNPCKeepsItsVanillaPose(
        helper, entityType(ModNPCEntityType.CAT));
    helper.succeed();
  }
}
