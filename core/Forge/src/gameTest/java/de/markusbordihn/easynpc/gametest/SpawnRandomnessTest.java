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
public class SpawnRandomnessTest {

  private static EntityType<?> entityType(ModNPCEntityType npcEntityType) {
    return ModEntityType.getEntityType(npcEntityType);
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPiglinNPCSpawnsWithoutRandomBabyOrEquipment(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertPiglinNPCSpawnsWithoutRandomBabyOrEquipment(
        helper, entityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPiglinNPCKeepsConfiguredAgeAndEquipmentOnSpawn(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertPiglinNPCKeepsConfiguredAgeAndEquipmentOnSpawn(
        helper, entityType(ModNPCEntityType.PIGLIN));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testSpiderNPCSpawnsWithoutJockeyOrEffect(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertSpiderNPCSpawnsWithoutJockeyOrEffect(
        helper, entityType(ModNPCEntityType.SPIDER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testPillagerNPCNeverBecomesPatrolLeader(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertIllagerNPCNeverBecomesPatrolLeader(
        helper, entityType(ModNPCEntityType.PILLAGER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testVindicatorNPCNeverBecomesPatrolLeader(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertIllagerNPCNeverBecomesPatrolLeader(
        helper, entityType(ModNPCEntityType.VINDICATOR));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testEvokerNPCNeverBecomesPatrolLeader(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertIllagerNPCNeverBecomesPatrolLeader(
        helper, entityType(ModNPCEntityType.EVOKER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testIllusionerNPCNeverBecomesPatrolLeader(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertIllagerNPCNeverBecomesPatrolLeader(
        helper, entityType(ModNPCEntityType.ILLUSIONER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testCreeperNPCStaysUnchargedByLightning(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertCreeperNPCStaysUnchargedByLightning(
        helper, entityType(ModNPCEntityType.CREEPER));
    helper.succeed();
  }

  @GameTest(template = "gametest.3x3x3")
  public void testGhastNPCSurvivesReflectedFireball(GameTestHelper helper) {
    SpawnRandomnessTestHelper.assertGhastNPCSurvivesReflectedFireball(
        helper, entityType(ModNPCEntityType.GHAST));
    helper.succeed();
  }
}
