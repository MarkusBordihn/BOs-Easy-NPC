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
import net.fabricmc.fabric.api.gametest.v1.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;

@SuppressWarnings("unused")
public class IdleNPCTest {

  private static EntityType<?> humanoid() {
    return ModEntityType.getEntityType(ModNPCEntityType.HUMANOID);
  }

  @GameTest(
      environment = "easy_npc:idle_npc_without_player",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testIdleNPCSkipsTravel(GameTestHelper helper) {
    IdleNPCTestHelper.assertIdleNPCSkipsTravel(helper, humanoid());
    helper.succeed();
  }

  @GameTest(
      environment = "easy_npc:idle_npc_without_player",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testPeriodicTickRunsTravel(GameTestHelper helper) {
    IdleNPCTestHelper.assertPeriodicTickRunsTravel(helper, humanoid());
    helper.succeed();
  }

  @GameTest(
      environment = "easy_npc:idle_npc_without_player",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testMovingNPCRunsTravel(GameTestHelper helper) {
    IdleNPCTestHelper.assertMovingNPCRunsTravel(helper, humanoid());
    helper.succeed();
  }

  @GameTest(
      environment = "easy_npc:idle_npc_without_player",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testAirborneOrHurtNPCRunsTravel(GameTestHelper helper) {
    IdleNPCTestHelper.assertAirborneOrHurtNPCRunsTravel(helper, humanoid());
    helper.succeed();
  }

  @GameTest(
      environment = "easy_npc:idle_npc_nearby_player",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testNearbyPlayerKeepsTravel(GameTestHelper helper) {
    IdleNPCTestHelper.assertNearbyPlayerKeepsTravel(helper, humanoid());
    helper.succeed();
  }

  @GameTest(
      environment = "easy_npc:idle_npc_spectator",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testSpectatorIsIgnored(GameTestHelper helper) {
    IdleNPCTestHelper.assertSpectatorIsIgnored(helper, humanoid());
    helper.succeed();
  }

  @GameTest(
      environment = "easy_npc:idle_npc_look_cooldown",
      structure = "easy_npc:gametest.3x3x3",
      maxTicks = 100)
  public void testLookGoalWaitsAfterFailedSearch(GameTestHelper helper) {
    IdleNPCTestHelper.assertLookGoalWaitsAfterFailedSearch(helper, humanoid());
    helper.succeed();
  }
}
