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

import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.allay;
import static de.markusbordihn.easynpc.gametest.GameTestEntityTypes.humanoid;

import net.minecraft.gametest.framework.GameTestHelper;

public final class ShoulderTest {

  private ShoulderTest() {}

  public static void testSitOnShoulderStoresNPC(GameTestHelper helper) {
    ShoulderTestHelper.assertSitOnShoulderStoresNPC(helper, allay());
    helper.succeed();
  }

  public static void testSecondNPCUsesRightShoulder(GameTestHelper helper) {
    ShoulderTestHelper.assertSecondNPCUsesRightShoulder(helper, allay());
    helper.succeed();
  }

  public static void testDismountRespawnsNPC(GameTestHelper helper) {
    ShoulderTestHelper.assertDismountRespawnsNPC(helper, allay());
    helper.succeed();
  }

  public static void testShoulderSizeLimit(GameTestHelper helper) {
    ShoulderTestHelper.assertShoulderSizeLimit(helper, allay(), humanoid());
    helper.succeed();
  }

  public static void testAttributeTogglesObjective(GameTestHelper helper) {
    ShoulderTestHelper.assertAttributeTogglesObjective(helper, allay());
    helper.succeed();
  }

  public static void testDismountKeepsNPCWhenUUIDTaken(GameTestHelper helper) {
    ShoulderTestHelper.assertDismountKeepsNPCWhenUUIDTaken(helper, allay());
    helper.succeed();
  }

  public static void testDismountKeepsNPCWhenBlocked(GameTestHelper helper) {
    ShoulderTestHelper.assertDismountKeepsNPCWhenBlocked(helper, allay());
    helper.succeed();
  }

  public static void testDismountFallsBackToPlayerPosition(GameTestHelper helper) {
    ShoulderTestHelper.assertDismountFallsBackToPlayerPosition(helper, allay());
    helper.succeed();
  }

  public static void testVanillaShoulderReleaseRespawnsNPC(GameTestHelper helper) {
    ShoulderTestHelper.assertVanillaShoulderReleaseRespawnsNPC(helper, allay());
  }

  public static void testVanillaShoulderReleaseKeepsBlockedNPC(GameTestHelper helper) {
    ShoulderTestHelper.assertVanillaShoulderReleaseKeepsBlockedNPC(helper, allay());
  }

  public static void testPlayerDeathPutsNPCDown(GameTestHelper helper) {
    ShoulderTestHelper.assertPlayerDeathPutsNPCDown(helper, allay());
  }

  public static void testBlockedNPCStaysOnShoulderAfterRespawn(GameTestHelper helper) {
    ShoulderTestHelper.assertBlockedNPCStaysOnShoulderAfterRespawn(helper, allay());
  }

  public static void testOwnerDismountBlocksRemount(GameTestHelper helper) {
    ShoulderTestHelper.assertOwnerDismountBlocksRemount(helper, allay());
  }

  public static void testRemountCooldownSurvivesReload(GameTestHelper helper) {
    ShoulderTestHelper.assertRemountCooldownSurvivesReload(helper, allay());
    helper.succeed();
  }

  public static void testShoulderCooldownFromObjective(GameTestHelper helper) {
    ShoulderTestHelper.assertShoulderCooldownFromObjective(helper, allay());
    helper.succeed();
  }

  public static void testShoulderGoalRespectsMaxDistance(GameTestHelper helper) {
    ShoulderTestHelper.assertShoulderGoalRespectsMaxDistance(helper, allay());
    helper.succeed();
  }

  public static void testShoulderGoalStopsWithoutProgress(GameTestHelper helper) {
    ShoulderTestHelper.assertShoulderGoalStopsWithoutProgress(helper, allay());
  }

  public static void testShoulderGoalWaitsOutOfCombat(GameTestHelper helper) {
    ShoulderTestHelper.assertShoulderGoalWaitsOutOfCombat(helper, allay());
    helper.succeed();
  }

  public static void testReviveRejectsNPCOnShoulder(GameTestHelper helper) {
    ShoulderTestHelper.assertReviveRejectsNPCOnShoulder(helper, allay());
    helper.succeed();
  }

  public static void testSitOnShoulderRejectsUnavailableNPC(GameTestHelper helper) {
    ShoulderTestHelper.assertSitOnShoulderRejectsUnavailableNPC(helper, allay());
    helper.succeed();
  }

  public static void testShoulderDataSentToClientsWithoutServerData(GameTestHelper helper) {
    ShoulderTestHelper.assertShoulderDataSentToClientsWithoutServerData(helper, allay());
    helper.succeed();
  }
}
