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

import de.markusbordihn.easynpc.api.handler.EasyNPCPlayerHandler;
import de.markusbordihn.easynpc.condition.PlayerIdleCondition;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.handler.PlayerIdleTracker;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class PlayerIdleTestHelper {

  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 MOVED_PLAYER_POSITION = new Vec3(2, 2, 2);
  private static final int IDLE_TICKS = 60;
  private static final int MOVEMENT_TICKS = 40;
  private static final int TICKS_AFTER_MOVEMENT = 15;
  private static final int ONE_SECOND_IN_TICKS = 20;

  private PlayerIdleTestHelper() {}

  private static ConditionDataEntry idleFor(int seconds) {
    return new ConditionDataEntry(
        ConditionType.PLAYER_IDLE, ConditionOperationType.GREATER_THAN_OR_EQUALS, "", seconds);
  }

  private static long idleTicks(ServerPlayer serverPlayer) {
    return EasyNPCPlayerHandler.getIdleTicks(serverPlayer);
  }

  public static void assertIdleTimeGrowsWhileThePlayerStandsStill(GameTestHelper helper) {
    PlayerIdleTracker.reset();
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);

    helper.runAfterDelay(
        IDLE_TICKS,
        () -> {
          GameTestHelpers.assertTrue(
              helper,
              "A standing player must collect idle time",
              idleTicks(serverPlayer) >= ONE_SECOND_IN_TICKS);
          GameTestHelpers.assertTrue(
              helper,
              "A player idling for a second must satisfy a one second condition",
              PlayerIdleCondition.evaluate(idleFor(1), serverPlayer));
          GameTestHelpers.assertTrue(
              helper,
              "An idle condition without a player must never be satisfied",
              !PlayerIdleCondition.evaluate(idleFor(1), null));
          GameTestHelpers.assertTrue(
              helper,
              "A player idling for a second must not count as idling for a minute",
              !EasyNPCPlayerHandler.isIdle(serverPlayer, 60));
          helper.succeed();
        });
  }

  public static void assertMovementResetsTheIdleTime(GameTestHelper helper) {
    PlayerIdleTracker.reset();
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);

    helper
        .startSequence()
        .thenExecuteAfter(
            MOVEMENT_TICKS,
            () -> {
              GameTestHelpers.assertTrue(
                  helper,
                  "A standing player must collect idle time",
                  idleTicks(serverPlayer) >= ONE_SECOND_IN_TICKS);
              serverPlayer.setPos(helper.absoluteVec(MOVED_PLAYER_POSITION));
            })
        .thenExecuteAfter(
            TICKS_AFTER_MOVEMENT,
            () ->
                GameTestHelpers.assertTrue(
                    helper,
                    "A moving player must lose the collected idle time",
                    idleTicks(serverPlayer) < ONE_SECOND_IN_TICKS))
        .thenSucceed();
  }

  public static void assertIdleTimeOfAnUnknownPlayerIsZero(GameTestHelper helper) {
    PlayerIdleTracker.reset();

    GameTestHelpers.assertEquals(
        helper, "An unknown player must not have any idle time", 0L, idleTicks(null));
    GameTestHelpers.assertTrue(
        helper, "An unknown player must never be idle", !EasyNPCPlayerHandler.isIdle(null, 1));
  }
}
