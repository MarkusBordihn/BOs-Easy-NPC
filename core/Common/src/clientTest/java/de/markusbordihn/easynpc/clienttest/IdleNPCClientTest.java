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

package de.markusbordihn.easynpc.clienttest;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.time.Duration;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class IdleNPCClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:humanoid";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final int OUT_OF_LOOK_DISTANCE_Z = 24;
  private static final int RAISED_VIEW_Y = VIEW_Y + 3;
  private static final int RAISED_STAND_Z = -3;
  private static final int LOWERED_VIEW_Y = VIEW_Y - 3;
  private static final int LOWERED_STAND_Z = -5;
  private static final double LOOKING_UP_PITCH = -20.0;
  private static final double LOOKING_DOWN_PITCH = 20.0;
  private static final float PLAYER_FACING_NPC_PITCH = 35.0F;
  private static final long IDLE_TICKS = 60;

  // Vanilla LookControl turns the head sideways by at most 10 degrees per tick.
  private static final long HEAD_HALF_TURN_TICKS = 20;
  private static final Duration REACTION_TIMEOUT = Duration.ofSeconds(2);
  private static final Duration FALL_TIMEOUT = Duration.ofSeconds(1);
  private static final int SIDE_STAND_X = 3;
  private static final double FACING_EAST_YAW = -90.0;
  private static final double HEAD_YAW_TOLERANCE = 30.0;

  private UUID npcUUID;

  private static double yawDifference(double yaw, double expectedYaw) {
    return Math.abs(((yaw - expectedYaw) % 360 + 540) % 360 - 180);
  }

  @BeforeEach
  void standOnPlatform() {
    awaitWorldSetupCapability();
    placeScene("idle_npc_scene", -2, LOWERED_VIEW_Y - 1, LOWERED_STAND_Z);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
  }

  @Test
  @DisplayName("Idle NPC looks at a player who comes close and follows them within the look range")
  void idleNPCLooksAtApproachingPlayer() {
    client.teleport(0.5, VIEW_Y, OUT_OF_LOOK_DISTANCE_Z + 0.5, 0, 0);
    this.spawnNPC(0.5, VIEW_Y, 1.5);
    client.await(Until.ticksElapsed(IDLE_TICKS));

    client.teleport(0.5, RAISED_VIEW_Y, RAISED_STAND_Z + 0.5, 0, PLAYER_FACING_NPC_PITCH);
    client.awaitEntity(
        this.npcParameters(),
        entity -> entity.rotation().pitch() <= LOOKING_UP_PITCH,
        REACTION_TIMEOUT);
    client.await(Until.ticksElapsed(HEAD_HALF_TURN_TICKS));
    client.saveScreenshot("idle_npc_looks_at_player");

    client.teleport(0.5, LOWERED_VIEW_Y, LOWERED_STAND_Z + 0.5, 0, 0);
    client.awaitEntity(
        this.npcParameters(),
        entity -> entity.rotation().pitch() >= LOOKING_DOWN_PITCH,
        REACTION_TIMEOUT);
  }

  @Test
  @DisplayName("Idle NPC turns its head sideways to a player who comes close")
  void idleNPCTurnsHeadToPlayerAtItsSide() {
    client.teleport(0.5, VIEW_Y, OUT_OF_LOOK_DISTANCE_Z + 0.5, 0, 0);
    this.spawnNPC(0.5, VIEW_Y, 1.5);
    client.await(Until.ticksElapsed(IDLE_TICKS));

    client.teleport(SIDE_STAND_X + 0.5, VIEW_Y, 1.5, 90, 0);
    client.awaitEntity(
        this.npcParameters(),
        entity -> yawDifference(entity.headYaw(), FACING_EAST_YAW) <= HEAD_YAW_TOLERANCE,
        REACTION_TIMEOUT);
  }

  @Test
  @DisplayName("Idle NPC without a player nearby falls within one second when its ground is removed")
  void idleNPCFallsWithoutNearbyPlayer() {
    this.spawnNPC(0.5, VIEW_Y, OUT_OF_LOOK_DISTANCE_Z + 0.5);
    client.await(Until.ticksElapsed(IDLE_TICKS));

    client.setBlock(0, PLATFORM_Y, OUT_OF_LOOK_DISTANCE_Z, "minecraft:air");
    client.awaitEntity(
        this.npcParameters(), entity -> entity.position().y() < VIEW_Y - 1, FALL_TIMEOUT);
  }

  @AfterEach
  void removeNPC() {
    if (this.npcUUID != null) {
      client.runCommand("easy_npc delete " + this.npcUUID);
    }
  }

  private void spawnNPC(double x, double y, double z) {
    client.runCommand("summon " + NPC_TYPE + " " + x + " " + y + " " + z);
    this.npcUUID =
        UUID.fromString(
            client.awaitEntity(entityTypeInSearchRadius(NPC_TYPE), entity -> true).uuid());
  }

  private Parameters npcParameters() {
    return entityInSearchRadius(NPC_TYPE, this.npcUUID);
  }
}
