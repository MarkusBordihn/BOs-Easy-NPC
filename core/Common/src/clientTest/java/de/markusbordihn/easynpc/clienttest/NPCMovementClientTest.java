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

import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.Entity;
import java.time.Duration;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class NPCMovementClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:humanoid";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final int PLAYER_STAND_Z = 10;
  private static final long IDLE_TICKS = 60;
  private static final double ARRIVED_DISTANCE = 3.5;
  private static final double MINIMUM_BLOCKS_PER_SECOND = 4.0;
  private static final Duration WALK_TIMEOUT = Duration.ofSeconds(10);

  private UUID npcUUID;

  @BeforeEach
  void standOnWalkway() {
    awaitWorldSetupCapability();
    placeScene("walkway", -1, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, PLAYER_STAND_Z + 0.5, 180, 0);
  }

  @Test
  @DisplayName("Standing NPC starts walking to a followed player at normal walking speed")
  void standingNPCWalksToFollowedPlayer() {
    client.runCommand("summon " + NPC_TYPE + " 0.5 " + VIEW_Y + " 0.5");
    this.npcUUID =
        UUID.fromString(
            client.awaitEntity(entityTypeInSearchRadius(NPC_TYPE), entity -> true).uuid());
    client.await(Until.ticksElapsed(IDLE_TICKS));
    Entity standingNPC = client.awaitEntity(this.npcParameters(), entity -> true);

    client.runCommand("easy_npc objective " + this.npcUUID + " set follow player @s");
    long walkStartNanos = System.nanoTime();
    Entity arrivedNPC =
        client.awaitEntity(
            this.npcParameters(), entity -> entity.distance() <= ARRIVED_DISTANCE, WALK_TIMEOUT);
    double walkSeconds = (System.nanoTime() - walkStartNanos) / 1_000_000_000.0;
    client.saveScreenshot("npc_walked_to_player");

    double walkedBlocks = arrivedNPC.position().z() - standingNPC.position().z();
    double blocksPerSecond = walkedBlocks / walkSeconds;
    assertTrue(
        blocksPerSecond >= MINIMUM_BLOCKS_PER_SECOND,
        "NPC walked "
            + walkedBlocks
            + " blocks in "
            + walkSeconds
            + " s ("
            + blocksPerSecond
            + " blocks/s)");
  }

  @AfterEach
  void removeNPC() {
    if (this.npcUUID != null) {
      client.runCommand("easy_npc delete " + this.npcUUID);
    }
  }

  private Parameters npcParameters() {
    return entityInSearchRadius(NPC_TYPE, this.npcUUID);
  }
}
