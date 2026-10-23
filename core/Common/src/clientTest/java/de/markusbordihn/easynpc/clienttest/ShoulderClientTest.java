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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ShoulderClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:allay";
  private static final String PERSPECTIVE_BINDING = "key.togglePerspective";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final int REMOUNT_COOLDOWN_TICKS = 200;
  private static final int OBJECTIVE_COOLDOWN_TICKS = 20;
  private static final long RENDER_TICKS = 20;
  private static final Duration REMOUNT_BLOCKED_DURATION = Duration.ofSeconds(4);
  private static final Duration SIT_ON_SHOULDER_TIMEOUT = Duration.ofSeconds(30);
  private static final double SHOULDER_OFFSET_UP = 0.25;
  private static final double SHOULDER_OFFSET_DOWN = -0.25;

  private final List<UUID> spawnedNPCs = new ArrayList<>();

  @BeforeEach
  void standOnPlatform() {
    awaitWorldSetupCapability();
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
  }

  @Test
  @DisplayName("Shoulder commands seat an NPC, render it and put it down again")
  void shoulderCommandsSeatRenderAndDismountNPC() {
    UUID npcUUID = this.spawnNPC();

    client.runCommand("easy_npc shoulder sit " + npcUUID + " @s");
    client.await(Until.entityAbsent(npcParameters(npcUUID)));
    client.await(Until.playerShoulder(NPC_TYPE));
    captureFromFront("npc_on_shoulder");

    client.runCommand("easy_npc shoulder dismount @s left");
    client.await(Until.entityPresent(npcParameters(npcUUID)));
    client.await(Until.playerShoulderEmpty());
  }

  @Test
  @DisplayName("Sit on Shoulder objective waits for the remount cooldown after a dismount")
  void sitOnShoulderObjectiveWaitsForRemountCooldown() {
    // The vanilla Allay brain lets the NPC wander out of sight, which would read as a remount.
    placeScene("shoulder_cage", -4, VIEW_Y, -2);
    UUID npcUUID = this.spawnNPC();
    client.runCommand("easy_npc owner set " + npcUUID + " @s");
    client.runCommand("easy_npc shoulder enable " + npcUUID);
    client.runCommand(
        "easy_npc objective " + npcUUID + " set owner_shoulder 16 " + OBJECTIVE_COOLDOWN_TICKS);
    client.await(SIT_ON_SHOULDER_TIMEOUT, Until.entityAbsent(npcParameters(npcUUID)));
    client.await(Until.playerShoulder(NPC_TYPE));

    client.runCommand("easy_npc shoulder dismount @s left " + REMOUNT_COOLDOWN_TICKS);
    client.await(Until.entityPresent(npcParameters(npcUUID)));
    client.await(Until.playerShoulderEmpty());

    client.assertNever(REMOUNT_BLOCKED_DURATION, Until.playerShoulder(NPC_TYPE));
    client.await(SIT_ON_SHOULDER_TIMEOUT, Until.entityAbsent(npcParameters(npcUUID)));
    client.await(Until.playerShoulder(NPC_TYPE));
  }

  @Test
  @DisplayName("A shoulder offset moves the seated NPC up or down on the shoulder")
  void shoulderOffsetMovesSeatedNPC() {
    client.renderSettings(Parameters.of().with("perspective", "front"));
    client.await(Until.ticksElapsed(RENDER_TICKS));
    long defaultTop =
        seatedShoulderNPCTop(ShoulderClientTest::spawnUntrackedNPC, 0.0, "offset_default");
    long raisedTop =
        seatedShoulderNPCTop(
            ShoulderClientTest::spawnUntrackedNPC, SHOULDER_OFFSET_UP, "offset_up");
    long loweredTop =
        seatedShoulderNPCTop(
            ShoulderClientTest::spawnUntrackedNPC, SHOULDER_OFFSET_DOWN, "offset_down");

    String tops = "default " + defaultTop + ", up " + raisedTop + ", down " + loweredTop;
    assertTrue(raisedTop < defaultTop, "offset up did not raise the NPC: " + tops);
    assertTrue(loweredTop > defaultTop, "offset down did not lower the NPC: " + tops);
  }

  @AfterEach
  void removeSpawnedNPCs() {
    client.runCommand("easy_npc shoulder dismount @s");
    for (UUID npcUUID : this.spawnedNPCs) {
      client.runCommand("easy_npc delete " + npcUUID);
    }
    this.spawnedNPCs.clear();
  }

  private UUID spawnNPC() {
    UUID npcUUID = client.spawnEntity(EntitySpawn.of(NPC_TYPE, 0.5, VIEW_Y, 2.5));
    this.spawnedNPCs.add(npcUUID);
    client.await(Until.entityPresent(npcParameters(npcUUID)));
    return npcUUID;
  }

  private static UUID spawnUntrackedNPC() {
    UUID npcUUID = client.spawnEntity(EntitySpawn.of(NPC_TYPE, 0.5, VIEW_Y, 2.5));
    client.await(Until.entityPresent(npcParameters(npcUUID)));
    return npcUUID;
  }

  private static Parameters npcParameters(UUID npcUUID) {
    return Parameters.of("type", NPC_TYPE).with("uuid", npcUUID.toString());
  }

  private static void captureFromFront(String screenshotName) {
    client.pressBinding(PERSPECTIVE_BINDING);
    client.pressBinding(PERSPECTIVE_BINDING);
    try {
      client.await(Until.ticksElapsed(RENDER_TICKS));
      client.saveScreenshot(screenshotName);
    } finally {
      client.pressBinding(PERSPECTIVE_BINDING);
    }
  }
}
