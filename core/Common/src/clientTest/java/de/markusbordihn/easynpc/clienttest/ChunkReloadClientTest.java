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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClient.TemporaryChange;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.GameMode;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ChunkReloadClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:humanoid";
  private static final String CHANGED_NAME = "Reloaded";
  private static final int RENDER_DISTANCE_CHUNKS = 2;
  private static final int SIMULATION_DISTANCE_CHUNKS = 5;
  private static final int CHUNK_SIZE = 16;
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final int OUTSIDE_SPAWN_CHUNKS_X = 1000;
  private static final double NPC_Z = 3.5;
  private static final double NEARBY_X = OUTSIDE_SPAWN_CHUNKS_X + 2 * CHUNK_SIZE - 1.5;
  private static final double UNLOADING_X = OUTSIDE_SPAWN_CHUNKS_X + 10 * CHUNK_SIZE + 0.5;
  private static final long WAITING_TICKS = 200;

  private UUID npcUUID;

  @BeforeEach
  void lowerViewAndSimulationDistance() {
    awaitWorldSetupCapability();
    client.renderSettings(
        Parameters.of()
            .with("renderDistance", RENDER_DISTANCE_CHUNKS)
            .with("simulationDistance", SIMULATION_DISTANCE_CHUNKS));
  }

  @Test
  @DisplayName("Changed NPC keeps its change after its chunk was unloaded and loaded again")
  void changedNPCKeepsChangeAfterChunkReload() {
    // Vanilla keeps spectator flight when switching back to creative, survival ends it first.
    try (TemporaryChange landing = client.setGameMode(GameMode.SURVIVAL);
        TemporaryChange spectator = client.setGameMode(GameMode.SPECTATOR)) {
      this.spawnChangedNPC();

      client.teleport(UNLOADING_X, VIEW_Y, NPC_Z, 0, 0);
      client.await(Until.entityAbsent(this.npcParameters()));
      client.await(Until.ticksElapsed(WAITING_TICKS));

      client.teleport(OUTSIDE_SPAWN_CHUNKS_X + 0.5, VIEW_Y, 0.5, 0, 0);
      client.await(Until.entityPresent(this.npcParameters()));
      this.awaitChangedName();
      client.saveScreenshot("reloaded_npc");
    }
  }

  @Test
  @DisplayName("Changed NPC two chunks away stays loaded and keeps its change")
  void changedNPCStaysLoadedTwoChunksAway() {
    try (TemporaryChange landing = client.setGameMode(GameMode.SURVIVAL);
        TemporaryChange spectator = client.setGameMode(GameMode.SPECTATOR)) {
      this.spawnChangedNPC();

      client.teleport(NEARBY_X, VIEW_Y, NPC_Z, 90, 0);
      client.await(Until.ticksElapsed(WAITING_TICKS));
      this.awaitChangedName();
      client.saveScreenshot("nearby_npc");
    }
  }

  @AfterEach
  void removeNPC() {
    if (this.npcUUID != null) {
      client.runCommand("easy_npc delete " + this.npcUUID);
    }
  }

  private void spawnChangedNPC() {
    client.teleport(OUTSIDE_SPAWN_CHUNKS_X + 0.5, VIEW_Y, 0.5, 0, 0);
    placeScene("npc_platform", OUTSIDE_SPAWN_CHUNKS_X - 2, PLATFORM_Y, -1);
    this.npcUUID =
        client.spawnEntity(EntitySpawn.of(NPC_TYPE, OUTSIDE_SPAWN_CHUNKS_X + 0.5, VIEW_Y, NPC_Z));
    client.await(Until.entityPresent(this.npcParameters()));
    client.runCommand("easy_npc name set " + this.npcUUID + " " + CHANGED_NAME);
    this.awaitChangedName();
  }

  private void awaitChangedName() {
    client.awaitEntity(
        this.npcParameters(),
        entity -> entity.customName().filter(CHANGED_NAME::equals).isPresent());
  }

  private Parameters npcParameters() {
    return entityInSearchRadius(NPC_TYPE, this.npcUUID);
  }
}
