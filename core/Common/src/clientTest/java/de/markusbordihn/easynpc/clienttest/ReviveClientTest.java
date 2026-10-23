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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.time.Duration;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ReviveClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:humanoid";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final String REVIVE_POSITION = "0.5 " + VIEW_Y + " 4.5";
  private static final Duration SPAWN_REFUSED_DURATION = Duration.ofSeconds(2);

  private UUID npcUUID;

  @BeforeEach
  void standOnPlatform() {
    awaitWorldSetupCapability();
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
  }

  @Test
  @DisplayName("Killed NPC is refused by spawn and comes back with revive")
  void killedNPCComesBackWithRevive() {
    this.npcUUID = client.spawnEntity(EntitySpawn.of(NPC_TYPE, 0.5, VIEW_Y, 2.5));
    client.await(Until.entityPresent(this.npcParameters()));

    client.runCommand("kill " + this.npcUUID);
    client.await(Until.entityAbsent(this.npcParameters()));

    client.runCommand("easy_npc spawn " + this.npcUUID);
    client.assertNever(SPAWN_REFUSED_DURATION, Until.entityPresent(this.npcParameters()));

    client.runCommand("easy_npc revive " + this.npcUUID + " " + REVIVE_POSITION);
    client.await(Until.entityPresent(this.npcParameters()));
    client.saveScreenshot("revived_npc");
  }

  @AfterEach
  void removeNPC() {
    if (this.npcUUID != null) {
      client.runCommand("easy_npc delete " + this.npcUUID);
    }
  }

  private Parameters npcParameters() {
    return Parameters.of("type", NPC_TYPE).with("uuid", this.npcUUID.toString());
  }
}
