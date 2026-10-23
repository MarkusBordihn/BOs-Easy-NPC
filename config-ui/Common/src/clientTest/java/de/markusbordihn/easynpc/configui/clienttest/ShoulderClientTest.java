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

package de.markusbordihn.easynpc.configui.clienttest;

import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.time.Duration;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ShoulderClientTest extends ClientTestBase {

  private static final String SHOULDER_NPC_TYPE = "easy_npc:cat";
  private static final String MISC_ATTRIBUTE_SCREEN = "MiscAttributeConfigurationScreen";
  private static final double PLAYER_X = 0.5;
  private static final double PLAYER_Z = 0.5;
  private static final double NPC_Z = 2.5;
  private static final double FALL_HEIGHT = 3.0;
  private static final long VISIBLE_CHANGE_PIXELS = 500;
  private static final long RENDER_TICKS = 20;
  private static final long LANDING_TICKS = 20;
  private static final Duration SIT_DOWN_TIMEOUT = Duration.ofSeconds(30);
  private static final CompareOptions PLAYER_AREA_COMPARE =
      CompareOptions.DEFAULT.withTolerance(8).withRegion(240, 40, 160, 220);

  // Vanilla Player.aiStep ignores fall triggers for 20 ticks after an entity landed on the
  // shoulder.
  private static final long SHOULDER_GUARD_TICKS = 30;

  private static Parameters npcQuery(UUID shoulderNpc) {
    return Parameters.of("uuid", shoulderNpc.toString());
  }

  private static String preparePlayerFrontView() {
    buildTestPlatform();
    client.teleport(PLAYER_X, STAND_Y, PLAYER_Z, 0, 0);
    client.renderSettings(Parameters.of().with("perspective", "front"));
    client.await(Until.ticksElapsed(RENDER_TICKS));
    return client.retainFrame();
  }

  private static UUID spawnOwnedShoulderNpc() {
    UUID shoulderNpc =
        client.spawnEntity(
            EntitySpawn.of(SHOULDER_NPC_TYPE, PLAYER_X, STAND_Y, NPC_Z).withNoAi(false));
    client.runCommand("easy_npc owner set " + shoulderNpc + " @s");
    return shoulderNpc;
  }

  private static void enableSitOnShoulder(UUID shoulderNpc) {
    openMainConfiguration(shoulderNpc);
    openSubPage("attributes", "misc", MISC_ATTRIBUTE_SCREEN);
    client.click(configurationButton("can_sit_on_shoulder"));
    client.closeScreen();
    client.await(Until.noScreen());
  }

  private static long changedPixelsSince(String baselineFrameId) {
    return client.compare(baselineFrameId, PLAYER_AREA_COMPARE).comparison().changedPixels();
  }

  @Test
  @DisplayName(
      "An owned NPC with \"Sit on Shoulder\" sits on the player and drops off after a fall")
  void ownedNpcSitsOnShoulderAndDropsOffAfterFall() {
    String playerOnlyFrame = preparePlayerFrontView();
    UUID shoulderNpc = spawnOwnedShoulderNpc();
    enableSitOnShoulder(shoulderNpc);
    client.await(SIT_DOWN_TIMEOUT, Until.entityAbsent(npcQuery(shoulderNpc)));
    client.await(Until.playerShoulder(SHOULDER_NPC_TYPE));
    client.await(Until.ticksElapsed(SHOULDER_GUARD_TICKS));
    long seatedPixels = changedPixelsSince(playerOnlyFrame);
    client.saveScreenshot("shoulder_seated");

    client.teleport(PLAYER_X, STAND_Y + FALL_HEIGHT, PLAYER_Z);
    client.await(Until.ticksElapsed(LANDING_TICKS));
    client.await(Until.entityPresent(npcQuery(shoulderNpc)));
    client.await(Until.playerShoulderEmpty());
    client.saveScreenshot("shoulder_dropped");

    assertTrue(seatedPixels > VISIBLE_CHANGE_PIXELS, "NPC not on shoulder: " + seatedPixels);
  }
}
