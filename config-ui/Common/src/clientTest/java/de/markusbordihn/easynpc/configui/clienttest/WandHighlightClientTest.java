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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClient.TemporaryChange;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Weather;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureCrop;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureImage;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class WandHighlightClientTest extends ClientTestBase {

  private static final String WAND_ITEM = "easy_npc_config_ui:easy_npc_wand";
  private static final long NOON_DAY_TIME = 6000;
  private static final long NPC_RENDER_TICKS = 20;
  private static final long PLATFORM_LIGHTING_TICKS = 40;
  private static final long SETTLED_TICKS = 5;
  private static final long HIGHLIGHT_ARRIVAL_TICKS = 40;
  private static final double MINIMUM_HIGHLIGHT_CHANGED_RATIO = 0.05;
  private static final double NPC_SEARCH_HEIGHT_SHARE = 0.9;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(32);

  @Test
  @DisplayName("Holding the wand outlines nearby NPCs")
  void holdingWandOutlinesNearbyNPCs() {
    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      client.closeAllScreens();
      client.await(Until.noScreen());
      client.clearInventory();
      buildTestPlatform();
      client.teleport(0.5, STAND_Y, 0.5, 0, 0);
      client.await(Until.ticksElapsed(PLATFORM_LIGHTING_TICKS));
      client.await(Until.settled(SETTLED_TICKS));
      String emptyPlatformFrame = client.retainFrame();

      CaptureImage frameSize = client.screenshot().image();
      UUID testNpc = client.spawnEntity(TEST_NPC_TYPE, 0.5, STAND_Y, 3.5, true);
      client.await(Until.ticksElapsed(NPC_RENDER_TICKS));
      CaptureCrop npcBounds =
          client
              .compare(
                  emptyPlatformFrame,
                  MODEL_COMPARE.withRegion(
                      (int) frameSize.width() / 4,
                      0,
                      (int) frameSize.width() / 2,
                      (int) (frameSize.height() * NPC_SEARCH_HEIGHT_SHARE)))
              .comparison()
              .changedBounds()
              .orElseThrow();
      String plainNPCFrame = client.retainFrame();

      client.equip(WAND_ITEM);
      client.await(Until.ticksElapsed(HIGHLIGHT_ARRIVAL_TICKS));
      double highlightChange =
          client
              .compare(
                  plainNPCFrame,
                  MODEL_COMPARE.withRegion(
                      (int) npcBounds.x(),
                      (int) npcBounds.y(),
                      (int) npcBounds.width(),
                      (int) npcBounds.height()))
              .comparison()
              .changedRatio();
      client.saveScreenshot("wand_highlight");
      client.clearInventory();
      client.removeEntity(testNpc);

      assertTrue(
          highlightChange >= MINIMUM_HIGHLIGHT_CHANGED_RATIO,
          "Wand highlight changes only " + highlightChange + " of the NPC area");
    }
  }
}
