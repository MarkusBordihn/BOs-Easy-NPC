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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.EnabledIfModLoaded;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

@EnabledIfModLoaded("easy_model_entities")
class EasyModelEntitiesClientTest extends ClientTestBase {

  private static final String EASY_MODEL_NPC_TYPE = "easy_npc:easy_model_entities_npc";
  private static final String EXAMPLE_PROFILE_PREFIX = "easy_model_entities_examples:entity/";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final long RENDER_TICKS = 20;
  private static final long VISIBLE_CHANGE_PIXELS = 500;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(8);

  private static long changedPixelsSince(String frameId) {
    return client.compare(frameId, MODEL_COMPARE).comparison().changedPixels();
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"wisp_lantern", "coral_drifter", "dawn_sparrow", "skybound_wanderer"})
  @DisplayName("Easy Model NPC renders its default profile and switches to the example profile")
  void exampleProfileRendersInView(String profile) {
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
    client.await(Until.ticksElapsed(RENDER_TICKS));
    String emptyPlatformFrame = client.retainFrame();

    UUID easyModelNpc = client.spawnEntity(EASY_MODEL_NPC_TYPE, 0.5, VIEW_Y, 4.5, true);
    client.await(Until.entityPresent(Parameters.of("uuid", easyModelNpc.toString())));
    client.await(Until.ticksElapsed(RENDER_TICKS));
    long defaultProfilePixels = changedPixelsSince(emptyPlatformFrame);
    String defaultProfileFrame = client.retainFrame();

    client.runCommand(
        "easy_npc render set species "
            + easyModelNpc
            + " \""
            + EXAMPLE_PROFILE_PREFIX
            + profile
            + "\"");
    client.await(Until.ticksElapsed(RENDER_TICKS));
    long switchedProfilePixels = changedPixelsSince(defaultProfileFrame);
    client.saveScreenshot("easy_model_" + profile);

    assertTrue(
        defaultProfilePixels > VISIBLE_CHANGE_PIXELS,
        "Default profile not visible: " + defaultProfilePixels);
    assertTrue(
        switchedProfilePixels > VISIBLE_CHANGE_PIXELS,
        profile + " did not change the rendered model: " + switchedProfilePixels);
  }
}
