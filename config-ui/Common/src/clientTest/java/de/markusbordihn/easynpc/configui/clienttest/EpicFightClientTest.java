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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.EnabledIfModLoaded;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

@EnabledIfModLoaded("epicfight")
class EpicFightClientTest extends ClientTestBase {

  private static final long RENDER_TICKS = 20;
  private static final long VISIBLE_CHANGE_PIXELS = 500;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(8);

  @ParameterizedTest(name = "{0}")
  @ValueSource(
      strings = {
        "cave_spider",
        "creeper",
        "drowned",
        "enderman",
        "evoker",
        "humanoid",
        "humanoid_slim",
        "husk",
        "iron_golem",
        "piglin",
        "piglin_brute",
        "pillager",
        "skeleton",
        "spider",
        "stray",
        "vex",
        "vindicator",
        "witch",
        "wither_skeleton",
        "zombie",
        "zombie_villager",
        "zombified_piglin"
      })
  @DisplayName("Epic Fight NPC renders in view without crashing the client")
  void epicFightModelRendersInView(String npcType) {
    buildTestPlatform();
    client.teleport(0.5, STAND_Y, 0.5, 0, 0);
    client.await(Until.ticksElapsed(RENDER_TICKS));
    String emptyPlatformFrame = client.retainFrame();

    UUID epicFightNpc =
        client.spawnEntity("easy_npc:epic_fight_" + npcType + "_raw", 0.5, STAND_Y, 3.5, true);
    client.await(Until.entityPresent(Parameters.of("uuid", epicFightNpc.toString())));
    client.await(Until.ticksElapsed(RENDER_TICKS));
    long changedPixels =
        client.compare(emptyPlatformFrame, MODEL_COMPARE).comparison().changedPixels();
    client.saveScreenshot("epic_fight_" + npcType);

    assertTrue(changedPixels > VISIBLE_CHANGE_PIXELS, npcType + " not visible: " + changedPixels);
  }
}
