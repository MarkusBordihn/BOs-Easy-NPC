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
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.clienttest;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ModelRenderClientTest extends ClientTestBase {

  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final long RENDER_TICKS = 20;

  @ParameterizedTest(name = "{0}")
  @ValueSource(
      strings = {
        "allay",
        "cat",
        "cave_spider",
        "chicken",
        "creeper",
        "doppler",
        "drowned",
        "enderman",
        "evoker",
        "fairy",
        "fox",
        "ghast",
        "horse",
        "humanoid",
        "humanoid_slim",
        "husk",
        "illusioner",
        "iron_golem",
        "orc",
        "orc_warrior",
        "pig",
        "piglin",
        "piglin_brute",
        "pillager",
        "skeleton",
        "skeleton_horse",
        "slime",
        "spider",
        "stray",
        "vex",
        "villager",
        "vindicator",
        "wandering_trader",
        "witch",
        "wither_skeleton",
        "wolf",
        "zombie",
        "zombie_horse",
        "zombie_villager",
        "zombified_piglin"
      })
  @DisplayName("NPC model renders in view without crashing the client")
  void modelRendersInView(String entityType) {
    String entityId = "easy_npc:" + entityType;
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
    client.spawnEntity(entityId, 0.5, VIEW_Y, 4.5, true);

    client.await(Until.entityPresent(Parameters.of("type", entityId)));
    client.await(Until.ticksElapsed(RENDER_TICKS));
    client.saveScreenshot(entityType);
  }
}
