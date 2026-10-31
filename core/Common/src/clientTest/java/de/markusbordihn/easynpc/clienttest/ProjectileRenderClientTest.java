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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.time.Duration;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ProjectileRenderClientTest extends ClientTestBase {

  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final double PROJECTILE_Y = VIEW_Y + 1.5;
  private static final long SETTLED_TICKS = 5;
  private static final double VISIBLE_CHANGE_RATIO = 0.0005;
  private static final CompareOptions PROJECTILE_COMPARE =
      CompareOptions.DEFAULT
          .withTolerance(8)
          .withAwaitChangedRatioAbove(VISIBLE_CHANGE_RATIO, Duration.ofSeconds(10));
  private static final String PROJECTILE_ITEM = "minecraft:diamond_sword";
  private static final String PROJECTILE_NBT =
      "{ProjectileData:{ProjectileType:\"%s\",ProjectileItem:\"%s\"},Motion:[-0.1d,0.25d,0.0d]}";

  @ParameterizedTest(name = "{0}")
  @CsvSource({
    "bullet, npc_bullet",
    "arrow, npc_bullet",
    "fire_arrow, npc_bullet",
    "spectral_arrow, npc_bullet",
    "trident, npc_bullet",
    "snowball, npc_item_projectile",
    "egg, npc_item_projectile",
    "small_fireball, npc_item_projectile",
    "large_fireball, npc_item_projectile",
    "wither_skull, npc_item_projectile",
    "item, npc_item_projectile"
  })
  @DisplayName("Projectile type renders in flight without crashing the client")
  void projectileRendersInFlight(String projectileType, String entityType) {
    String entityId = "easy_npc:" + entityType;
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
    client.await(Until.settled(SETTLED_TICKS));
    String emptyPlatformFrame = client.retainFrame();
    client.spawnEntity(
        EntitySpawn.of(entityId, 1.0, PROJECTILE_Y, 4.5)
            .withNbt(String.format(PROJECTILE_NBT, projectileType, PROJECTILE_ITEM)));

    client.await(Until.entityPresent(Parameters.of("type", entityId)));
    client.compare(emptyPlatformFrame, PROJECTILE_COMPARE);
    client.saveScreenshot(projectileType);
  }
}
