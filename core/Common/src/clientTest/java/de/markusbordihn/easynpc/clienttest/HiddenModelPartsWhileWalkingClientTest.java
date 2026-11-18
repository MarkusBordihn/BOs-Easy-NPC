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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClient.TemporaryChange;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Weather;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureCrop;
import java.time.Duration;
import java.util.Locale;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class HiddenModelPartsWhileWalkingClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:humanoid";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final int PLAYER_STAND_Z = 10;
  private static final long NOON_DAY_TIME = 6000;
  private static final long WALKWAY_LIGHTING_TICKS = 40;
  private static final double MID_WALK_DISTANCE = 6.0;
  private static final Duration WALK_TIMEOUT = Duration.ofSeconds(10);
  private static final String HIDDEN_NAME_TAG_DATA =
      "DisplayAttribute:[{Type:\"NAME_VISIBILITY\",Text:\"NEVER\"}]";
  private static final String DEFAULT_OBJECTIVE_DATA =
      "ObjectiveData:{ObjectiveDataSet:[{Type:\"LOOK_AT_PLAYER\"}]}";
  private static final String HIDDEN_HEAD_MODEL_DATA =
      "ModelData:{Pose:\"DEFAULT\",Visible:{Head:0b,Hat:0b}}";
  private static final int TOP_BAND_HEIGHT_DIVISOR = 8;
  private static final double MINIMUM_HIDDEN_HEAD_TOP_WIDTH_FACTOR = 1.4;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(32);

  private static String retainEmptyWalkwayFrame() {
    awaitWorldSetupCapability();
    placeScene("walkway", -1, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, PLAYER_STAND_Z + 0.5, 180, 0);
    client.await(Until.ticksElapsed(WALKWAY_LIGHTING_TICKS));
    return client.retainFrame();
  }

  private static double walkingNPCTopWidthRatio(
      String emptyWalkwayFrame, String animationBehavior, String modelData, String label) {
    String npcData =
        HIDDEN_NAME_TAG_DATA
            + ","
            + DEFAULT_OBJECTIVE_DATA
            + ",AnimationData:{Behavior:\""
            + animationBehavior
            + "\"}";
    if (!modelData.isEmpty()) {
      npcData += "," + modelData;
    }
    client.runCommand("summon " + NPC_TYPE + " 0.5 " + VIEW_Y + " 0.5 {" + npcData + "}");
    UUID npcUUID =
        UUID.fromString(
            client.awaitEntity(entityTypeInSearchRadius(NPC_TYPE), entity -> true).uuid());
    Parameters npcParameters = entityInSearchRadius(NPC_TYPE, npcUUID);

    client.runCommand("easy_npc objective " + npcUUID + " set follow player @s");
    client.awaitEntity(
        npcParameters, entity -> entity.distance() <= MID_WALK_DISTANCE, WALK_TIMEOUT);
    CaptureCrop npcBounds = changedBounds(emptyWalkwayFrame, MODEL_COMPARE);
    CaptureCrop topBandBounds =
        changedBounds(
            emptyWalkwayFrame,
            MODEL_COMPARE.withRegion(
                (int) npcBounds.x(),
                (int) npcBounds.y(),
                (int) npcBounds.width(),
                (int) Math.max(1, npcBounds.height() / TOP_BAND_HEIGHT_DIVISOR)));
    client.saveScreenshot(
        animationBehavior.toLowerCase(Locale.ROOT) + "_" + label + "_while_walking");

    client.runCommand("easy_npc delete " + npcUUID);
    client.await(Until.entityAbsent(Parameters.of("uuid", npcUUID.toString())));
    return (double) topBandBounds.width() / npcBounds.width();
  }

  private static CaptureCrop changedBounds(String referenceFrame, CompareOptions compareOptions) {
    return client
        .compare(referenceFrame, compareOptions)
        .comparison()
        .changedBounds()
        .orElseThrow();
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"DEFAULT", "NONE"})
  @DisplayName("Hidden head stays hidden while walking with a non-smart animation behavior")
  void hiddenHeadStaysHiddenWhileWalking(String animationBehavior) {
    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      String emptyWalkwayFrame = retainEmptyWalkwayFrame();
      double visibleHeadTopWidth =
          walkingNPCTopWidthRatio(emptyWalkwayFrame, animationBehavior, "", "visible_head");
      double hiddenHeadTopWidth =
          walkingNPCTopWidthRatio(
              emptyWalkwayFrame, animationBehavior, HIDDEN_HEAD_MODEL_DATA, "hidden_head");

      assertTrue(
          hiddenHeadTopWidth >= visibleHeadTopWidth * MINIMUM_HIDDEN_HEAD_TOP_WIDTH_FACTOR,
          animationBehavior
              + " walking NPC with hidden head has a top width ratio of "
              + hiddenHeadTopWidth
              + ", with visible head "
              + visibleHeadTopWidth);
    }
  }
}
