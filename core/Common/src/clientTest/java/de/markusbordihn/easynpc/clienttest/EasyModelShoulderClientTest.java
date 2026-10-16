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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.TestArea;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@EnabledIfModLoaded("easy_model_entities")
class EasyModelShoulderClientTest extends ClientTestBase {

  private static final String EASY_MODEL_NPC_TYPE = "easy_npc:easy_model_entities_npc";
  private static final String SHOULDER_PROFILE = "easy_model_entities_examples:entity/dawn_sparrow";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final TestArea VIEWING_PLATFORM =
      TestArea.between(-3, PLATFORM_Y, -1, 3, PLATFORM_Y, 6);
  private static final int GUI_WIDTH = 640;
  private static final int GUI_HEIGHT = 360;
  private static final int HALF_GUI_WIDTH = GUI_WIDTH / 2;
  private static final long RENDER_TICKS = 20;
  private static final long VISIBLE_CHANGE_PIXELS = 500;
  private static final double SHOULDER_OFFSET_UP = 0.25;
  private static final double SHOULDER_OFFSET_DOWN = -0.25;
  private static final long SKY_SETTLE_AFTER_TELEPORT_TICKS = 100;
  private static final CompareOptions LEFT_HALF_COMPARE =
      CompareOptions.DEFAULT.withTolerance(8).withRegion(0, 0, HALF_GUI_WIDTH, GUI_HEIGHT);
  private static final CompareOptions RIGHT_HALF_COMPARE =
      CompareOptions.DEFAULT
          .withTolerance(8)
          .withRegion(HALF_GUI_WIDTH, 0, HALF_GUI_WIDTH, GUI_HEIGHT);

  // Player.aiStep ignores fall triggers for 20 ticks after an entity landed on the shoulder.
  private static final long SHOULDER_GUARD_TICKS = 30;

  private static long changedPixelsSince(String frameId, CompareOptions compareOptions) {
    return client.compare(frameId, compareOptions).comparison().changedPixels();
  }

  private static UUID spawnShoulderProfileNPC() {
    UUID npcUUID = client.spawnEntity(EASY_MODEL_NPC_TYPE, 0.5, VIEW_Y, 2.5, true);
    client.await(Until.entityPresent(Parameters.of("uuid", npcUUID.toString())));
    client.runCommand("easy_npc render set species " + npcUUID + " \"" + SHOULDER_PROFILE + "\"");
    return npcUUID;
  }

  @BeforeEach
  void standOnPlatform() {
    awaitWorldSetupCapability();
    client.fill(VIEWING_PLATFORM, "minecraft:stone");
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
  }

  @ParameterizedTest(name = "{0}")
  @CsvSource({"front, true", "back, false"})
  @DisplayName("Easy Model NPC on the left shoulder renders on the matching screen side")
  void easyModelNPCRendersOnLeftShoulder(String perspective, boolean leftShoulderOnRightHalf) {
    client.renderSettings(Parameters.of().with("perspective", perspective));
    try {
      client.await(Until.ticksElapsed(RENDER_TICKS));
      String playerOnlyFrame = client.retainFrame();

      UUID npcUUID = spawnShoulderProfileNPC();
      client.runCommand("easy_npc shoulder sit " + npcUUID + " @s");
      client.await(Until.entityAbsent(Parameters.of("uuid", npcUUID.toString())));
      client.await(Until.ticksElapsed(SHOULDER_GUARD_TICKS));
      long leftHalfPixels = changedPixelsSince(playerOnlyFrame, LEFT_HALF_COMPARE);
      long rightHalfPixels = changedPixelsSince(playerOnlyFrame, RIGHT_HALF_COMPARE);
      client.saveScreenshot("easy_model_shoulder_" + perspective);

      client.runCommand("easy_npc shoulder dismount @s");
      client.runCommand("easy_npc delete " + npcUUID);
      long shoulderSidePixels = leftShoulderOnRightHalf ? rightHalfPixels : leftHalfPixels;
      long otherSidePixels = leftShoulderOnRightHalf ? leftHalfPixels : rightHalfPixels;
      assertTrue(
          shoulderSidePixels > VISIBLE_CHANGE_PIXELS,
          "NPC not visible on the shoulder side: " + shoulderSidePixels);
      assertTrue(
          shoulderSidePixels > otherSidePixels,
          "NPC rendered on the wrong side: " + shoulderSidePixels + " <= " + otherSidePixels);
    } finally {
      client.renderSettings(Parameters.of().with("perspective", "first"));
    }
  }

  @Test
  @DisplayName("A shoulder offset moves the seated Easy Model NPC up or down on the shoulder")
  void shoulderOffsetMovesSeatedEasyModelNPC() {
    client.renderSettings(Parameters.of().with("perspective", "front"));
    try {
      client.await(Until.ticksElapsed(SKY_SETTLE_AFTER_TELEPORT_TICKS));
      long defaultTop =
          seatedShoulderNPCTop(
              EasyModelShoulderClientTest::spawnShoulderProfileNPC,
              0.0,
              "easy_model_offset_default");
      long raisedTop =
          seatedShoulderNPCTop(
              EasyModelShoulderClientTest::spawnShoulderProfileNPC,
              SHOULDER_OFFSET_UP,
              "easy_model_offset_up");
      long loweredTop =
          seatedShoulderNPCTop(
              EasyModelShoulderClientTest::spawnShoulderProfileNPC,
              SHOULDER_OFFSET_DOWN,
              "easy_model_offset_down");

      String tops = "default " + defaultTop + ", up " + raisedTop + ", down " + loweredTop;
      assertTrue(raisedTop < defaultTop, "offset up did not raise the NPC: " + tops);
      assertTrue(loweredTop > defaultTop, "offset down did not lower the NPC: " + tops);
    } finally {
      client.renderSettings(Parameters.of().with("perspective", "first"));
    }
  }
}
