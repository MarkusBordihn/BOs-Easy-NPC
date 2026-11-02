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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.FileOverride;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureComparison;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Objects;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@EnabledIfModLoaded("armourers_workshop")
class ArmourersWorkshopClientTest extends ClientTestBase {

  private static final String WAND_OF_STYLE = "armourers_workshop:wand-of-style";
  private static final String WARDROBE_SCREEN_CLASS =
      "moe.plushie.armourers_workshop.compatibility.client.gui.AbstractWrappedScreen";
  private static final String TEST_SKIN_FILE_NAME = "easy_npc_test_hat.armour";
  private static final String TEST_SKIN_RESOURCE = "/armourers_workshop/" + TEST_SKIN_FILE_NAME;
  private static final String SKIN_LIBRARY_DIRECTORY = "armourers_workshop/skin-library";
  private static final double NPC_X = 0.5;
  private static final double NPC_Z = 3.5;
  private static final double NPC_HEAD_HEIGHT = 1.6;
  private static final long SETTLED_TICKS = 10;
  private static final double VISIBLE_CHANGE_RATIO = 0.0005;
  private static final CompareOptions SKIN_COMPARE =
      CompareOptions.DEFAULT
          .withTolerance(8)
          .withAwaitChangedRatioAbove(VISIBLE_CHANGE_RATIO, Duration.ofSeconds(10));

  private static byte[] readTestSkin() {
    try (InputStream testSkinStream =
        Objects.requireNonNull(
            ArmourersWorkshopClientTest.class.getResourceAsStream(TEST_SKIN_RESOURCE),
            "Missing test skin " + TEST_SKIN_RESOURCE)) {
      return testSkinStream.readAllBytes();
    } catch (IOException e) {
      throw new UncheckedIOException("Unable to read the test skin " + TEST_SKIN_RESOURCE, e);
    }
  }

  private static UUID spawnNpcInView() {
    UUID testNpc = spawnTestNpc();
    client.teleport(0.5, STAND_Y, 0.5, 0, 0);
    client.await(Until.entityPresent(Parameters.of("uuid", testNpc.toString())));
    client.lookAtPoint(NPC_X, STAND_Y + NPC_HEAD_HEIGHT, NPC_Z);
    client.await(Until.targetKind("entity"), Until.settled(SETTLED_TICKS));
    return testNpc;
  }

  @Test
  @DisplayName("Wand of Style opens the wardrobe of an Easy NPC")
  void wandOfStyleOpensWardrobe() {
    spawnNpcInView();
    client.giveItem(WAND_OF_STYLE);
    client.equip(WAND_OF_STYLE);
    client.clearChat();

    client.useHeldItem();
    client.await(Until.screenInstanceOf(WARDROBE_SCREEN_CLASS), Until.settled(SETTLED_TICKS));
    client.saveScreenshot("wardrobe");
  }

  @Test
  @DisplayName("Skin from the skin library renders on an Easy NPC")
  void librarySkinRendersOnNpc() {
    Path testSkinFile =
        client
            .requireGameDirectory()
            .resolve(SKIN_LIBRARY_DIRECTORY)
            .resolve(TEST_SKIN_FILE_NAME);
    try (FileOverride testSkin = client.overrideFile(testSkinFile, readTestSkin())) {
      UUID testNpc = spawnNpcInView();
      String npcWithoutSkinFrame = client.retainFrame();

      client.runCommand("armourers setSkin " + testNpc + " head 1 /" + TEST_SKIN_FILE_NAME);
      CaptureComparison comparison =
          client.compare(npcWithoutSkinFrame, SKIN_COMPARE).comparison();
      client.saveScreenshot("test_hat");

      assertTrue(
          comparison.ratioReached().orElse(false),
          "Test hat not visible: changedPixels=" + comparison.changedPixels());
    }
  }
}
