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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Screenshot;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Weather;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureCrop;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Arrays;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class RenderLayerClientTest extends ClientTestBase {

  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final long SETTLED_TICKS = 5;
  private static final long LAYER_RENDER_TICKS = 20;
  private static final long PLATFORM_LIGHTING_TICKS = 40;
  private static final long NOON_DAY_TIME = 6000;
  private static final long MIDNIGHT_DAY_TIME = 18000;
  private static final String HUMANOID_TYPE = "easy_npc:humanoid";
  private static final String ZOMBIE_TYPE = "easy_npc:zombie";
  private static final String HORSE_TYPE = "easy_npc:horse";
  private static final String HIDDEN_NAME_TAG_ATTRIBUTE =
      "{Type:\"NAME_VISIBILITY\",Text:\"NEVER\"}";
  private static final String DEFAULT_SKIN_DATA = "SkinData:{Type:\"DEFAULT\"}";
  private static final String DISABLED_LAYERS_SKIN_DATA =
      "SkinData:{Type:\"DEFAULT\",DisableLayers:1b}";
  private static final String CUSTOM_POSE_MODEL_DATA = "ModelData:{Pose:\"CUSTOM\"}";
  private static final String HIDDEN_HEAD_MODEL_DATA =
      "ModelData:{Pose:\"CUSTOM\",Visible:{Head:0b}}";
  private static final String HIDDEN_HELMET_MODEL_DATA = "ModelData:{Visible:{Helmet:0b}}";
  private static final String HELMET_EQUIPMENT_DATA =
      "ArmorItems:[{},{},{},{id:\"minecraft:diamond_helmet\",Count:1b}]";
  private static final String SKULL_EQUIPMENT_DATA =
      "ArmorItems:[{},{},{},{id:\"minecraft:skeleton_skull\",Count:1b}]";
  private static final String HALF_OPACITY_ATTRIBUTE = "{Type:\"OPACITY\",Int:50}";
  private static final String FULL_LIGHT_LEVEL_ATTRIBUTE = "{Type:\"LIGHT_LEVEL\",Int:15}";
  private static final String INVISIBLE_ATTRIBUTE = "{Type:\"VISIBLE\",Bool:0b}";
  private static final double MINIMUM_LAYER_CHANGED_RATIO = 0.1;
  private static final double MINIMUM_HEAD_CHANGED_RATIO = 0.15;
  private static final double MAXIMUM_HIDDEN_HEAD_ITEM_CHANGED_RATIO = 0.03;
  private static final double MINIMUM_OPACITY_CHANGED_RATIO = 0.2;
  private static final double MAXIMUM_INVISIBLE_CHANGED_RATIO = 0.03;
  private static final double MINIMUM_LIGHT_LEVEL_LUMINANCE_GAIN = 15;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(32);

  private static double fullAreaChange(
      EntitySpawn npcSpawn,
      String referenceData,
      String testedData,
      String label,
      String... testedDisplayAttributes) {
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID referenceNPCUUID = spawnFacingPlayer(npcSpawn, referenceData);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    String referenceFrame = client.retainFrame();
    removeAndAwaitAbsent(referenceNPCUUID);

    UUID testedNPCUUID = spawnFacingPlayer(npcSpawn, testedData, testedDisplayAttributes);
    client.await(Until.ticksElapsed(LAYER_RENDER_TICKS));
    double change = regionChange(referenceFrame, npcBounds, npcBounds.height());
    client.saveScreenshot(label);
    removeAndAwaitAbsent(testedNPCUUID);
    return change;
  }

  private static double headAreaChange(
      EntitySpawn npcSpawn, String referenceData, String testedData, String label) {
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID referenceNPCUUID = spawnFacingPlayer(npcSpawn, referenceData);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    String referenceFrame = client.retainFrame();
    removeAndAwaitAbsent(referenceNPCUUID);

    UUID testedNPCUUID = spawnFacingPlayer(npcSpawn, testedData);
    client.await(Until.ticksElapsed(LAYER_RENDER_TICKS));
    double change = regionChange(referenceFrame, npcBounds, npcBounds.height() / 4);
    client.saveScreenshot(label);
    removeAndAwaitAbsent(testedNPCUUID);
    return change;
  }

  private static String retainEmptyPlatformFrame() {
    awaitWorldSetupCapability();
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
    client.await(Until.ticksElapsed(PLATFORM_LIGHTING_TICKS));
    return client.retainFrame();
  }

  private static String displayAttributes(String... attributes) {
    return Stream.concat(Stream.of(HIDDEN_NAME_TAG_ATTRIBUTE), Arrays.stream(attributes))
        .collect(Collectors.joining(",", "DisplayAttribute:[", "]"));
  }

  private static String joinNPCData(String... npcDataParts) {
    return String.join(
        ",", Arrays.stream(npcDataParts).filter(npcDataPart -> !npcDataPart.isEmpty()).toList());
  }

  private static UUID spawnFacingPlayer(
      EntitySpawn npcSpawn, String npcData, String... npcDisplayAttributes) {
    UUID npcUUID =
        client.spawnEntity(
            npcSpawn
                .withRotation(180, 0)
                .withNoAi(true)
                .withNbt(
                    "{" + joinNPCData(displayAttributes(npcDisplayAttributes), npcData) + "}"));
    client.await(
        Until.entityPresent(
            Parameters.of("type", npcSpawn.type()).with("uuid", npcUUID.toString())));
    client.await(Until.settled(SETTLED_TICKS));
    return npcUUID;
  }

  private static CaptureCrop changedBounds(String referenceFrame) {
    return client.compare(referenceFrame, MODEL_COMPARE).comparison().changedBounds().orElseThrow();
  }

  private static double regionChange(String referenceFrame, CaptureCrop npcBounds, long height) {
    return client
        .compare(
            referenceFrame,
            MODEL_COMPARE.withRegion(
                (int) npcBounds.x(),
                (int) npcBounds.y(),
                (int) npcBounds.width(),
                (int) Math.max(1, height)))
        .comparison()
        .changedRatio();
  }

  private static double averageLuminance(Screenshot screenshot, CaptureCrop region) {
    BufferedImage image;
    try {
      image = ImageIO.read(new ByteArrayInputStream(screenshot.imageBytes()));
    } catch (IOException e) {
      throw new UncheckedIOException("Unable to decode screenshot", e);
    }
    double luminanceSum = 0;
    long pixelCount = 0;
    for (long y = region.y(); y < region.y() + region.height(); y++) {
      for (long x = region.x(); x < region.x() + region.width(); x++) {
        int rgb = image.getRGB((int) x, (int) y);
        luminanceSum +=
            0.2126 * ((rgb >> 16) & 0xff) + 0.7152 * ((rgb >> 8) & 0xff) + 0.0722 * (rgb & 0xff);
        pixelCount++;
      }
    }
    return luminanceSum / Math.max(1, pixelCount);
  }

  private static void removeAndAwaitAbsent(UUID npcUUID) {
    client.removeEntity(npcUUID);
    client.await(Until.entityAbsent(Parameters.of("uuid", npcUUID.toString())));
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"drowned", "stray", "villager"})
  @DisplayName("Disabled skin layers remove the overlay layer")
  void disabledSkinLayersRemoveOverlayLayer(String entityType) {
    EntitySpawn npcSpawn = EntitySpawn.of("easy_npc:" + entityType, 0.5, VIEW_Y, 3.5);

    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      double layerChange =
          fullAreaChange(
              npcSpawn,
              DEFAULT_SKIN_DATA,
              DISABLED_LAYERS_SKIN_DATA,
              entityType + "_disabled_layers");

      assertTrue(
          layerChange >= MINIMUM_LAYER_CHANGED_RATIO,
          entityType + " with disabled layers differs only in " + layerChange);
    }
  }

  @Test
  @DisplayName("Hidden horse head is not rendered")
  void hiddenHorseHeadIsNotRendered() {
    EntitySpawn npcSpawn = EntitySpawn.of(HORSE_TYPE, 0.5, VIEW_Y, 4.5);

    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      double headChange =
          headAreaChange(
              npcSpawn, CUSTOM_POSE_MODEL_DATA, HIDDEN_HEAD_MODEL_DATA, "horse_hidden_head");

      assertTrue(
          headChange >= MINIMUM_HEAD_CHANGED_RATIO,
          "Horse with hidden head differs only in " + headChange + " of its head area");
    }
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"helmet", "skull"})
  @DisplayName("Hidden helmet slot renders like an empty head slot")
  void hiddenHelmetSlotRendersLikeEmptyHeadSlot(String headItem) {
    EntitySpawn npcSpawn = EntitySpawn.of(ZOMBIE_TYPE, 0.5, VIEW_Y, 3.5);
    String headItemData = headItem.equals("skull") ? SKULL_EQUIPMENT_DATA : HELMET_EQUIPMENT_DATA;

    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      double visibleHeadItemChange =
          headAreaChange(npcSpawn, "", headItemData, "zombie_visible_" + headItem);
      double hiddenHeadItemChange =
          headAreaChange(
              npcSpawn,
              HIDDEN_HELMET_MODEL_DATA,
              joinNPCData(headItemData, HIDDEN_HELMET_MODEL_DATA),
              "zombie_hidden_" + headItem);

      assertTrue(
          visibleHeadItemChange >= MINIMUM_HEAD_CHANGED_RATIO,
          "Visible " + headItem + " changes only " + visibleHeadItemChange + " of the head area");
      assertTrue(
          hiddenHeadItemChange <= MAXIMUM_HIDDEN_HEAD_ITEM_CHANGED_RATIO,
          "Hidden " + headItem + " still changes " + hiddenHeadItemChange + " of the head area");
    }
  }

  @Test
  @DisplayName("Half opacity renders the NPC translucent")
  void halfOpacityRendersTranslucent() {
    EntitySpawn npcSpawn = EntitySpawn.of(HUMANOID_TYPE, 0.5, VIEW_Y, 3.5);

    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      double opacityChange =
          fullAreaChange(
              npcSpawn,
              DEFAULT_SKIN_DATA,
              DEFAULT_SKIN_DATA,
              "humanoid_half_opacity",
              HALF_OPACITY_ATTRIBUTE);

      assertTrue(
          opacityChange >= MINIMUM_OPACITY_CHANGED_RATIO,
          "Half opacity differs only in " + opacityChange + " of the NPC area");
    }
  }

  @Test
  @DisplayName("Light level attribute renders the NPC brighter at night")
  void lightLevelRendersBrighterAtNight() {
    EntitySpawn npcSpawn = EntitySpawn.of(HUMANOID_TYPE, 0.5, VIEW_Y, 3.5);

    CaptureCrop npcBounds;
    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      String emptyPlatformFrame = retainEmptyPlatformFrame();
      UUID boundsNPCUUID = spawnFacingPlayer(npcSpawn, DEFAULT_SKIN_DATA);
      npcBounds = changedBounds(emptyPlatformFrame);
      removeAndAwaitAbsent(boundsNPCUUID);
    }

    try (TemporaryChange midnight = client.setDayTime(MIDNIGHT_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      client.await(Until.ticksElapsed(PLATFORM_LIGHTING_TICKS));
      UUID litNPCUUID = spawnFacingPlayer(npcSpawn, DEFAULT_SKIN_DATA, FULL_LIGHT_LEVEL_ATTRIBUTE);
      client.await(Until.ticksElapsed(LAYER_RENDER_TICKS));
      Screenshot litScreenshot = client.screenshot();
      client.saveScreenshot("humanoid_light_level_at_night");
      removeAndAwaitAbsent(litNPCUUID);

      UUID unlitNPCUUID = spawnFacingPlayer(npcSpawn, DEFAULT_SKIN_DATA);
      client.await(Until.ticksElapsed(LAYER_RENDER_TICKS));
      Screenshot unlitScreenshot = client.screenshot();
      client.saveScreenshot("humanoid_default_light_at_night");
      removeAndAwaitAbsent(unlitNPCUUID);

      double litLuminance = averageLuminance(litScreenshot, npcBounds);
      double unlitLuminance = averageLuminance(unlitScreenshot, npcBounds);
      assertTrue(
          litLuminance - unlitLuminance >= MINIMUM_LIGHT_LEVEL_LUMINANCE_GAIN,
          "Light level 15 luminance " + litLuminance + " vs default " + unlitLuminance);
    }
  }

  @Test
  @DisplayName("Invisible NPC renders an empty platform")
  void invisibleNPCRendersEmptyPlatform() {
    EntitySpawn npcSpawn = EntitySpawn.of(HUMANOID_TYPE, 0.5, VIEW_Y, 3.5);

    try (TemporaryChange noon = client.setDayTime(NOON_DAY_TIME);
        TemporaryChange clearWeather = client.setWeather(Weather.CLEAR)) {
      String emptyPlatformFrame = retainEmptyPlatformFrame();
      UUID visibleNPCUUID = spawnFacingPlayer(npcSpawn, DEFAULT_SKIN_DATA);
      CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
      removeAndAwaitAbsent(visibleNPCUUID);

      UUID invisibleNPCUUID = spawnFacingPlayer(npcSpawn, DEFAULT_SKIN_DATA, INVISIBLE_ATTRIBUTE);
      client.await(Until.ticksElapsed(LAYER_RENDER_TICKS));
      double invisibleChange = regionChange(emptyPlatformFrame, npcBounds, npcBounds.height());
      client.saveScreenshot("humanoid_invisible");
      removeAndAwaitAbsent(invisibleNPCUUID);

      assertTrue(
          invisibleChange <= MAXIMUM_INVISIBLE_CHANGED_RATIO,
          "Invisible NPC still changes " + invisibleChange + " of its area");
    }
  }
}
