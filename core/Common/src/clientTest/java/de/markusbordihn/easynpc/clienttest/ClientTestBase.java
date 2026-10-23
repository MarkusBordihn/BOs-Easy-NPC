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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientBuilder;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientExtension;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureCrop;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Supplier;
import org.junit.jupiter.api.extension.RegisterExtension;

abstract class ClientTestBase {

  @RegisterExtension
  static final GameClientExtension client =
      GameClientExtension.shared(ClientTestBase::configureLaunch);

  private static final String SUITE_NAME = "Easy NPC core";
  private static final String WORLD_SETUP_CAPABILITY = "world.setup";
  private static final String SCENE_RESOURCE_DIRECTORY = "/structures/";
  private static final String SCENE_DIRECTORY_PREFIX = "easy_npc_scene";
  private static final int MAXIMUM_ENTITY_SEARCH_RADIUS = 32;
  private static final int SKY_DRIFT_PIXEL_TOLERANCE = 32;
  private static final CompareOptions PLAYER_AREA_COMPARE =
      CompareOptions.DEFAULT.withTolerance(SKY_DRIFT_PIXEL_TOLERANCE).withRegion(160, 0, 320, 360);

  // Player.aiStep ignores fall triggers for 20 ticks after an entity landed on the shoulder.
  private static final long SHOULDER_GUARD_TICKS = 30;

  private static GameClientBuilder configureLaunch(GameClientBuilder builder) {
    return builder
        .withSuiteName(SUITE_NAME)
        .withWindowSize(1280, 720)
        .withGuiScale(2)
        .withAllowedCommands(
            "summon", "kill", "give", "clear", "scoreboard", "tag", "gamemode", "easy_npc")
        .withWorld()
        .withLayoutChecksOnScreenshot();
  }

  static boolean isEnabledWidget(By locator) {
    return client.findWidget(locator).map(widget -> widget.flag("enabled")).orElse(false);
  }

  static void awaitWorldSetupCapability() {
    client.awaitCapability(WORLD_SETUP_CAPABILITY);
  }

  static void placeScene(String sceneName, int x, int y, int z) {
    String sceneResource = SCENE_RESOURCE_DIRECTORY + sceneName + ".nbt";
    try (InputStream sceneStream =
        Objects.requireNonNull(
            ClientTestBase.class.getResourceAsStream(sceneResource),
            "Missing scene " + sceneResource)) {
      Path sceneDirectory = Files.createTempDirectory(SCENE_DIRECTORY_PREFIX);
      Path sceneFile = sceneDirectory.resolve(sceneName + ".nbt");
      Files.copy(sceneStream, sceneFile);
      sceneDirectory.toFile().deleteOnExit();
      sceneFile.toFile().deleteOnExit();
      client.placeStructure(sceneFile, x, y, z);
    } catch (IOException e) {
      throw new UncheckedIOException("Unable to read the scene " + sceneResource, e);
    }
  }

  static Parameters entityTypeInSearchRadius(String entityType) {
    return Parameters.of("type", entityType).with("radius", MAXIMUM_ENTITY_SEARCH_RADIUS);
  }

  static Parameters entityInSearchRadius(String entityType, UUID entityUUID) {
    return entityTypeInSearchRadius(entityType).with("uuid", entityUUID.toString());
  }

  static long seatedShoulderNPCTop(
      Supplier<UUID> npcSpawner, double shoulderOffsetY, String screenshotName) {
    String playerOnlyFrame = client.retainFrame();
    UUID npcUUID = npcSpawner.get();
    Parameters npcQuery = Parameters.of("uuid", npcUUID.toString());
    client.runCommand("easy_npc shoulder offset " + npcUUID + " 0 " + shoulderOffsetY + " 0");
    client.runCommand("easy_npc shoulder sit " + npcUUID + " @s");
    client.await(Until.entityAbsent(npcQuery));
    client.await(Until.ticksElapsed(SHOULDER_GUARD_TICKS));
    CaptureCrop seatedBounds =
        client
            .compare(playerOnlyFrame, PLAYER_AREA_COMPARE)
            .comparison()
            .changedBounds()
            .orElseThrow();
    client.saveScreenshot(screenshotName);
    client.runCommand("easy_npc shoulder dismount @s");
    client.await(Until.entityPresent(npcQuery));
    client.await(Until.playerShoulderEmpty());
    client.runCommand("easy_npc delete " + npcUUID);
    client.await(Until.entityAbsent(npcQuery));
    return seatedBounds.y();
  }
}
