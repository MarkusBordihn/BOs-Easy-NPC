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

package de.markusbordihn.easynpc.configui.clienttest;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientBuilder;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientExtension;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Locale;
import java.util.Objects;
import java.util.UUID;
import org.junit.jupiter.api.extension.RegisterExtension;

abstract class ClientTestBase {

  static final String TEST_NPC_TYPE = "easy_npc:humanoid";
  static final String MAIN_CONFIGURATION_SCREEN_ID =
      "de.markusbordihn.easynpc.configui.client.screen.configuration.main."
          + "mainconfigurationscreenwrapper";

  @RegisterExtension
  static final GameClientExtension client =
      GameClientExtension.shared(ClientTestBase::configureLaunch);

  private static final String SUITE_NAME = "Easy NPC config-ui";
  private static final String WORLD_SETUP_CAPABILITY = "world.setup";
  private static final Duration WORLD_SETUP_TIMEOUT = Duration.ofMinutes(1);
  private static final int PLATFORM_Y = 100;
  static final int STAND_Y = PLATFORM_Y + 1;
  private static final String TEST_PLATFORM_FILE_NAME = "npc_platform.nbt";
  private static final String TEST_PLATFORM_RESOURCE =
      "/structures/" + TEST_PLATFORM_FILE_NAME;
  private static final String TEST_PLATFORM_DIRECTORY_PREFIX = "easy_npc_scene";

  private static GameClientBuilder configureLaunch(GameClientBuilder builder) {
    return builder
        .withSuiteName(SUITE_NAME)
        .withWindowSize(1280, 720)
        .withGuiScale(2)
        .withAllowedCommands("easy_npc_config_ui", "easy_npc")
        .withWorld()
        .withLayoutChecksOnScreenshot();
  }

  static void buildTestPlatform() {
    awaitWorldSetupCapability();
    try (InputStream testPlatformStream =
        Objects.requireNonNull(
            ClientTestBase.class.getResourceAsStream(TEST_PLATFORM_RESOURCE),
            "Missing scene " + TEST_PLATFORM_RESOURCE)) {
      Path testPlatformDirectory = Files.createTempDirectory(TEST_PLATFORM_DIRECTORY_PREFIX);
      Path testPlatformFile = testPlatformDirectory.resolve(TEST_PLATFORM_FILE_NAME);
      Files.copy(testPlatformStream, testPlatformFile);
      testPlatformDirectory.toFile().deleteOnExit();
      testPlatformFile.toFile().deleteOnExit();
      client.placeStructure(testPlatformFile, -2, PLATFORM_Y, -1);
    } catch (IOException e) {
      throw new UncheckedIOException("Unable to read the scene " + TEST_PLATFORM_RESOURCE, e);
    }
  }

  // Toolkit 1.1.0 on Forge grants world.setup a few ticks after the world has loaded.
  private static void awaitWorldSetupCapability() {
    long deadline = System.nanoTime() + WORLD_SETUP_TIMEOUT.toNanos();
    while (!hasCapability(WORLD_SETUP_CAPABILITY)) {
      if (System.nanoTime() > deadline) {
        fail("The hosting server never granted " + WORLD_SETUP_CAPABILITY);
      }

      client.await(Until.ticksElapsed(2));
    }
  }

  private static boolean hasCapability(String capabilityId) {
    return client.status().list("capabilities").stream()
        .anyMatch(capability -> capabilityId.equals(capability.string("id")));
  }

  static UUID spawnTestNpc() {
    buildTestPlatform();
    client.teleport(0.5, STAND_Y, 0.5);
    return client.spawnEntity(TEST_NPC_TYPE, 0.5, STAND_Y, 3.5, true);
  }

  static void openMainConfiguration(UUID testNpc) {
    client.runCommand(
        "easy_npc_config_ui configure " + testNpc,
        Until.screen(MAIN_CONFIGURATION_SCREEN_ID),
        Until.settled(5));
  }

  static By configurationButton(String label) {
    return By.translationKey("text.easy_npc.config." + label);
  }

  static String clickAndAwaitScreenChange(By button) {
    client.clickAndAwaitScreen(button);
    client.await(Until.settled(10));
    return client.screenId();
  }

  static String openSubPage(String categoryLabel, String tabLabel, String screenClassName) {
    By categoryButton = configurationButton(categoryLabel);
    assumeTrue(isEnabledWidget(categoryButton), categoryLabel + " is not available for this NPC");
    String openedScreenId = clickAndAwaitScreenChange(categoryButton);

    if (!isScreenOfClass(openedScreenId, screenClassName)) {
      By tabButton = configurationButton(tabLabel);
      assumeTrue(isEnabledWidget(tabButton), tabLabel + " is not available on " + openedScreenId);
      openedScreenId = clickAndAwaitScreenChange(tabButton);
    }

    assertScreenOfClass(openedScreenId, screenClassName);
    return openedScreenId;
  }

  static void assertScreenOfClass(String screenId, String screenClassName) {
    assertTrue(
        isScreenOfClass(screenId, screenClassName),
        "Expected " + screenClassName + " but " + screenId + " is open");
  }

  static boolean isEnabledWidget(By locator) {
    return client.findWidget(locator).map(widget -> widget.flag("enabled")).orElse(false);
  }

  static boolean isScreenOfClass(String screenId, String screenClassName) {
    String lowerCaseClassName = screenClassName.toLowerCase(Locale.ROOT);
    String neoForgeWrapperClassName =
        lowerCaseClassName.replace("containerscreen", "screen") + "wrapper";
    return screenId.endsWith("." + lowerCaseClassName)
        || screenId.endsWith("." + lowerCaseClassName + "wrapper")
        || screenId.endsWith("." + neoForgeWrapperClassName);
  }
}
