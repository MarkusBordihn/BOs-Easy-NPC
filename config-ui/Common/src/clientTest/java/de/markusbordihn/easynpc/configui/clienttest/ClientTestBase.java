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
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientBuilder;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientExtension;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.TestArea;
import java.util.Locale;
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
  private static final String WORLD_NAME = "EasyNPC Client Test";
  private static final int PLATFORM_Y = 100;
  static final int STAND_Y = PLATFORM_Y + 1;
  private static final TestArea TEST_PLATFORM =
      TestArea.between(-2, PLATFORM_Y, -1, 2, PLATFORM_Y, 4);

  private static GameClientBuilder configureLaunch(GameClientBuilder builder) {
    return builder
        .withSuiteName(SUITE_NAME)
        .withWindowSize(1280, 720)
        .withGuiScale(2)
        .withAllowedCommands("easy_npc_config_ui", "easy_npc")
        .withWorld(Parameters.of("name", WORLD_NAME))
        .withLayoutChecksOnScreenshot();
  }

  static void buildTestPlatform() {
    client.fill(TEST_PLATFORM, "minecraft:stone");
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

  // Forge registers the Common screen classes, Fabric a "<name>Wrapper" subclass in its own
  // package.
  static boolean isScreenOfClass(String screenId, String screenClassName) {
    String lowerCaseClassName = screenClassName.toLowerCase(Locale.ROOT);
    return screenId.endsWith("." + lowerCaseClassName)
        || screenId.endsWith("." + lowerCaseClassName + "wrapper");
  }
}
