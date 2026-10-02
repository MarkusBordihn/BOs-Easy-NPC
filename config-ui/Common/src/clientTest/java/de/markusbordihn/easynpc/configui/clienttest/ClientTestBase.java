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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.List;
import java.util.Locale;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.extension.RegisterExtension;

abstract class ClientTestBase {

  static final String TEST_TAG = "easynpc_clienttest";
  static final String TEST_NPC_TYPE = "easy_npc:humanoid";
  static final String TEST_NPC_SELECTOR = "@e[tag=" + TEST_TAG + ",limit=1]";
  static final String MAIN_CONFIGURATION_SCREEN_ID =
      "de.markusbordihn.easynpc.configui.client.screen.configuration.main."
          + "mainconfigurationscreenwrapper";

  @RegisterExtension
  static final GameClientExtension client =
      GameClientExtension.shared(ClientTestBase::configureLaunch);

  private static final String SUITE_NAME = "Easy NPC config-ui";
  private static final String LAYOUT_REPORT_FILE_NAME = "layout-issues.txt";
  private static final Duration WORLD_TIMEOUT = Duration.ofMinutes(3);

  private static GameClientBuilder configureLaunch(GameClientBuilder builder) {
    return builder
        .withSuiteName(SUITE_NAME)
        .withWindowSize(1280, 720)
        .withGuiScale(2)
        .withConfiguration("command_allowlist", "summon,kill,tp,gamerule,easy_npc_config_ui");
  }

  @BeforeAll
  static void loadWorld() {
    if (!client.checkState(Until.worldLoaded(true)).passed()) {
      client.await(WORLD_TIMEOUT, Until.resourcesLoaded());
      client.api().world().create(Parameters.of("name", "EasyNPC Client Test"));
    }
    client.await(WORLD_TIMEOUT, Until.worldLoaded(true), Until.playerAvailable(true));
    command("gamerule send_command_feedback false");
  }

  static void command(String command) {
    client.api().command().run(Parameters.of("command", command));
    client.await(Until.ticksElapsed(5));
  }

  static void summonTestNpc() {
    command("summon " + TEST_NPC_TYPE + " ~ ~ ~3 {Tags:[\"" + TEST_TAG + "\"],NoAI:1b}");
  }

  static void openMainConfiguration() {
    command("easy_npc_config_ui configure " + TEST_NPC_SELECTOR);
    client.await(Until.screen(MAIN_CONFIGURATION_SCREEN_ID), Until.settled(5));
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

  static void captureScreen(String label) throws IOException {
    Path screenshot = client.saveScreenshot(label);
    List<String> layoutIssues = client.layoutIssues();
    if (layoutIssues.isEmpty()) {
      return;
    }

    StringBuilder report =
        new StringBuilder(client.screenshotDirectory().relativize(screenshot) + "\n");
    layoutIssues.forEach(layoutIssue -> report.append("  ").append(layoutIssue).append('\n'));
    Files.writeString(
        client.screenshotDirectory().resolve(LAYOUT_REPORT_FILE_NAME),
        report,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND);
  }

  @AfterEach
  void removeTestNpcs() {
    if (!Until.NO_SCREEN_ID.equals(client.screenId())) {
      client.closeScreen();
    }
    command("kill @e[tag=" + TEST_TAG + "]");
  }
}
