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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.extension.RegisterExtension;

abstract class ClientTestBase {

  static final String TEST_TAG = "easynpc_clienttest";
  static final String TEST_NPC_SELECTOR = "@e[tag=" + TEST_TAG + ",limit=1]";

  @RegisterExtension
  static final GameClientExtension client =
      GameClientExtension.shared(ClientTestBase::configureLaunch);

  private static final String SUITE_NAME = "Easy NPC core";
  private static final String LAYOUT_REPORT_FILE_NAME = "layout-issues.txt";
  private static final Duration WORLD_TIMEOUT = Duration.ofMinutes(3);

  private static GameClientBuilder configureLaunch(GameClientBuilder builder) {
    return builder
        .withSuiteName(SUITE_NAME)
        .withWindowSize(1280, 720)
        .withGuiScale(2)
        .withConfiguration("command_allowlist", "summon,kill,tp,gamerule,easy_npc");
  }

  @BeforeAll
  static void loadWorld() {
    if (!client.checkState(Until.worldLoaded(true)).passed()) {
      client.await(WORLD_TIMEOUT, Until.resourcesLoaded());
      client.api().world().create(Parameters.of("name", "EasyNPC Client Test"));
    }
    client.await(WORLD_TIMEOUT, Until.worldLoaded(true), Until.playerAvailable(true));
    command("gamerule sendCommandFeedback false");
  }

  static void command(String command) {
    client.api().command().run(Parameters.of("command", command));
    client.await(Until.ticksElapsed(5));
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
