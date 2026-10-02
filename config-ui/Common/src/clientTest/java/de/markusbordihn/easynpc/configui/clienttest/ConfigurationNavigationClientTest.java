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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import java.io.IOException;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ConfigurationNavigationClientTest extends ClientTestBase {

  @Test
  @DisplayName("Main configuration screen renders")
  void mainConfigurationScreenRenders() throws IOException {
    summonTestNpc();
    openMainConfiguration();

    captureScreen("main");
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(
      strings = {
        "actions",
        "attributes",
        "dialog",
        "equipment",
        "objective",
        "pose",
        "position",
        "rotation",
        "scaling",
        "sound",
        "trading",
        "edit_skin",
        "change_model",
        "import",
        "export"
      })
  @DisplayName("Main configuration button opens and renders its configuration screen")
  void buttonOpensConfigurationScreen(String buttonLabel) throws IOException {
    summonTestNpc();
    openMainConfiguration();

    By button = configurationButton(buttonLabel);
    Assumptions.assumeTrue(isEnabledWidget(button), buttonLabel + " is not available for this NPC");

    String openedScreenId = clickAndAwaitScreenChange(button);
    assertTrue(
        openedScreenId.startsWith("de.markusbordihn.easynpc."),
        buttonLabel + " opened " + openedScreenId);
    assertEquals(openedScreenId, client.screenId());
    captureScreen(buttonLabel);
  }
}
