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
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import java.io.IOException;
import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class NameChangeClientTest extends ClientTestBase {

  private static final By NAME_FIELD =
      By.className("de.markusbordihn.easynpc.client.screen.components.TextField");
  private static final By SAVE_BUTTON =
      By.className("de.markusbordihn.easynpc.configui.client.screen.components.SaveButton");
  private static final String NEW_NAME = "Renamed Tester";

  @Test
  @DisplayName("Saved name reaches the server and is shown after reopening")
  void savedNameSurvivesReopen() throws IOException {
    summonTestNpc();
    openMainConfiguration();

    client.type(NAME_FIELD, NEW_NAME, true);
    assertTrue(isEnabledWidget(SAVE_BUTTON), "Save stays disabled after changing the name");

    client.click(SAVE_BUTTON, MAIN_CONFIGURATION_SCREEN_ID);
    client.awaitEntity(
        Parameters.of("type", TEST_NPC_TYPE),
        entity -> entity.customName().equals(Optional.of(NEW_NAME)));
    client.closeScreen();
    client.await(Until.noScreen());
    openMainConfiguration();

    assertEquals(NEW_NAME, client.findWidget(NAME_FIELD).orElseThrow().string("value"));
    captureScreen("reopened");
  }
}
