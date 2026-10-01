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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import java.io.IOException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class BasicDialogClientTest extends ClientTestBase {

  private static final String BASIC_DIALOG_SCREEN = "BasicDialogConfigurationScreen";
  private static final By DIALOG_FIELD =
      By.className("de.markusbordihn.easynpc.client.screen.components.TextField");
  private static final By SAVE_BUTTON =
      By.className("de.markusbordihn.easynpc.configui.client.screen.components.SaveButton");
  private static final String DIALOG_TEXT = "Hello from the client test";

  private static String openBasicDialog() {
    openMainConfiguration();
    return openSubPage("dialog", "basic", BASIC_DIALOG_SCREEN);
  }

  @Test
  @DisplayName("Saved basic dialog text reaches the server and is shown after reopening")
  void savedDialogTextSurvivesReopen() throws IOException {
    summonTestNpc();
    String screenId = openBasicDialog();

    client.type(DIALOG_FIELD, DIALOG_TEXT, true);
    assertTrue(isEnabledWidget(SAVE_BUTTON), "Save stays disabled after changing the text");

    client.click(SAVE_BUTTON, screenId);
    client.closeScreen();
    client.await(Until.noScreen());
    openBasicDialog();

    assertEquals(DIALOG_TEXT, client.findWidget(DIALOG_FIELD).orElseThrow().string("value"));
    captureScreen("reopened");
  }
}
