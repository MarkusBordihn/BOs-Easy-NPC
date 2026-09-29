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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.JsonValue;
import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DisplayAttributeClientTest extends ClientTestBase {

  private static final String DISPLAY_ATTRIBUTE_SCREEN = "DisplayAttributeConfigurationScreen";
  private static final By TEXT_FIELD =
      By.className("de.markusbordihn.easynpc.client.screen.components.TextField");
  private static final By SAVE_BUTTON =
      By.className("de.markusbordihn.easynpc.configui.client.screen.components.SaveButton");
  private static final int OPACITY_INDEX = 1;
  private static final String CHANGED_OPACITY = "40";
  private static final By VISIBLE_CHECKBOX = configurationButton("visible");
  private static final By VISIBLE_AT_DAY_CHECKBOX = configurationButton("visible_at_day");

  private static String openDisplayAttributes() {
    openMainConfiguration();
    return openSubPage("attributes", "display", DISPLAY_ATTRIBUTE_SCREEN);
  }

  private static void reopen() {
    client.closeScreen();
    client.await(Until.noScreen());
    openDisplayAttributes();
  }

  private static JsonValue nthWidget(By locator, int index) {
    List<JsonValue> widgets = client.findWidgets(locator);
    assertTrue(widgets.size() > index, "Expected more than " + index + " of " + locator);
    return widgets.get(index);
  }

  private static By nthWidgetLocator(By locator, int index) {
    return By.id(nthWidget(locator, index).string(By.FIELD_ID));
  }

  @Test
  @DisplayName("Saved opacity reaches the server and is shown after reopening")
  void savedOpacitySurvivesReopen() throws IOException {
    summonTestNpc();
    String screenId = openDisplayAttributes();

    client.type(nthWidgetLocator(TEXT_FIELD, OPACITY_INDEX), CHANGED_OPACITY, true);
    assertTrue(
        nthWidget(SAVE_BUTTON, OPACITY_INDEX).flag("enabled"),
        "Opacity save stays disabled after changing the value");

    client.click(nthWidgetLocator(SAVE_BUTTON, OPACITY_INDEX), screenId);
    reopen();

    assertEquals(CHANGED_OPACITY, nthWidget(TEXT_FIELD, OPACITY_INDEX).string("value"));
    captureScreen("reopened");
  }

  @Test
  @DisplayName("Unchecked visibility reaches the server and disables the dependent checkboxes")
  void uncheckedVisibilitySurvivesReopen() throws IOException {
    summonTestNpc();
    openDisplayAttributes();
    assertTrue(isEnabledWidget(VISIBLE_AT_DAY_CHECKBOX), "Visible at day is disabled on a new NPC");

    client.click(VISIBLE_CHECKBOX);
    reopen();

    assertFalse(
        isEnabledWidget(VISIBLE_AT_DAY_CHECKBOX),
        "Visible at day is still enabled after the NPC was made invisible");
    captureScreen("reopened");
  }
}
