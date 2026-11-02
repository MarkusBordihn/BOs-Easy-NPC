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
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class BossBarAttributeClientTest extends ClientTestBase {

  private static final String DISPLAY_ATTRIBUTE_SCREEN = "DisplayAttributeConfigurationScreen";
  private static final String BOSS_BAR_ATTRIBUTE_SCREEN = "BossBarAttributeConfigurationScreen";
  private static final String TEXT_FIELD_CLASS =
      "de.markusbordihn.easynpc.client.screen.components.TextField";
  private static final String SAVE_BUTTON_CLASS =
      "de.markusbordihn.easynpc.configui.client.screen.components.SaveButton";
  private static final By BOSS_BAR_SETTINGS_BUTTON = configurationButton("boss_bar_settings");
  private static final By SHOW_BOSS_BAR_CHECKBOX = configurationButton("show_boss_bar");
  private static final By RANGE_TEXT_FIELD = By.className(TEXT_FIELD_CLASS).withIndex(0);
  private static final By RANGE_SAVE_BUTTON = By.className(SAVE_BUTTON_CLASS).withIndex(0);
  private static final By AUTO_HIDE_TEXT_FIELD = By.className(TEXT_FIELD_CLASS).withIndex(1);
  private static final By AUTO_HIDE_SAVE_BUTTON = By.className(SAVE_BUTTON_CLASS).withIndex(1);
  private static final String CHANGED_RANGE = "48";
  private static final String CHANGED_AUTO_HIDE_SECONDS = "15";
  private static final String TOO_LONG_AUTO_HIDE_SECONDS = "601";

  private static void openBossBarAttributes(UUID testNpc) {
    openMainConfiguration(testNpc);
    openSubPage("attributes", "display", DISPLAY_ATTRIBUTE_SCREEN);
    String screenId = clickAndAwaitScreenChange(BOSS_BAR_SETTINGS_BUTTON);
    assertScreenOfClass(screenId, BOSS_BAR_ATTRIBUTE_SCREEN);
  }

  private static void typeAndSave(By textField, By saveButton, String value) {
    client.type(textField, value, true);
    assertTrue(isEnabledWidget(saveButton), "Changed value " + value + " cannot be saved");
    client.click(saveButton);
    client.await(Until.settled(5));
  }

  @Test
  @DisplayName("Saved boss bar settings reach the server and are shown after reopening")
  void savedBossBarSettingsSurviveReopen() {
    UUID testNpc = spawnTestNpc();
    openBossBarAttributes(testNpc);

    client.click(SHOW_BOSS_BAR_CHECKBOX);
    client.await(Until.settled(5));
    typeAndSave(RANGE_TEXT_FIELD, RANGE_SAVE_BUTTON, CHANGED_RANGE);
    client.type(AUTO_HIDE_TEXT_FIELD, TOO_LONG_AUTO_HIDE_SECONDS, true);
    assertFalse(
        isEnabledWidget(AUTO_HIDE_SAVE_BUTTON), "Auto hide above the maximum can be saved");
    typeAndSave(AUTO_HIDE_TEXT_FIELD, AUTO_HIDE_SAVE_BUTTON, CHANGED_AUTO_HIDE_SECONDS);

    runCommandAndAwaitFeedback(
        "easy_npc boss_bar " + testNpc + " info",
        ": shown, color purple, style progress, range "
            + CHANGED_RANGE
            + " blocks, auto hide "
            + CHANGED_AUTO_HIDE_SECONDS
            + " s");

    openBossBarAttributes(testNpc);
    assertEquals(CHANGED_RANGE, client.widget(RANGE_TEXT_FIELD).string("value"));
    assertEquals(CHANGED_AUTO_HIDE_SECONDS, client.widget(AUTO_HIDE_TEXT_FIELD).string("value"));
    client.saveScreenshot("reopened");
  }
}
