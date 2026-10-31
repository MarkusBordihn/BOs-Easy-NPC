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

import static org.junit.jupiter.api.Assertions.assertFalse;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.By;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameLog;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import java.time.Duration;
import java.util.regex.Pattern;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SkinErrorMessageClientTest extends ClientTestBase {

  private static final String TEXT_FIELD_CLASS =
      "de.markusbordihn.easynpc.client.screen.components.TextField";
  private static final String NON_IMAGE_URL = "https://example.org/";
  private static final Duration REMOTE_IMAGE_VALIDATION_TIMEOUT = Duration.ofSeconds(20);

  private static void typeIntoTextField(String text) {
    client.type(By.className(TEXT_FIELD_CLASS), text, true);
    client.await(Until.settled(5));
  }

  @Test
  @DisplayName("URL skin shows the invalid remote image error for a non-image URL")
  void urlSkinShowsInvalidRemoteImageError() {
    openMainConfiguration(spawnTestNpc());
    openSubPage("edit_skin", "url_skin", "UrlSkinConfigurationScreen");
    By addButton = configurationButton("add");

    typeIntoTextField("example.org");
    assertFalse(isEnabledWidget(addButton), "Add must stay disabled for a URL without scheme");

    typeIntoTextField(NON_IMAGE_URL);
    client.await(Until.widgetEnabled(addButton));

    GameLog.Mark beforeAdd = client.gameLog().mark();
    client.click(addButton);
    client.awaitLogLine(
        beforeAdd,
        Pattern.compile(Pattern.quote("Unable to set remote user texture to " + NON_IMAGE_URL)),
        REMOTE_IMAGE_VALIDATION_TIMEOUT);
    client.await(Until.settled(5));
    client.saveScreenshot("url_skin-invalid_remote_image");

    typeIntoTextField(NON_IMAGE_URL);
    client.await(Until.widgetEnabled(addButton));
  }

  @Test
  @DisplayName("Player skin keeps add disabled for an invalid player name")
  void playerSkinRejectsInvalidPlayerName() {
    openMainConfiguration(spawnTestNpc());
    openSubPage("edit_skin", "player_skin", "PlayerSkinConfigurationScreen");
    By addButton = configurationButton("add");

    typeIntoTextField("invalid name!");
    assertFalse(isEnabledWidget(addButton), "Add must stay disabled for an invalid player name");

    typeIntoTextField("Steve");
    client.await(Until.widgetEnabled(addButton));
  }
}
