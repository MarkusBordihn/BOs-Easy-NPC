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

import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.EnabledIfModLoaded;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@EnabledIfModLoaded("easy_model_entities")
class EasyModelEntitiesClientTest extends ClientTestBase {

  private static final String EASY_MODEL_NPC_TYPE = "easy_npc:easy_model_entities_npc";
  private static final String EASY_MODEL_CONFIGURATION_SCREEN =
      "EasyModelEntitiesModelConfigurationScreen";

  @Test
  @DisplayName("\"Change Model\" opens the Easy Model Entities profile selection")
  void changeModelOpensProfileSelection() {
    buildTestPlatform();
    client.teleport(0.5, STAND_Y, 0.5);
    UUID easyModelNpc = client.spawnEntity(EASY_MODEL_NPC_TYPE, 0.5, STAND_Y, 3.5, true);
    openMainConfiguration(easyModelNpc);

    assertTrue(
        isEnabledWidget(configurationButton("change_model")),
        "\"Change Model\" is not available for an Easy Model NPC");
    String openedScreenId = clickAndAwaitScreenChange(configurationButton("change_model"));
    client.saveScreenshot("easy_model_profile_selection");

    assertScreenOfClass(openedScreenId, EASY_MODEL_CONFIGURATION_SCREEN);
  }
}
