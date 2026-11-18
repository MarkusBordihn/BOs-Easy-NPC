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

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class BrokenSkinDataScreenClientTest extends ClientTestBase {

  private static final String BROKEN_PLAYER_SKIN_DATA =
      "{SkinData:{Type:\"PLAYER_SKIN\",Name:\"Steve\","
          + "UUID:\"8667ba71-b85a-4004-af54-457a9734eed7\"}}";

  @ParameterizedTest(name = "{1}")
  @CsvSource({
    "edit_skin, player_skin, PlayerSkinConfigurationScreen",
    "edit_skin, url_skin, UrlSkinConfigurationScreen",
    "edit_skin, custom, CustomSkinConfigurationScreen"
  })
  @DisplayName("Skin screens open for an NPC whose stored skin UUID is broken")
  void skinScreenOpensForBrokenSkinUUID(
      String categoryLabel, String tabLabel, String screenClassName) {
    buildTestPlatform();
    client.teleport(0.5, STAND_Y, 0.5);
    UUID npcUUID =
        client.spawnEntity(
            EntitySpawn.of(TEST_NPC_TYPE, 0.5, STAND_Y, 3.5)
                .withNoAi(true)
                .withNbt(BROKEN_PLAYER_SKIN_DATA));
    openMainConfiguration(npcUUID);

    openSubPage(categoryLabel, tabLabel, screenClassName);

    client.saveScreenshot("broken_skin_uuid-" + tabLabel);
  }
}
