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

package de.markusbordihn.easynpc.configui;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class SkinErrorMessageTranslationTest {

  private static final String[] SKIN_ERROR_MESSAGES = {
    "error_loading_image",
    "invalid_player_name",
    "invalid_player_uuid",
    "invalid_remote_image",
    "invalid_resource_location",
    "invalid_url",
    "missing_resource_location"
  };
  private static final String KEY_PREFIX = "text.easy_npc.";

  private static JsonObject readLanguageFile(String languageFile) throws IOException {
    String resource = "/assets/easy_npc_config_ui/lang/" + languageFile;
    try (InputStream inputStream =
        SkinErrorMessageTranslationTest.class.getResourceAsStream(resource)) {
      assertNotNull(inputStream, "Missing language file " + resource);

      return JsonParser.parseReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
          .getAsJsonObject();
    }
  }

  @ParameterizedTest(name = "{0}")
  @ValueSource(strings = {"en_us.json", "de_de.json"})
  @DisplayName("Every skin error message should be translated")
  void testSkinErrorMessageTranslations(String languageFile) throws IOException {
    JsonObject translations = readLanguageFile(languageFile);
    for (String skinErrorMessage : SKIN_ERROR_MESSAGES) {
      String key = KEY_PREFIX + skinErrorMessage;
      assertTrue(translations.has(key), "Missing " + key + " in " + languageFile);
      assertFalse(
          translations.get(key).getAsString().isBlank(), "Empty " + key + " in " + languageFile);
    }
  }
}
