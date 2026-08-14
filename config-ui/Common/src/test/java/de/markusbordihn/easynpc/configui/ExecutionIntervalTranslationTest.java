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
import de.markusbordihn.easynpc.data.condition.DurationType;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ExecutionIntervalTranslationTest {

  private static final String[] LANGUAGE_FILES = {
    "en_us.json", "de_de.json", "es_mx.json", "ru_ru.json", "zh_cn.json", "zh_tw.json"
  };
  private static final String KEY_PREFIX = "text.easy_npc.config.executionInterval.";

  private static JsonObject readLanguageFile(String languageFile) throws IOException {
    String resource = "/assets/easy_npc_config_ui/lang/" + languageFile;
    try (InputStream inputStream =
        ExecutionIntervalTranslationTest.class.getResourceAsStream(resource)) {
      assertNotNull(inputStream, "Missing language file " + resource);

      return JsonParser.parseReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))
          .getAsJsonObject();
    }
  }

  @Test
  @DisplayName("Every duration type should be translated in every language")
  void testDurationTypeTranslations() throws IOException {
    for (String languageFile : LANGUAGE_FILES) {
      JsonObject translations = readLanguageFile(languageFile);
      for (DurationType durationType : DurationType.values()) {
        String key = KEY_PREFIX + durationType.name().toLowerCase(Locale.ROOT);
        assertTrue(translations.has(key), "Missing " + key + " in " + languageFile);
        assertFalse(
            translations.get(key).getAsString().isBlank(), "Empty " + key + " in " + languageFile);
      }
    }
  }
}
