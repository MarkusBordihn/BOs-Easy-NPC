/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.compat;

import de.markusbordihn.easynpc.Constants;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class IntegrationRegistry {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<String, IntegrationModelProvider> modelProviders = new HashMap<>();
  private static final Map<String, List<String>> modelCache = new HashMap<>();
  private static boolean guiPreviewMode = false;

  private IntegrationRegistry() {}

  public static void register(IntegrationModelProvider provider) {
    if (provider != null) {
      String id = provider.getIntegrationId();
      modelProviders.put(id, provider);
      List<String> models = provider.getAvailableModels();
      modelCache.put(id, models);
      log.info("Registered integration model provider '{}' with {} models.", id, models.size());
    }
  }

  public static List<String> getModels(String integrationId) {
    List<String> cached = modelCache.getOrDefault(integrationId, Collections.emptyList());
    if (cached.isEmpty()) {
      IntegrationModelProvider provider = modelProviders.get(integrationId);
      if (provider != null) {
        List<String> fresh = provider.getAvailableModels();
        if (!fresh.isEmpty()) {
          modelCache.put(integrationId, fresh);
          log.debug("Late-loaded {} models for integration '{}'.", fresh.size(), integrationId);
          return fresh;
        }
      }
    }
    return cached;
  }

  public static boolean hasModels(String integrationId) {
    return !getModels(integrationId).isEmpty();
  }

  public static boolean isGuiPreviewMode() {
    return guiPreviewMode;
  }

  public static void setGuiPreviewMode(boolean enabled) {
    guiPreviewMode = enabled;
  }
}
