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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class IntegrationRegistry {

  private static final Map<String, IntegrationModelProvider> modelProviders = new HashMap<>();
  private static boolean guiPreviewMode = false;

  private IntegrationRegistry() {}

  public static void register(IntegrationModelProvider provider) {
    if (provider != null) {
      modelProviders.put(provider.getIntegrationId(), provider);
    }
  }

  public static List<String> getModels(String integrationId) {
    IntegrationModelProvider provider = modelProviders.get(integrationId);
    if (provider == null) {
      return Collections.emptyList();
    }

    return provider.getAvailableModels();
  }

  public static boolean hasModels(String integrationId) {
    IntegrationModelProvider provider = modelProviders.get(integrationId);
    return provider != null && !provider.getAvailableModels().isEmpty();
  }

  public static List<String> getAllModels() {
    List<String> allModels = new ArrayList<>();
    for (IntegrationModelProvider provider : modelProviders.values()) {
      allModels.addAll(provider.getAvailableModels());
    }
    return allModels;
  }

  public static boolean isGuiPreviewMode() {
    return guiPreviewMode;
  }

  public static void setGuiPreviewMode(boolean enabled) {
    guiPreviewMode = enabled;
  }
}
