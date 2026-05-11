/*
 * Copyright 2025 Markus Bordihn
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
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class IntegrationRegistry {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<String, List<ResourceLocation>> registeredModels = new HashMap<>();
  private static boolean guiPreviewMode = false;

  private IntegrationRegistry() {}

  public static boolean isGuiPreviewMode() {
    return guiPreviewMode;
  }

  public static void setGuiPreviewMode(boolean preview) {
    guiPreviewMode = preview;
  }

  public static void register(IntegrationModelProvider provider) {
    String id = provider.getIntegrationId();
    List<ResourceLocation> models = provider.getAvailableModels();
    registeredModels.put(id, models);
    log.info("Registered {} models for integration '{}'.", models.size(), id);
  }

  public static List<ResourceLocation> getModels(String integrationId) {
    return registeredModels.getOrDefault(integrationId, Collections.emptyList());
  }

  public static boolean hasModels(String integrationId) {
    return !registeredModels.getOrDefault(integrationId, Collections.emptyList()).isEmpty();
  }
}
