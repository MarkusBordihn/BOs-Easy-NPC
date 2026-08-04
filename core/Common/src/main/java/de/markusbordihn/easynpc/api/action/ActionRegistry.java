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

package de.markusbordihn.easynpc.api.action;

import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ActionRegistry {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Map<ResourceLocation, CustomActionExecutor> customActionExecutors =
      new ConcurrentHashMap<>();

  private ActionRegistry() {}

  public static void register(
      ResourceLocation actionId, CustomActionExecutor customActionExecutor) {
    if (actionId == null || customActionExecutor == null) {
      log.error("Unable to register action {} with executor {}", actionId, customActionExecutor);
      return;
    }

    CustomActionExecutor previousExecutor =
        customActionExecutors.putIfAbsent(actionId, customActionExecutor);
    if (previousExecutor != null) {
      log.error("Action {} is already registered by {}", actionId, previousExecutor);
      return;
    }

    log.info("Registered custom action {}", actionId);
  }

  public static CustomActionExecutor get(ResourceLocation actionId) {
    return actionId != null ? customActionExecutors.get(actionId) : null;
  }

  public static boolean isRegistered(ResourceLocation actionId) {
    return get(actionId) != null;
  }
}
