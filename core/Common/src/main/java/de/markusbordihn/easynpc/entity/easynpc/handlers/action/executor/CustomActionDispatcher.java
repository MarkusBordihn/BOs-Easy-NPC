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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.action.ActionRegistry;
import de.markusbordihn.easynpc.api.action.CustomActionExecutor;
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.CustomActionCommand;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomActionDispatcher {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Set<ResourceLocation> reportedMissingActions = ConcurrentHashMap.newKeySet();

  private CustomActionDispatcher() {}

  public static void execute(
      ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC, ActionContext actionContext) {
    CustomActionCommand customActionCommand = CustomActionCommand.parse(actionDataEntry.command());
    if (!customActionCommand.isValid()) {
      log.warn("Ignoring custom action without a valid action id: {}", actionDataEntry);
      return;
    }

    CustomActionExecutor customActionExecutor = ActionRegistry.get(customActionCommand.actionId());
    if (customActionExecutor == null) {
      if (reportedMissingActions.add(customActionCommand.actionId())) {
        log.warn(
            "Custom action {} is not registered, every action using it is skipped.",
            customActionCommand.actionId());
      }
      return;
    }

    try {
      customActionExecutor.execute(
          actionDataEntry, easyNPC, customActionCommand.arguments(), actionContext);
    } catch (Exception e) {
      log.error("Error executing custom action {}", customActionCommand.actionId(), e);
    }
  }
}
