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
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.state.StateActionCommand;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class StateActionExecutor {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private StateActionExecutor() {}

  private static StateDataCapable<?> resolveTargetStateData(
      ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return null;
    }

    if (actionDataEntry.targetUUID() == null) {
      StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();
      if (stateData == null) {
        log.warn(
            "Unable to execute state action {}: {} has no state data", actionDataEntry, easyNPC);
      }
      return stateData;
    }

    EasyNPC<?> targetNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(
            actionDataEntry.targetUUID(), easyNPC.getEntityServerLevel());
    if (targetNPC == null) {
      log.warn(
          "Unable to execute state action {}: no NPC with the UUID {} was found",
          actionDataEntry,
          actionDataEntry.targetUUID());
      return null;
    }

    StateDataCapable<?> targetStateData = targetNPC.getEasyNPCStateData();
    if (targetStateData == null) {
      log.warn(
          "Unable to execute state action {}: {} has no state data", actionDataEntry, targetNPC);
    }
    return targetStateData;
  }

  public static void execute(
      ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC, ActionContext actionContext) {
    StateDataCapable<?> stateData = resolveTargetStateData(actionDataEntry, easyNPC);
    if (stateData == null) {
      return;
    }

    StateActionCommand stateActionCommand = StateActionCommand.parse(actionDataEntry.command());
    if (stateActionCommand.operation() == null) {
      log.warn(
          "Unable to execute state action {}: '{}' is not a known operation",
          actionDataEntry,
          actionDataEntry.command());
      return;
    }

    Identifier stateId = StateIdentifier.parse(stateActionCommand.stateName());
    if (stateId == null) {
      log.warn(
          "Unable to execute state action {}: '{}' is not a valid state",
          actionDataEntry,
          stateActionCommand.stateName());
      return;
    }

    if (stateActionCommand.operation().requiresValue() && !stateActionCommand.hasValue()) {
      log.warn(
          "Unable to execute state action {}: {} needs a value",
          actionDataEntry,
          stateActionCommand.operation());
      return;
    }

    StateEntry previousStateEntry = stateData.getState(stateId);
    StateEntry updatedStateEntry = stateActionCommand.apply(previousStateEntry);
    stateData.setState(stateId, updatedStateEntry, actionContext);

    if (actionDataEntry.enableDebug()) {
      log.info(
          "{} state {} of {} from {} to {}",
          stateActionCommand.operation(),
          stateId,
          stateData,
          previousStateEntry,
          updatedStateEntry);
    }
  }
}
