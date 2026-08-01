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

package de.markusbordihn.easynpc.api.event;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEventRegistry {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final List<DialogEventListener> dialogEventListeners =
      new CopyOnWriteArrayList<>();
  private static final List<ActionEventListener> actionEventListeners =
      new CopyOnWriteArrayList<>();
  private static final List<StateEventListener> stateEventListeners = new CopyOnWriteArrayList<>();

  private EasyNPCEventRegistry() {}

  public static void registerDialogEventListener(DialogEventListener dialogEventListener) {
    if (dialogEventListener == null) {
      log.error("Unable to register a null dialog event listener");
      return;
    }

    dialogEventListeners.add(dialogEventListener);
  }

  public static void unregisterDialogEventListener(DialogEventListener dialogEventListener) {
    dialogEventListeners.remove(dialogEventListener);
  }

  public static void registerActionEventListener(ActionEventListener actionEventListener) {
    if (actionEventListener == null) {
      log.error("Unable to register a null action event listener");
      return;
    }

    actionEventListeners.add(actionEventListener);
  }

  public static void unregisterActionEventListener(ActionEventListener actionEventListener) {
    actionEventListeners.remove(actionEventListener);
  }

  public static void registerStateEventListener(StateEventListener stateEventListener) {
    if (stateEventListener == null) {
      log.error("Unable to register a null state event listener");
      return;
    }

    stateEventListeners.add(stateEventListener);
  }

  public static void unregisterStateEventListener(StateEventListener stateEventListener) {
    stateEventListeners.remove(stateEventListener);
  }

  public static void fireStateChanged(
      EasyNPC<?> easyNPC,
      Identifier stateId,
      StateEntry previousStateEntry,
      StateEntry currentStateEntry) {
    if (stateEventListeners.isEmpty() || easyNPC == null || stateId == null) {
      return;
    }

    for (StateEventListener stateEventListener : stateEventListeners) {
      try {
        stateEventListener.onStateChanged(easyNPC, stateId, previousStateEntry, currentStateEntry);
      } catch (Exception e) {
        log.error("State event listener {} failed for {}", stateEventListener, easyNPC, e);
      }
    }
  }

  public static void fireDialogOpened(
      EasyNPC<?> easyNPC, ServerPlayer serverPlayer, DialogDataEntry dialogDataEntry) {
    if (dialogEventListeners.isEmpty() || easyNPC == null || serverPlayer == null) {
      return;
    }

    for (DialogEventListener dialogEventListener : dialogEventListeners) {
      try {
        dialogEventListener.onDialogOpened(easyNPC, serverPlayer, dialogDataEntry);
      } catch (Exception e) {
        log.error("Dialog event listener {} failed for {}", dialogEventListener, easyNPC, e);
      }
    }
  }

  public static void fireActionExecuted(
      EasyNPC<?> easyNPC, ServerPlayer serverPlayer, ActionDataEntry actionDataEntry) {
    if (actionEventListeners.isEmpty() || easyNPC == null) {
      return;
    }

    for (ActionEventListener actionEventListener : actionEventListeners) {
      try {
        actionEventListener.onActionExecuted(easyNPC, serverPlayer, actionDataEntry);
      } catch (Exception e) {
        log.error("Action event listener {} failed for {}", actionEventListener, easyNPC, e);
      }
    }
  }
}
