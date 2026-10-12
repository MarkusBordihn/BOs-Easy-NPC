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
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCEventRegistry {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final List<DialogEventListener> dialogEventListeners =
      new CopyOnWriteArrayList<>();
  private static final List<ActionEventListener> actionEventListeners =
      new CopyOnWriteArrayList<>();
  private static final List<StateEventListener> stateEventListeners = new CopyOnWriteArrayList<>();
  private static final List<EntityLifecycleListener> entityLifecycleListeners =
      new CopyOnWriteArrayList<>();
  private static final List<OwnerEventListener> ownerEventListeners = new CopyOnWriteArrayList<>();
  private static final List<ObjectiveEventListener> objectiveEventListeners =
      new CopyOnWriteArrayList<>();
  private static final List<ShoulderEventListener> shoulderEventListeners =
      new CopyOnWriteArrayList<>();

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

  public static void registerEntityLifecycleListener(
      EntityLifecycleListener entityLifecycleListener) {
    if (entityLifecycleListener == null) {
      log.error("Unable to register a null entity lifecycle listener");
      return;
    }

    entityLifecycleListeners.add(entityLifecycleListener);
  }

  public static void unregisterEntityLifecycleListener(
      EntityLifecycleListener entityLifecycleListener) {
    entityLifecycleListeners.remove(entityLifecycleListener);
  }

  public static void registerOwnerEventListener(OwnerEventListener ownerEventListener) {
    if (ownerEventListener == null) {
      log.error("Unable to register a null owner event listener");
      return;
    }

    ownerEventListeners.add(ownerEventListener);
  }

  public static void unregisterOwnerEventListener(OwnerEventListener ownerEventListener) {
    ownerEventListeners.remove(ownerEventListener);
  }

  public static void registerObjectiveEventListener(ObjectiveEventListener objectiveEventListener) {
    if (objectiveEventListener == null) {
      log.error("Unable to register a null objective event listener");
      return;
    }

    objectiveEventListeners.add(objectiveEventListener);
  }

  public static void unregisterObjectiveEventListener(
      ObjectiveEventListener objectiveEventListener) {
    objectiveEventListeners.remove(objectiveEventListener);
  }

  public static void registerShoulderEventListener(ShoulderEventListener shoulderEventListener) {
    if (shoulderEventListener == null) {
      log.error("Unable to register a null shoulder event listener");
      return;
    }

    shoulderEventListeners.add(shoulderEventListener);
  }

  public static void unregisterShoulderEventListener(ShoulderEventListener shoulderEventListener) {
    shoulderEventListeners.remove(shoulderEventListener);
  }

  public static void fireOwnerChanged(EasyNPC<?> easyNPC, UUID previousOwnerUUID, UUID ownerUUID) {
    if (ownerEventListeners.isEmpty() || easyNPC == null) {
      return;
    }

    for (OwnerEventListener ownerEventListener : ownerEventListeners) {
      try {
        ownerEventListener.onOwnerChanged(easyNPC, previousOwnerUUID, ownerUUID);
      } catch (Exception e) {
        log.error("Owner event listener {} failed for {}", ownerEventListener, easyNPC, e);
      }
    }
  }

  public static void fireObjectiveAdded(EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveEventListeners.isEmpty() || easyNPC == null || objectiveDataEntry == null) {
      return;
    }

    for (ObjectiveEventListener objectiveEventListener : objectiveEventListeners) {
      try {
        objectiveEventListener.onObjectiveAdded(easyNPC, objectiveDataEntry);
      } catch (Exception e) {
        log.error("Objective event listener {} failed for {}", objectiveEventListener, easyNPC, e);
      }
    }
  }

  public static void fireObjectiveRemoved(
      EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveEventListeners.isEmpty() || easyNPC == null || objectiveDataEntry == null) {
      return;
    }

    for (ObjectiveEventListener objectiveEventListener : objectiveEventListeners) {
      try {
        objectiveEventListener.onObjectiveRemoved(easyNPC, objectiveDataEntry);
      } catch (Exception e) {
        log.error("Objective event listener {} failed for {}", objectiveEventListener, easyNPC, e);
      }
    }
  }

  public static void fireNPCSpawned(EasyNPC<?> easyNPC) {
    if (entityLifecycleListeners.isEmpty() || easyNPC == null) {
      return;
    }

    for (EntityLifecycleListener entityLifecycleListener : entityLifecycleListeners) {
      try {
        entityLifecycleListener.onNPCSpawned(easyNPC);
      } catch (Exception e) {
        log.error(
            "Entity lifecycle listener {} failed for {}", entityLifecycleListener, easyNPC, e);
      }
    }
  }

  public static void fireNPCRemoved(EasyNPC<?> easyNPC, NPCRemovalReason removalReason) {
    if (entityLifecycleListeners.isEmpty() || easyNPC == null) {
      return;
    }

    for (EntityLifecycleListener entityLifecycleListener : entityLifecycleListeners) {
      try {
        entityLifecycleListener.onNPCRemoved(easyNPC, removalReason);
      } catch (Exception e) {
        log.error(
            "Entity lifecycle listener {} failed for {}", entityLifecycleListener, easyNPC, e);
      }
    }
  }

  public static void fireStateChanged(
      EasyNPC<?> easyNPC,
      ResourceLocation stateId,
      StateEntry previousStateEntry,
      StateEntry currentStateEntry) {
    fireStateChanged(easyNPC, stateId, previousStateEntry, currentStateEntry, ActionContext.EMPTY);
  }

  public static void fireStateChanged(
      EasyNPC<?> easyNPC,
      ResourceLocation stateId,
      StateEntry previousStateEntry,
      StateEntry currentStateEntry,
      ActionContext actionContext) {
    if (stateEventListeners.isEmpty() || easyNPC == null || stateId == null) {
      return;
    }

    for (StateEventListener stateEventListener : stateEventListeners) {
      try {
        stateEventListener.onStateChanged(
            easyNPC, stateId, previousStateEntry, currentStateEntry, actionContext);
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
    fireActionExecuted(easyNPC, actionDataEntry, ActionContext.of(serverPlayer));
  }

  public static void fireActionExecuted(
      EasyNPC<?> easyNPC, ActionDataEntry actionDataEntry, ActionContext actionContext) {
    if (actionEventListeners.isEmpty() || easyNPC == null) {
      return;
    }

    for (ActionEventListener actionEventListener : actionEventListeners) {
      try {
        actionEventListener.onActionExecuted(easyNPC, actionDataEntry, actionContext);
      } catch (Exception e) {
        log.error("Action event listener {} failed for {}", actionEventListener, easyNPC, e);
      }
    }
  }

  public static void fireShoulderMounted(
      EasyNPC<?> easyNPC, Player player, ShoulderSide shoulderSide) {
    if (shoulderEventListeners.isEmpty() || easyNPC == null || player == null) {
      return;
    }

    for (ShoulderEventListener shoulderEventListener : shoulderEventListeners) {
      try {
        shoulderEventListener.onShoulderMounted(easyNPC, player, shoulderSide);
      } catch (Exception e) {
        log.error("Shoulder event listener {} failed for {}", shoulderEventListener, easyNPC, e);
      }
    }
  }

  public static void fireShoulderDismounted(EasyNPC<?> easyNPC) {
    if (shoulderEventListeners.isEmpty() || easyNPC == null) {
      return;
    }

    for (ShoulderEventListener shoulderEventListener : shoulderEventListeners) {
      try {
        shoulderEventListener.onShoulderDismounted(easyNPC);
      } catch (Exception e) {
        log.error("Shoulder event listener {} failed for {}", shoulderEventListener, easyNPC, e);
      }
    }
  }
}
