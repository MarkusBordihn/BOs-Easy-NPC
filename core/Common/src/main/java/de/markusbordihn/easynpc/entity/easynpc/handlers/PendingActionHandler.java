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

package de.markusbordihn.easynpc.entity.easynpc.handlers;

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.PendingActionChain;
import de.markusbordihn.easynpc.data.action.PendingActionSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import java.util.List;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;

public interface PendingActionHandler<E extends Mob> extends EasyNPC<E> {

  default boolean hasPendingAction(ActionEventType actionEventType, ResourceLocation sourceId) {
    PendingActionSet pendingActionSet = this.getPendingActions();
    return pendingActionSet != null && pendingActionSet.has(actionEventType, sourceId);
  }

  default void schedulePendingAction(PendingActionChain pendingActionChain) {
    PendingActionSet pendingActionSet = this.getPendingActions();
    if (pendingActionSet != null) {
      pendingActionSet.schedule(pendingActionChain);
    }
  }

  default void cancelPendingAction(ActionEventType actionEventType, ResourceLocation sourceId) {
    PendingActionSet pendingActionSet = this.getPendingActions();
    if (pendingActionSet != null) {
      pendingActionSet.cancel(actionEventType, sourceId);
    }
  }

  default boolean resumePendingActionEarly(
      ActionEventType actionEventType, ResourceLocation sourceId) {
    PendingActionSet pendingActionSet = this.getPendingActions();
    PendingActionChain pendingActionChain =
        pendingActionSet != null ? pendingActionSet.remove(actionEventType, sourceId) : null;
    if (pendingActionChain == null) {
      return false;
    }

    this.resumePendingAction(pendingActionChain);
    return true;
  }

  default void cancelPendingActions() {
    PendingActionSet pendingActionSet = this.getPendingActions();
    if (pendingActionSet != null) {
      pendingActionSet.clear();
    }
  }

  default void tickPendingActions() {
    PendingActionSet pendingActionSet = this.getPendingActions();
    if (pendingActionSet == null || pendingActionSet.isEmpty()) {
      return;
    }

    for (PendingActionChain pendingActionChain : pendingActionSet.tickAndRemoveDueChains()) {
      this.resumePendingAction(pendingActionChain);
    }
  }

  default void resumePendingAction(PendingActionChain pendingActionChain) {
    ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
    if (actionHandler == null) {
      return;
    }

    List<ServerPlayer> audience =
        actionHandler.getServerPlayersInRange(ActionHandler.INTERVAL_ACTION_RANGE);
    actionHandler.executeActionSequence(
        pendingActionChain.remainingActions(),
        pendingActionChain.fallbackActions(),
        ActionContext.of(
                pendingActionChain.actionEventType(),
                this.getPendingActionInitiator(pendingActionChain),
                audience)
            .withSourceId(pendingActionChain.sourceId()),
        pendingActionChain.executionState());
  }

  private PendingActionSet getPendingActions() {
    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    return actionEventData != null ? actionEventData.getPendingActionSet() : null;
  }

  private ServerPlayer getPendingActionInitiator(PendingActionChain pendingActionChain) {
    if (pendingActionChain.initiatorUUID() == null || this.getEntityServerLevel() == null) {
      return null;
    }

    Player player = this.getEntityServerLevel().getPlayerByUUID(pendingActionChain.initiatorUUID());
    return player instanceof ServerPlayer serverPlayer ? serverPlayer : null;
  }
}
