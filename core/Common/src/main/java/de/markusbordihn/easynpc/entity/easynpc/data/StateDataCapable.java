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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.state.StateDataSet;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.Objects;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface StateDataCapable<E extends Mob> extends EasyNPC<E> {
  long STATE_CHANGE_ACTION_INTERVAL_TICKS = BaseTickHandler.BASE_TICK + 1L;

  ServerDataAccessor<StateDataSet> CUSTOM_DATA_STATE_DATA_SET =
      ServerEntityData.defineId(
          ServerDataIndex.NPC_STATE, EntityDataSerializersManager.STATE_DATA_SET);

  String DATA_STATE_TAG = "StateData";

  default StateDataSet getStateDataSet() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_STATE_DATA_SET);
  }

  default void setStateDataSet(StateDataSet stateDataSet) {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_STATE_DATA_SET, stateDataSet);
  }

  default StateEntry getState(Identifier stateId) {
    StateDataSet stateDataSet = this.getStateDataSet();
    return stateDataSet != null ? stateDataSet.get(stateId) : null;
  }

  default int getStateNumber(Identifier stateId) {
    StateEntry stateEntry = this.getState(stateId);
    return stateEntry != null ? stateEntry.asNumber() : 0;
  }

  default String getStateText(Identifier stateId) {
    StateEntry stateEntry = this.getState(stateId);
    return stateEntry != null ? stateEntry.asText() : "";
  }

  default boolean getStateFlag(Identifier stateId) {
    StateEntry stateEntry = this.getState(stateId);
    return stateEntry != null && stateEntry.asFlag();
  }

  default void setState(Identifier stateId, StateEntry stateEntry) {
    this.setState(stateId, stateEntry, ActionContext.EMPTY);
  }

  default void setState(Identifier stateId, StateEntry stateEntry, ActionContext actionContext) {
    StateDataSet stateDataSet = this.getStateDataSet();
    if (stateDataSet == null) {
      return;
    }

    StateEntry previousStateEntry = stateDataSet.get(stateId);
    stateDataSet.set(stateId, stateEntry);
    this.setStateDataSet(stateDataSet);

    StateEntry currentStateEntry = stateDataSet.get(stateId);
    if (Objects.equals(previousStateEntry, currentStateEntry)) {
      return;
    }

    ActionContext stateChangeContext =
        (actionContext != null ? actionContext : ActionContext.EMPTY)
            .withEventType(ActionEventType.ON_STATE_CHANGE)
            .withSourceId(stateId);
    EasyNPCEventRegistry.fireStateChanged(
        this, stateId, previousStateEntry, currentStateEntry, stateChangeContext);

    ActionEventDataCapable<E> actionEventData = this.getEasyNPCActionEventData();
    if (actionEventData == null) {
      return;
    }

    Level entityLevel = this.getEntityLevel();
    long currentTick = entityLevel != null ? entityLevel.getGameTime() : 0L;
    if (!stateDataSet.tryStartActionEvent(currentTick, STATE_CHANGE_ACTION_INTERVAL_TICKS)) {
      return;
    }

    try {
      actionEventData.handleActionEvent(ActionEventType.ON_STATE_CHANGE, stateChangeContext);
    } finally {
      stateDataSet.finishActionEvent();
    }
  }

  default void setState(Identifier stateId, int numberValue) {
    this.setState(stateId, StateEntry.of(numberValue));
  }

  default void setState(Identifier stateId, String textValue) {
    this.setState(stateId, StateEntry.of(textValue));
  }

  default void setState(Identifier stateId, boolean flagValue) {
    this.setState(stateId, StateEntry.of(flagValue));
  }

  default void removeState(Identifier stateId) {
    this.setState(stateId, (StateEntry) null);
  }

  default boolean hasState(Identifier stateId) {
    StateDataSet stateDataSet = this.getStateDataSet();
    return stateDataSet != null && stateDataSet.has(stateId);
  }

  default void clearStateDataSet() {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_STATE_DATA_SET, new StateDataSet());
  }

  default void defineCustomStateData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_STATE_DATA_SET, new StateDataSet());
  }

  default void addAdditionalStateData(ValueOutput valueOutput) {
    if (!this.isServerSideInstance()) {
      return;
    }

    StateDataSet stateDataSet = this.getStateDataSet();
    if (stateDataSet == null || stateDataSet.isEmpty()) {
      return;
    }

    valueOutput.store(DATA_STATE_TAG, CompoundTag.CODEC, stateDataSet.createTag());
  }

  default void readAdditionalStateData(ValueInput valueInput) {
    valueInput
        .read(DATA_STATE_TAG, CompoundTag.CODEC)
        .ifPresent(stateDataTag -> this.setStateDataSet(new StateDataSet(stateDataTag)));
  }
}
