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
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.state.StateDataSet;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.Objects;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Mob;

public interface StateDataCapable<E extends Mob> extends EasyNPC<E> {

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

  default StateEntry getState(ResourceLocation stateId) {
    StateDataSet stateDataSet = this.getStateDataSet();
    return stateDataSet != null ? stateDataSet.get(stateId) : null;
  }

  default int getStateNumber(ResourceLocation stateId) {
    StateEntry stateEntry = this.getState(stateId);
    return stateEntry != null ? stateEntry.asNumber() : 0;
  }

  default String getStateText(ResourceLocation stateId) {
    StateEntry stateEntry = this.getState(stateId);
    return stateEntry != null ? stateEntry.asText() : "";
  }

  default boolean getStateFlag(ResourceLocation stateId) {
    StateEntry stateEntry = this.getState(stateId);
    return stateEntry != null && stateEntry.asFlag();
  }

  default void setState(ResourceLocation stateId, StateEntry stateEntry) {
    StateDataSet stateDataSet = this.getStateDataSet();
    if (stateDataSet == null) {
      return;
    }

    StateEntry previousStateEntry = stateDataSet.get(stateId);
    stateDataSet.set(stateId, stateEntry);
    this.setStateDataSet(stateDataSet);

    StateEntry currentStateEntry = stateDataSet.get(stateId);
    if (!Objects.equals(previousStateEntry, currentStateEntry)) {
      EasyNPCEventRegistry.fireStateChanged(this, stateId, previousStateEntry, currentStateEntry);
    }
  }

  default void setState(ResourceLocation stateId, int numberValue) {
    this.setState(stateId, StateEntry.of(numberValue));
  }

  default void setState(ResourceLocation stateId, String textValue) {
    this.setState(stateId, StateEntry.of(textValue));
  }

  default void setState(ResourceLocation stateId, boolean flagValue) {
    this.setState(stateId, StateEntry.of(flagValue));
  }

  default void removeState(ResourceLocation stateId) {
    this.setState(stateId, (StateEntry) null);
  }

  default boolean hasState(ResourceLocation stateId) {
    StateDataSet stateDataSet = this.getStateDataSet();
    return stateDataSet != null && stateDataSet.has(stateId);
  }

  default void clearStateDataSet() {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_STATE_DATA_SET, new StateDataSet());
  }

  default void defineCustomStateData() {
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_STATE_DATA_SET, new StateDataSet());
  }

  default void addAdditionalStateData(CompoundTag compoundTag) {
    if (!this.isServerSideInstance()) {
      return;
    }

    StateDataSet stateDataSet = this.getStateDataSet();
    if (stateDataSet == null || stateDataSet.isEmpty()) {
      return;
    }

    compoundTag.put(DATA_STATE_TAG, stateDataSet.createTag());
  }

  default void readAdditionalStateData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_STATE_TAG)) {
      return;
    }

    this.setStateDataSet(new StateDataSet(compoundTag.getCompound(DATA_STATE_TAG)));
  }
}
