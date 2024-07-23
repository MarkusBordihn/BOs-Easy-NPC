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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.PathfinderMob;

public interface ActionEventData<E extends PathfinderMob> extends EasyNPC<E> {

  ServerDataAccessor<ActionEventSet> CUSTOM_DATA_ACTION_EVENT_SET =
      ServerEntityData.defineId(
          ServerDataIndex.ACTION_EVENT_SET, EntityDataSerializersManager.ACTION_EVENT_SET);
  ServerDataAccessor<Integer> CUSTOM_DATA_ACTION_PERMISSION_LEVEL =
      ServerEntityData.defineId(EntityDataSerializers.INT);

  String DATA_ACTION_DATA_TAG = "ActionData";
  String DATA_ACTION_PERMISSION_LEVEL_TAG = "ActionPermissionLevel";

  default ActionEventSet getActionEventSet() {
    return getServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET);
  }

  default void setActionEventSet(ActionEventSet actions) {
    setServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET, actions);
  }

  default boolean hasActionEvent(ActionEventType actionEventType) {
    return actionEventType != null
        && getActionEventSet() != null
        && getActionEventSet().hasActionEvent(actionEventType);
  }

  default ActionDataEntry getActionEvent(ActionEventType actionEventType) {
    return hasActionEvent(actionEventType)
        ? getActionEventSet().getActionEvent(actionEventType)
        : null;
  }

  default ActionDataSet getActionDataSet(ActionEventType actionEventType) {
    return hasActionEvent(actionEventType)
        ? getActionEventSet().getActionEvents(actionEventType)
        : null;
  }

  default void clearActionEventSet() {
    setServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET, new ActionEventSet());
  }

  default int getActionPermissionLevel() {
    return getServerEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL);
  }

  default void setActionPermissionLevel(int actionPermissionLevel) {
    setServerEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL, actionPermissionLevel);
  }

  default void defineSynchedActionData(SynchedEntityData.Builder builder) {}

  default void defineCustomActionData() {
    defineServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET, new ActionEventSet());
    defineServerEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL, 0);
  }

  default void addAdditionalActionData(CompoundTag compoundTag) {
    CompoundTag actionDataTag = new CompoundTag();

    if (this.isServerSide()) {
      ActionEventSet actionEventSet = this.getActionEventSet();
      if (actionEventSet != null) {
        actionEventSet.save(actionDataTag);
      }
      actionDataTag.putInt(DATA_ACTION_PERMISSION_LEVEL_TAG, this.getActionPermissionLevel());
    }

    compoundTag.put(DATA_ACTION_DATA_TAG, actionDataTag);
  }

  default void readAdditionalActionData(CompoundTag compoundTag) {

    // Early exit if no action data is available
    if (!compoundTag.contains(DATA_ACTION_DATA_TAG)) {
      return;
    }

    // Read action data
    CompoundTag actionDataTag = compoundTag.getCompound(DATA_ACTION_DATA_TAG);

    // Read actions
    if (actionDataTag.contains(ActionEventSet.DATA_ACTION_EVENT_SET_TAG)) {
      ActionEventSet actionDataSet = new ActionEventSet(actionDataTag);
      this.setActionEventSet(actionDataSet);
    }

    // Read permission level
    if (actionDataTag.contains(DATA_ACTION_PERMISSION_LEVEL_TAG)) {
      this.setActionPermissionLevel(actionDataTag.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG));
    }
  }

  default void handleActionInteractionEvent(ServerPlayer serverPlayer) {
    if (this.hasActionEvent(ActionEventType.ON_INTERACTION)) {
      ActionDataSet actionDataSet = this.getActionDataSet(ActionEventType.ON_INTERACTION);
      ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
      if (actionDataSet != null && actionHandler != null) {
        actionHandler.executeActions(actionDataSet, serverPlayer);
      }
    }
  }

  default void handleActionHurtEvent(DamageSource damageSource, float damage) {
    if (this.hasActionEvent(ActionEventType.ON_HURT)) {
      ActionDataEntry actionDataEntry = this.getActionEvent(ActionEventType.ON_HURT);
      ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
      if (actionDataEntry != null
          && actionDataEntry.isValidAndNotEmpty()
          && actionHandler != null) {
        actionHandler.executeAction(actionDataEntry, damageSource);
      }
    }
  }

  default void handleActionDieEvent(DamageSource damageSource) {
    if (this.hasActionEvent(ActionEventType.ON_DEATH)) {
      ActionDataEntry actionDataEntry = this.getActionEvent(ActionEventType.ON_DEATH);
      ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
      if (actionDataEntry != null
          && actionDataEntry.isValidAndNotEmpty()
          && actionHandler != null) {
        actionHandler.executeAction(actionDataEntry, damageSource);
      }
    }
  }
}
