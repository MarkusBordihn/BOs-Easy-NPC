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
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Mob;

public interface ActionEventDataCapable<E extends Mob> extends EasyNPC<E> {

  ServerDataAccessor<ActionEventSet> CUSTOM_DATA_ACTION_EVENT_SET =
      ServerEntityData.defineId(
          ServerDataIndex.ACTION_EVENT_SET, EntityDataSerializersManager.ACTION_EVENT_SET);
  ServerDataAccessor<Integer> CUSTOM_DATA_ACTION_PERMISSION_LEVEL =
      ServerEntityData.defineId(ServerDataIndex.ACTION_PERMISSION_LEVEL, EntityDataSerializers.INT);

  String DATA_ACTION_DATA_TAG = "ActionData";
  String DATA_ACTION_PERMISSION_LEVEL_TAG = "ActionPermissionLevel";

  default ActionEventSet getActionEventSet() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET);
  }

  default void setActionEventSet(ActionEventSet actions) {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET, actions);
  }

  default boolean hasActionEvent(ActionEventType actionEventType) {
    return actionEventType != null
        && getActionEventSet() != null
        && getActionEventSet().hasActionEvent(actionEventType);
  }

  default ActionDataSet getActionDataSet(ActionEventType actionEventType) {
    return hasActionEvent(actionEventType)
        ? getActionEventSet().getActionEvents(actionEventType)
        : null;
  }

  default boolean hasActionEventSet() {
    return getActionEventSet() != null;
  }

  default void clearActionEventSet() {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET, new ActionEventSet());
  }

  default int getActionPermissionLevel() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL);
  }

  default void setActionPermissionLevel(int actionPermissionLevel) {
    CommandPermissionLevel commandPermissionLevel =
        CommandPermissionLevel.fromMinecraftLevel(actionPermissionLevel);
    getEasyNPCServerData()
        .setServerEntityData(
            CUSTOM_DATA_ACTION_PERMISSION_LEVEL, commandPermissionLevel.minecraftLevel());
  }

  default CommandPermissionLevel getActionCommandPermissionLevel() {
    return CommandPermissionLevel.fromMinecraftLevel(this.getActionPermissionLevel());
  }

  default void setActionCommandPermissionLevel(CommandPermissionLevel commandPermissionLevel) {
    getEasyNPCServerData()
        .setServerEntityData(
            CUSTOM_DATA_ACTION_PERMISSION_LEVEL,
            (commandPermissionLevel != null ? commandPermissionLevel : CommandPermissionLevel.ALL)
                .minecraftLevel());
  }

  default void defineSynchedActionData() {}

  default void defineCustomActionData() {
    getEasyNPCServerData()
        .defineServerEntityData(CUSTOM_DATA_ACTION_EVENT_SET, new ActionEventSet());
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL, 0);
  }

  default void addAdditionalActionData(CompoundTag compoundTag) {
    CompoundTag actionDataTag = new CompoundTag();

    if (this.isServerSideInstance()) {
      ActionEventSet actionEventSet = this.getActionEventSet();
      if (this.hasSavableActionEventData(actionEventSet)) {
        actionEventSet.save(actionDataTag);
      }
      if (this.getActionPermissionLevel() != 0) {
        actionDataTag.putInt(DATA_ACTION_PERMISSION_LEVEL_TAG, this.getActionPermissionLevel());
      }
    }

    CompoundTagUtils.putIfNotEmpty(compoundTag, DATA_ACTION_DATA_TAG, actionDataTag);
  }

  default boolean hasSavableActionEventData(ActionEventSet actionEventSet) {
    if (actionEventSet == null) {
      return false;
    }

    for (ActionEventType actionEventType : ActionEventType.values()) {
      if (actionEventSet.hasActionEvent(actionEventType)) {
        return true;
      }
    }

    return false;
  }

  default void readAdditionalActionData(CompoundTag compoundTag) {

    // Early exit if no action data is available
    if (!compoundTag.contains(DATA_ACTION_DATA_TAG)) {
      return;
    }

    CompoundTag actionDataTag = compoundTag.getCompound(DATA_ACTION_DATA_TAG);

    if (actionDataTag.contains(ActionEventSet.DATA_ACTION_EVENT_SET_TAG)) {
      ActionEventSet actionDataSet = new ActionEventSet(actionDataTag);
      this.setActionEventSet(actionDataSet);
    }

    if (actionDataTag.contains(DATA_ACTION_PERMISSION_LEVEL_TAG)) {
      this.setActionPermissionLevel(actionDataTag.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG));
    }
  }

  default void registerDefaultActionInteractionEvents() {
    log.debug("Register default action interaction events for {} ...", this);

    // Get existing action event set or create a new one
    ActionEventSet actionEventSet =
        this.hasActionEventSet() ? this.getActionEventSet() : new ActionEventSet();
    ActionDataSet actionDataSet =
        actionEventSet.hasActionEvent(ActionEventType.ON_INTERACTION)
            ? actionEventSet.getActionEvents(ActionEventType.ON_INTERACTION)
            : new ActionDataSet();

    ActionDataEntry actionDataEntryOpenDialog =
        new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG);
    actionDataSet.add(actionDataEntryOpenDialog);

    ActionDataEntry actionDataEntryOpenTradingScreen =
        new ActionDataEntry(ActionDataType.OPEN_TRADING_SCREEN);
    actionDataSet.add(actionDataEntryOpenTradingScreen);

    actionEventSet.setActionEvent(ActionEventType.ON_INTERACTION, actionDataSet);
    this.setActionEventSet(actionEventSet);
  }

  default void handleActionEvent(ActionEventType actionEventType) {
    if (!this.hasActionEvent(actionEventType)) {
      return;
    }

    ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
    if (actionHandler != null) {
      actionHandler.executeActions(this.getActionDataSet(actionEventType), null);
    }
  }

  default void handleActionEvent(ActionEventType actionEventType, ServerPlayer serverPlayer) {
    if (!this.hasActionEvent(actionEventType)) {
      return;
    }

    ActionHandler<E> actionHandler = this.getEasyNPCActionHandler();
    if (actionHandler != null) {
      actionHandler.executeActions(this.getActionDataSet(actionEventType), serverPlayer);
    }
  }
}
