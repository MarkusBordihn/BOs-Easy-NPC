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

package de.markusbordihn.easynpc.entity.data;

import de.markusbordihn.easynpc.data.action.ActionData;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.action.ActionType;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.data.custom.CustomDataIndex;
import de.markusbordihn.easynpc.data.entity.CustomDataSerializers;
import de.markusbordihn.easynpc.data.entity.CustomEntityData;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.syncher.EntityDataSerializers;

public interface EntityActionEventData extends EntityDataInterface {

  // Synced entity data

  // Custom entity data
  CustomDataAccessor<ActionEventSet> CUSTOM_DATA_ACTION_EVENT_SET =
      CustomEntityData.defineId(
          CustomDataIndex.ACTION_EVENT_SET, CustomDataSerializers.ACTION_EVENT_SET);
  CustomDataAccessor<Integer> CUSTOM_DATA_ACTION_PERMISSION_LEVEL =
      CustomEntityData.defineId(EntityDataSerializers.INT);

  // CompoundTags
  String DATA_ACTIONS_TAG = "Actions";
  String DATA_ACTION_DATA_TAG = "ActionData";
  String DATA_ACTION_PERMISSION_LEVEL_TAG = "ActionPermissionLevel";

  // Legacy CompoundTags
  String DATA_ACTION_ENABLE_DEBUG_TAG = "ActionEnableDebug";
  String DATA_ACTION_EXECUTE_AS_USER_TAG = "ActionExecuteAsUser";
  String DATA_ACTION_TAG = "Action";
  String DATA_ACTION_TYPE_TAG = "ActionType";

  // Legancy Action Types
  String ON_YES_SELECTION = "ON_YES_SELECTION";
  String ON_NO_SELECTION = "ON_NO_SELECTION";

  default ActionEventSet getActionEventSet() {
    return getCustomEntityData(CUSTOM_DATA_ACTION_EVENT_SET);
  }

  default void setActionEventSet(ActionEventSet actions) {
    setCustomEntityData(CUSTOM_DATA_ACTION_EVENT_SET, actions);
  }

  default boolean hasActionEvent(ActionEventType actionEventType) {
    return actionEventType != null
        && getActionEventSet() != null
        && getActionEventSet().hasActionEvent(actionEventType);
  }

  default ActionData getActionEvent(ActionEventType actionEventType) {
    return hasActionEvent(actionEventType)
        ? getActionEventSet().getActionEvent(actionEventType)
        : null;
  }

  default void clearActionEventSet() {
    setCustomEntityData(CUSTOM_DATA_ACTION_EVENT_SET, new ActionEventSet());
  }

  default int getActionPermissionLevel() {
    return getCustomEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL);
  }

  default void setActionPermissionLevel(int actionPermissionLevel) {
    setCustomEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL, actionPermissionLevel);
  }

  default void defineSynchedActionData() {
  }

  default void defineCustomActionData() {
    defineCustomEntityData(CUSTOM_DATA_ACTION_EVENT_SET, new ActionEventSet());
    defineCustomEntityData(CUSTOM_DATA_ACTION_PERMISSION_LEVEL, 0);
  }

  default void addAdditionalActionData(CompoundTag compoundTag) {
    CompoundTag actionDataTag = new CompoundTag();

    // Store action data
    getActionEventSet().save(actionDataTag);

    // Store permission level
    actionDataTag.putInt(DATA_ACTION_PERMISSION_LEVEL_TAG, this.getActionPermissionLevel());

    compoundTag.put(DATA_ACTION_DATA_TAG, actionDataTag);
  }

  default void readAdditionalActionData(CompoundTag compoundTag) {

    // Legacy data support
    if (readAdditionalLegacyActionData(compoundTag)) {
      return;
    }

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

  default boolean readAdditionalLegacyActionData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_ACTION_PERMISSION_LEVEL_TAG)) {
      log.info("Converting legacy action data to new multi-format for {}", this);

      // Store permission level
      this.setActionPermissionLevel(compoundTag.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG));

      // Convert legacy action data
      if (compoundTag.contains(DATA_ACTION_DATA_TAG)) {
        CompoundTag actionDataTag = compoundTag.getCompound(DATA_ACTION_DATA_TAG);
        if (actionDataTag.contains(DATA_ACTIONS_TAG)) {
          ListTag listTag = actionDataTag.getList(DATA_ACTIONS_TAG, 10);
          for (int i = 0; i < listTag.size(); ++i) {
            CompoundTag actionTag = listTag.getCompound(i);
            ActionEventType actionEventType =
                ActionEventType.get(actionTag.getString(DATA_ACTION_TYPE_TAG));
            boolean executeAsUser = actionTag.getBoolean(DATA_ACTION_EXECUTE_AS_USER_TAG);
            boolean enableDebug = actionTag.getBoolean(DATA_ACTION_ENABLE_DEBUG_TAG);
            int permissionLevel = actionTag.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG);
            String action = actionTag.getString(DATA_ACTION_TAG);
            if (actionEventType != null
                && actionEventType != ActionEventType.NONE
                && action != null
                && !action.isEmpty()) {
              ActionData actionData =
                  new ActionData(
                      ActionType.COMMAND, action, permissionLevel, executeAsUser, enableDebug);
              this.getActionEventSet().setActionEvent(actionEventType, actionData);
            }
          }
        }
      }
      return true;
    }
    return false;
  }
}
