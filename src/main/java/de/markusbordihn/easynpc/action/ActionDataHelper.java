/**
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

package de.markusbordihn.easynpc.action;

import java.util.EnumMap;
import java.util.Map;
import java.util.Map.Entry;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public class ActionDataHelper {

  // Action Data Tags
  public static final String DATA_ACTIONS_TAG = "Actions";
  public static final String DATA_ACTION_TAG = "Action";
  public static final String DATA_ACTION_TYPE_TAG = "ActionType";
  public static final String DATA_ACTION_EXECUTE_AS_USER_TAG = "ActionExecuteAsUser";
  public static final String DATA_ACTION_ENABLE_DEBUG_TAG = "ActionEnableDebug";
  public static final String DATA_ACTION_PERMISSION_LEVEL_TAG = "ActionPermissionLevel";

  public static CompoundTag setAction(CompoundTag compoundTag, ActionType actionType,
      ActionData actionData) {
    if (actionType != ActionType.NONE) {
      Map<ActionType, ActionData> actions = readActionData(compoundTag);
      actions.put(actionType, actionData);
      return saveActionData(actions);
    }
    return compoundTag;
  }

  public static String getAction(CompoundTag compoundTag, ActionType actionType) {
    ActionData actionData = getActionData(compoundTag, actionType);
    if (actionData != null) {
      return actionData.getAction();
    }
    return "";
  }

  public static ActionData getActionData(CompoundTag compoundTag, ActionType actionType) {
    if (actionType != ActionType.NONE) {
      Map<ActionType, ActionData> actions = readActionData(compoundTag);
      return actions.get(actionType);
    }
    return null;
  }

  public static boolean hasAction(CompoundTag compoundTag, ActionType actionType) {
    ActionData actionData = getActionData(compoundTag, actionType);
    if (actionData != null) {
      return actionData.hasAction();
    }
    return false;
  }

  public static CompoundTag saveActionData(Map<ActionType, ActionData> actions) {
    CompoundTag compoundTag = new CompoundTag();
    saveActionData(compoundTag, actions);
    return compoundTag;
  }

  public static CompoundTag saveActionData(CompoundTag compoundTag,
      Map<ActionType, ActionData> actions) {
    if (actions != null && !actions.isEmpty()) {
      ListTag listTag = new ListTag();
      for (Entry<ActionType, ActionData> actionEntry : actions.entrySet()) {
        ActionType actionType = actionEntry.getKey();
        if (actionType != ActionType.NONE) {
          ActionData actionData = actionEntry.getValue();
          if (actionData != null && actionData.hasAction()) {
            CompoundTag compoundTagAction = new CompoundTag();
            compoundTagAction.putBoolean(DATA_ACTION_ENABLE_DEBUG_TAG, actionData.isDebugEnabled());
            compoundTagAction.putBoolean(DATA_ACTION_EXECUTE_AS_USER_TAG,
                actionData.shouldExecuteAsUser());
            compoundTagAction.putInt(DATA_ACTION_PERMISSION_LEVEL_TAG,
                actionData.getPermissionLevel());
            compoundTagAction.putString(DATA_ACTION_TAG, actionData.getAction());
            compoundTagAction.putString(DATA_ACTION_TYPE_TAG, actionData.getActionTypeName());
            listTag.add(compoundTagAction);
          }
        }
      }
      if (!listTag.isEmpty()) {
        compoundTag.put(DATA_ACTIONS_TAG, listTag);
      }
    }
    return compoundTag;
  }

  public static Map<ActionType, ActionData> readActionData(CompoundTag compoundTag) {
    EnumMap<ActionType, ActionData> actions = new EnumMap<>(ActionType.class);
    if (compoundTag.contains(DATA_ACTIONS_TAG)) {
      ListTag listTag = compoundTag.getList(DATA_ACTIONS_TAG, 10);
      for (int i = 0; i < listTag.size(); ++i) {
        CompoundTag compoundTagAction = listTag.getCompound(i);
        ActionType actionType = ActionType.get(compoundTagAction.getString(DATA_ACTION_TYPE_TAG));
        if (actionType != ActionType.NONE) {
          String action = compoundTagAction.getString(DATA_ACTION_TAG);
          int permissionLevel = compoundTagAction.getInt(DATA_ACTION_PERMISSION_LEVEL_TAG);
          boolean executeAsUser = compoundTagAction.getBoolean(DATA_ACTION_EXECUTE_AS_USER_TAG);
          boolean enableDebug = compoundTagAction.getBoolean(DATA_ACTION_ENABLE_DEBUG_TAG);
          if (action != null && !action.isEmpty()) {
            ActionData actionData = new ActionData(actionType, action);
            actionData.setEnableDebug(enableDebug);
            actionData.setExecuteAsUser(executeAsUser);
            actionData.setPermissionLevel(permissionLevel);
            actions.put(actionType, actionData);
          }
        }
      }
    }
    return actions;
  }

  public static boolean hasActionData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_ACTIONS_TAG)) {
      ListTag listTag = compoundTag.getList(DATA_ACTIONS_TAG, 10);
      return !listTag.isEmpty();
    }
    return false;
  }

}
