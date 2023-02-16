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

  public static CompoundTag setAction(CompoundTag compoundTag, ActionType actionType,
      String action) {
    if (actionType != ActionType.NONE) {
      Map<ActionType, String> actions = readActionData(compoundTag);
      actions.put(actionType, action);
      return saveActionData(actions);
    }
    return compoundTag;
  }

  public static String getAction(CompoundTag compoundTag, ActionType actionType) {
    if (actionType != ActionType.NONE) {
      Map<ActionType, String> actions = readActionData(compoundTag);
      return actions.getOrDefault(actionType, "");
    }
    return "";
  }

  public static boolean hasAction(CompoundTag compoundTag, ActionType actionType) {
    if (actionType != ActionType.NONE) {
      Map<ActionType, String> actions = readActionData(compoundTag);
      if (!actions.containsKey(actionType)) {
        return false;
      }
      String action = actions.get(actionType);
      if (action != null && !actions.isEmpty()) {
        return true;
      }
    }
    return false;
  }

  public static CompoundTag saveActionData(Map<ActionType, String> actions) {
    CompoundTag compoundTag = new CompoundTag();
    saveActionData(compoundTag, actions);
    return compoundTag;
  }

  public static CompoundTag saveActionData(CompoundTag compoundTag,
      Map<ActionType, String> actions) {
    if (actions != null && !actions.isEmpty()) {
      ListTag listTag = new ListTag();
      for (Entry<ActionType, String> actionEntry : actions.entrySet()) {
        ActionType actionType = actionEntry.getKey();
        if (actionType != ActionType.NONE) {
          String action = actionEntry.getValue();
          if (action != null && !action.isEmpty()) {
            CompoundTag compoundTagAction = new CompoundTag();
            compoundTagAction.putString(DATA_ACTION_TYPE_TAG, actionType.name());
            compoundTagAction.putString(DATA_ACTION_TAG, action);
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

  public static Map<ActionType, String> readActionData(CompoundTag compoundTag) {
    EnumMap<ActionType, String> actions = new EnumMap<>(ActionType.class);
    if (compoundTag.contains(DATA_ACTIONS_TAG)) {
      ListTag listTag = compoundTag.getList(DATA_ACTIONS_TAG, 10);
      for (int i = 0; i < listTag.size(); ++i) {
        CompoundTag compoundTagAction = listTag.getCompound(i);
        ActionType actionType = ActionType.get(compoundTagAction.getString(DATA_ACTION_TYPE_TAG));
        if (actionType != ActionType.NONE) {
          String action = compoundTagAction.getString(DATA_ACTION_TAG);
          if (action != null && !action.isEmpty()) {
            actions.put(actionType, action);
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
