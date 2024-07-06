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

package de.markusbordihn.easynpc.data.action;

import java.util.EnumMap;
import java.util.Map.Entry;
import net.minecraft.nbt.CompoundTag;

public class ActionEventSet {

  public static final String DATA_ACTION_EVENT_SET_TAG = "ActionEventSet";
  private final EnumMap<ActionEventType, ActionDataSet> actionsMap =
      new EnumMap<>(ActionEventType.class);
  private boolean hasDistanceActionEvent = false;

  public ActionEventSet() {}

  public ActionEventSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public void setActionEvent(ActionEventType actionEventType, ActionDataSet actionDataSet) {
    if (actionEventType != null
        && actionEventType != ActionEventType.NONE
        && actionDataSet != null) {
      this.actionsMap.put(actionEventType, actionDataSet);
      this.updateHasDistanceAction();
    }
  }

  public ActionDataEntry getActionEvent(ActionEventType actionEventType) {
    if (actionEventType != null && actionEventType != ActionEventType.NONE) {
      ActionDataSet actionDataSet = this.actionsMap.get(actionEventType);
      if (actionDataSet != null && !actionDataSet.isEmpty()) {
        return actionDataSet.getRandomEntry();
      }
    }
    return null;
  }

  public ActionDataSet getActionEvents(ActionEventType actionEventType) {
    if (actionEventType != ActionEventType.NONE && this.actionsMap.containsKey(actionEventType)) {
      return this.actionsMap.get(actionEventType);
    }
    return ActionDataSet.EMPTY;
  }

  public boolean hasActionEvent(ActionEventType actionEventType) {
    if (actionEventType != null && actionEventType != ActionEventType.NONE) {
      ActionDataSet actions = this.actionsMap.get(actionEventType);
      return actions != null && !actions.isEmpty();
    }
    return false;
  }

  public void updateHasDistanceAction() {
    this.hasDistanceActionEvent =
        (this.actionsMap.containsKey(ActionEventType.ON_DISTANCE_NEAR)
                && !this.actionsMap.get(ActionEventType.ON_DISTANCE_NEAR).isEmpty())
            || (this.actionsMap.containsKey(ActionEventType.ON_DISTANCE_CLOSE)
                && !this.actionsMap.get(ActionEventType.ON_DISTANCE_CLOSE).isEmpty())
            || (this.actionsMap.containsKey(ActionEventType.ON_DISTANCE_VERY_CLOSE)
                && !this.actionsMap.get(ActionEventType.ON_DISTANCE_VERY_CLOSE).isEmpty())
            || (this.actionsMap.containsKey(ActionEventType.ON_DISTANCE_TOUCH)
                && !this.actionsMap.get(ActionEventType.ON_DISTANCE_TOUCH).isEmpty());
  }

  public void clear() {
    this.actionsMap.clear();
    this.hasDistanceActionEvent = false;
  }

  public void load(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_ACTION_EVENT_SET_TAG)) {
      return;
    }

    // Clear existing actions
    this.clear();

    // Load actions data
    CompoundTag actionDataSetTag = compoundTag.getCompound(DATA_ACTION_EVENT_SET_TAG);
    for (ActionEventType actionEventType : ActionEventType.values()) {
      ActionDataSet actionDataEntryList =
          new ActionDataSet(actionDataSetTag, actionEventType.name());
      if (!actionDataEntryList.isEmpty()) {
        this.actionsMap.put(actionEventType, actionDataEntryList);
      }
    }
    this.updateHasDistanceAction();
  }

  public CompoundTag save(CompoundTag compoundTag) {
    CompoundTag actionsTag = new CompoundTag();
    for (Entry<ActionEventType, ActionDataSet> entry : this.actionsMap.entrySet()) {
      ActionEventType actionEventType = entry.getKey();
      ActionDataSet actionDataSet = entry.getValue();
      actionDataSet.save(actionsTag, actionEventType.name());
    }
    compoundTag.put(DATA_ACTION_EVENT_SET_TAG, actionsTag);
    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "ActionEventSet [ hasDistanceActionEvent="
        + this.hasDistanceActionEvent
        + ", "
        + this.actionsMap
        + "]";
  }
}
