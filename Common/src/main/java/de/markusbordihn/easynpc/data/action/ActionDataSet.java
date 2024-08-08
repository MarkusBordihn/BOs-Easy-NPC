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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public final class ActionDataSet {

  public static final String ACTION_DATA_SET_TAG = "ActionDataSet";
  private final Set<ActionDataEntry> actionDataEntries = new LinkedHashSet<>();

  public ActionDataSet() {}

  public ActionDataSet(CompoundTag compoundTag) {
    this.load(compoundTag, ACTION_DATA_SET_TAG);
  }

  public ActionDataSet(CompoundTag compoundTag, String listName) {
    this.load(compoundTag, listName);
  }

  public void add(ActionDataEntry actionDataEntry) {
    if (actionDataEntry != null) {
      this.actionDataEntries.add(actionDataEntry);
    }
  }

  public void remove(ActionDataEntry actionDataEntry) {
    if (actionDataEntry != null) {
      this.actionDataEntries.remove(actionDataEntry);
    }
  }

  public void remove(UUID actionDataEntryId) {
    if (actionDataEntryId != null) {
      ActionDataEntry actionDataEntry = this.getEntry(actionDataEntryId);
      if (actionDataEntry != null) {
        this.actionDataEntries.remove(actionDataEntry);
      }
    }
  }

  public void put(UUID actionDataEntryId, ActionDataEntry actionDataEntry) {
    if (actionDataEntryId == null || actionDataEntry == null) {
      return;
    }

    // Store indexed version of action data set to keep order.
    ArrayList<ActionDataEntry> indexedActionDataSet = new ArrayList<>(this.actionDataEntries);
    int index = -1;

    // Find index of action data entry.
    for (int i = 0; i < indexedActionDataSet.size(); i++) {
      if (indexedActionDataSet.get(i).getId().equals(actionDataEntryId)) {
        index = i;
        break;
      }
    }

    // Replace action data entry.
    if (index >= 0) {
      indexedActionDataSet.set(index, actionDataEntry);
    } else {
      indexedActionDataSet.add(actionDataEntry);
    }

    // Rebuild action data set.
    this.actionDataEntries.clear();
    this.actionDataEntries.addAll(indexedActionDataSet);
  }

  public void moveUp(ActionDataEntry actionDataEntry) {
    if (actionDataEntry == null) {
      return;
    }
    int position = this.getPosition(actionDataEntry);
    if (position <= 0) {
      return;
    }
    ArrayList<ActionDataEntry> indexedActionDataSet = new ArrayList<>(this.actionDataEntries);
    ActionDataEntry previousActionDataEntry = indexedActionDataSet.get(position - 1);
    indexedActionDataSet.set(position - 1, actionDataEntry);
    indexedActionDataSet.set(position, previousActionDataEntry);
    this.actionDataEntries.clear();
    this.actionDataEntries.addAll(indexedActionDataSet);
  }

  public void moveDown(ActionDataEntry actionDataEntry) {
    if (actionDataEntry == null) {
      return;
    }
    int position = this.getPosition(actionDataEntry);
    if (position < 0 || position >= this.actionDataEntries.size() - 1) {
      return;
    }
    ArrayList<ActionDataEntry> indexedActionDataSet = new ArrayList<>(this.actionDataEntries);
    ActionDataEntry nextActionDataEntry = indexedActionDataSet.get(position + 1);
    indexedActionDataSet.set(position + 1, actionDataEntry);
    indexedActionDataSet.set(position, nextActionDataEntry);
    this.actionDataEntries.clear();
    this.actionDataEntries.addAll(indexedActionDataSet);
  }

  public boolean isEmpty() {
    return this.actionDataEntries.isEmpty();
  }

  public boolean hasActionData() {
    if (this.actionDataEntries.isEmpty()) {
      return false;
    }
    for (ActionDataEntry action : this.actionDataEntries) {
      if (action.isValidAndNotEmpty()) {
        return true;
      }
    }
    return false;
  }

  public Iterator<ActionDataEntry> iterator() {
    return this.actionDataEntries.iterator();
  }

  public int size() {
    return this.actionDataEntries.size();
  }

  public ActionDataEntry getRandomEntry() {
    if (this.actionDataEntries.isEmpty()) {
      return null;
    }
    return this.actionDataEntries.iterator().next();
  }

  public Set<ActionDataEntry> getEntries() {
    return this.actionDataEntries;
  }

  public ActionDataEntry getEntry(UUID actionDataEntryId) {
    if (actionDataEntryId == null) {
      return null;
    }
    for (ActionDataEntry actionDataEntry : this.actionDataEntries) {
      if (actionDataEntry.getId().equals(actionDataEntryId)) {
        return actionDataEntry;
      }
    }
    return null;
  }

  public boolean contains(UUID actionDataEntryId) {
    return this.getEntry(actionDataEntryId) != null;
  }

  public boolean contains(ActionDataEntry actionDataEntry) {
    return this.actionDataEntries.contains(actionDataEntry);
  }

  public int getPosition(ActionDataEntry actionDataEntry) {
    if (actionDataEntry == null) {
      return -1;
    }
    int position = 0;
    for (ActionDataEntry entry : this.actionDataEntries) {
      if (entry.getId().equals(actionDataEntry.getId())) {
        return position;
      }
      position++;
    }
    return -1;
  }

  public ActionDataEntry getEntryOrDefault(UUID actionDataEntryId) {
    ActionDataEntry actionDataEntry = this.getEntry(actionDataEntryId);
    return actionDataEntry != null ? actionDataEntry : new ActionDataEntry();
  }

  public ActionDataSet load(CompoundTag compoundTag, String listName) {
    if (compoundTag == null
        || listName == null
        || listName.isEmpty()
        || !compoundTag.contains(listName)) {
      return new ActionDataSet();
    }
    ListTag actionDataList = compoundTag.getList(listName, 10);
    return this.load(actionDataList);
  }

  public ActionDataSet load(ListTag actionDataList) {
    if (actionDataList == null || actionDataList.isEmpty()) {
      return new ActionDataSet();
    }
    this.actionDataEntries.clear();
    for (int i = 0; i < actionDataList.size(); i++) {
      CompoundTag actionDataEntryTag = actionDataList.getCompound(i);
      ActionDataEntry actionDataEntry = new ActionDataEntry(actionDataEntryTag);
      this.actionDataEntries.add(actionDataEntry);
    }
    return this;
  }

  public void save(CompoundTag compoundTag, String listName) {
    ListTag actionDataList = new ListTag();
    this.save(actionDataList);
    compoundTag.put(listName, actionDataList);
  }

  public void save(ListTag actionDataList) {
    if (actionDataList == null) {
      return;
    }
    for (ActionDataEntry actionDataEntry : this.actionDataEntries) {
      if (actionDataEntry == null || !actionDataEntry.isValidAndNotEmpty()) {
        continue;
      }
      actionDataList.add(actionDataEntry.createTag());
    }
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    this.save(compoundTag, ACTION_DATA_SET_TAG);
    return compoundTag;
  }

  public String toString() {
    return this.actionDataEntries.toString();
  }
}
