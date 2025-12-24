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

package de.markusbordihn.easynpc.data.condition;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public class ConditionDataSet {

  public static final ConditionDataSet EMPTY = new ConditionDataSet();
  public static final String CONDITION_DATA_SET_TAG = "ConditionDataSet";
  private final Set<ConditionDataEntry> conditionDataEntries = new LinkedHashSet<>();

  public ConditionDataSet() {}

  public ConditionDataSet(Set<ConditionDataEntry> conditionDataEntries) {
    if (conditionDataEntries != null) {
      this.conditionDataEntries.addAll(conditionDataEntries);
    }
  }

  public ConditionDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public boolean isEmpty() {
    return this.conditionDataEntries.isEmpty();
  }

  public int size() {
    return this.conditionDataEntries.size();
  }

  public Set<ConditionDataEntry> getConditions() {
    return new LinkedHashSet<>(this.conditionDataEntries);
  }

  public List<ConditionDataEntry> getConditionsList() {
    return new ArrayList<>(this.conditionDataEntries);
  }

  public ConditionDataEntry getCondition(UUID id) {
    for (ConditionDataEntry entry : this.conditionDataEntries) {
      if (entry.getId().equals(id)) {
        return entry;
      }
    }
    return null;
  }

  public boolean hasCondition(UUID id) {
    return getCondition(id) != null;
  }

  public void add(ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntry != null && conditionDataEntry.isValid()) {
      remove(conditionDataEntry.getId());
      this.conditionDataEntries.add(conditionDataEntry);
    }
  }

  public boolean remove(UUID id) {
    return this.conditionDataEntries.removeIf(entry -> entry.getId().equals(id));
  }

  public void put(UUID conditionDataEntryId, ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntryId == null || conditionDataEntry == null) {
      return;
    }
    remove(conditionDataEntryId);
    if (conditionDataEntry.isValid()) {
      this.conditionDataEntries.add(conditionDataEntry);
    }
  }

  public void update(ConditionDataEntry conditionDataEntry) {
    if (conditionDataEntry != null && conditionDataEntry.isValid()) {
      remove(conditionDataEntry.getId());
      this.conditionDataEntries.add(conditionDataEntry);
    }
  }

  public void clear() {
    this.conditionDataEntries.clear();
  }

  public void load(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(CONDITION_DATA_SET_TAG)) {
      return;
    }
    CompoundTag conditionDataSetTag = compoundTag.getCompound(CONDITION_DATA_SET_TAG);

    // Load condition data entries
    this.conditionDataEntries.clear();
    ListTag conditionDataEntriesTag = conditionDataSetTag.getList(CONDITION_DATA_SET_TAG, 10);
    for (int i = 0; i < conditionDataEntriesTag.size(); i++) {
      CompoundTag conditionDataEntryTag = conditionDataEntriesTag.getCompound(i);
      ConditionDataEntry conditionDataEntry = new ConditionDataEntry(conditionDataEntryTag);
      if (conditionDataEntry.isValid()) {
        this.conditionDataEntries.add(conditionDataEntry);
      }
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    return this.save(compoundTag, CONDITION_DATA_SET_TAG);
  }

  public CompoundTag save(CompoundTag compoundTag, String tag) {
    if (isEmpty()) {
      return compoundTag;
    }

    // Save condition data entries
    CompoundTag conditionDataSetTag = new CompoundTag();
    ListTag conditionDataEntriesTag = new ListTag();
    for (ConditionDataEntry conditionDataEntry : this.conditionDataEntries) {
      if (conditionDataEntry == null || !conditionDataEntry.isValid()) {
        continue;
      }
      conditionDataEntriesTag.add(conditionDataEntry.createTag());
    }

    if (!conditionDataEntriesTag.isEmpty()) {
      conditionDataSetTag.put(CONDITION_DATA_SET_TAG, conditionDataEntriesTag);
      compoundTag.put(tag, conditionDataSetTag);
    }

    return compoundTag;
  }

  public boolean hasConditionData() {
    return !isEmpty();
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (object == null || getClass() != object.getClass()) {
      return false;
    }
    ConditionDataSet other = (ConditionDataSet) object;
    return this.conditionDataEntries.equals(other.conditionDataEntries);
  }

  @Override
  public int hashCode() {
    return this.conditionDataEntries.hashCode();
  }

  @Override
  public String toString() {
    return "ConditionDataSet[size=" + size() + ", entries=" + this.conditionDataEntries + "]";
  }
}
