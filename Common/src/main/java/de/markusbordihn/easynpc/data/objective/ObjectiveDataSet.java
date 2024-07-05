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

package de.markusbordihn.easynpc.data.objective;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveDataSet {

  // Objective Data Tags
  public static final String DATA_OBJECTIVE_DATA_SET_TAG = "ObjectiveDataSet";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final HashMap<String, ObjectiveDataEntry> objectives = new HashMap<>();
  private final HashSet<String> targetedPlayerSet = new HashSet<>();
  private final HashSet<UUID> targetedEntitySet = new HashSet<>();
  private boolean hasEntityTarget = false;
  private boolean hasObjectives = false;
  private boolean hasOwnerTarget = false;
  // Data
  private boolean hasPlayerTarget = false;
  private boolean hasTravelTarget = false;

  public ObjectiveDataSet() {}

  public ObjectiveDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public Set<ObjectiveDataEntry> getObjectives() {
    return new HashSet<>(this.objectives.values());
  }

  public ObjectiveDataEntry getOrCreateObjective(ObjectiveType objectiveType) {
    return this.getOrCreateObjective(objectiveType, objectiveType.getDefaultPriority());
  }

  public ObjectiveDataEntry getOrCreateObjective(ObjectiveType objectiveType, int priority) {
    if (this.hasObjective(objectiveType)) {
      ObjectiveDataEntry objectiveDataEntry = this.getObjective(objectiveType);
      if (objectiveDataEntry.getPriority() != priority) {
        objectiveDataEntry.setPriority(priority);
      }
      return objectiveDataEntry;
    }
    return new ObjectiveDataEntry(objectiveType, priority);
  }

  public ObjectiveDataEntry getObjective(ObjectiveType objectiveType) {
    return this.getObjective(objectiveType.name());
  }

  public ObjectiveDataEntry getObjective(String objectiveId) {
    ObjectiveDataEntry objectiveDataEntry = this.objectives.get(objectiveId);
    if (objectiveDataEntry != null && objectiveDataEntry.getType() != ObjectiveType.NONE) {
      return objectiveDataEntry;
    }
    return null;
  }

  public boolean hasObjective(String objectiveId) {
    ObjectiveDataEntry objectiveDataEntry = this.objectives.get(objectiveId);
    return objectiveDataEntry != null && objectiveDataEntry.getType() != ObjectiveType.NONE;
  }

  public boolean hasObjective(ObjectiveType objectiveType) {
    return hasObjective(objectiveType.name());
  }

  public boolean hasObjectives() {
    return this.hasObjectives;
  }

  public void addObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
      return;
    }
    this.objectives.put(objectiveDataEntry.getId(), objectiveDataEntry);
    this.updateTargetFlags();
  }

  public void removeObjective(ObjectiveType objectiveType) {
    this.removeObjective(objectiveType.name());
  }

  public boolean removeObjective(ObjectiveDataEntry objectiveDataEntry) {
    return this.removeObjective(objectiveDataEntry.getId());
  }

  public boolean removeObjective(String objectiveId) {
    boolean removed = this.objectives.remove(objectiveId) != null;
    if (removed) {
      this.updateTargetFlags();
    }
    return removed;
  }

  public boolean hasTravelTarget() {
    return this.hasTravelTarget;
  }

  public boolean hasPlayerTarget() {
    return this.hasPlayerTarget;
  }

  public boolean hasEntityTarget() {
    return this.hasEntityTarget;
  }

  public boolean hasOwnerTarget() {
    return this.hasOwnerTarget;
  }

  public boolean isTargetedPlayer(String playerName) {
    return playerName != null
        && playerName.isEmpty()
        && this.targetedPlayerSet.contains(playerName);
  }

  public boolean isTargetedEntity(UUID entityUUID) {
    return entityUUID != null && this.targetedEntitySet.contains(entityUUID);
  }

  public boolean hasValidTarget(EasyNPC<?> easyNPC) {
    for (ObjectiveDataEntry objectiveDataEntry : this.objectives.values()) {
      if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
        continue;
      }
      if (!objectiveDataEntry.hasValidTarget(easyNPC)) {
        return false;
      }
    }
    return true;
  }

  public void clear() {
    this.objectives.clear();
    this.hasObjectives = false;
  }

  private void updateTargetFlags() {
    // Clear existing target sets
    this.targetedPlayerSet.clear();
    this.targetedEntitySet.clear();

    boolean hasTravelObjectives = false;
    boolean hasPlayerTargetObjective = false;
    boolean hasEntityTargetObjective = false;
    boolean hasOwnerTargetObjective = false;
    for (ObjectiveDataEntry objectiveDataEntry : this.objectives.values()) {
      if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
        continue;
      }

      // Check if we have any travel objectives
      if (objectiveDataEntry.hasTravelObjective()) {
        hasTravelObjectives = true;
      }

      // Check if we have any object with a targeted player or entity.
      if (objectiveDataEntry.hasPlayerTarget()) {
        targetedPlayerSet.add(objectiveDataEntry.getTargetPlayerName());
        hasPlayerTargetObjective = true;
      } else if (objectiveDataEntry.hasEntityTarget()) {
        targetedEntitySet.add(objectiveDataEntry.getTargetEntityUUID());
        hasEntityTargetObjective = true;
      } else if (objectiveDataEntry.hasOwnerTarget()) {
        hasOwnerTargetObjective = true;
      }
    }

    // Update target flags
    this.hasTravelTarget = hasTravelObjectives;
    this.hasPlayerTarget = hasPlayerTargetObjective;
    this.hasEntityTarget = hasEntityTargetObjective;
    this.hasOwnerTarget = hasOwnerTargetObjective;
    this.hasObjectives = !this.objectives.isEmpty();
  }

  public void load(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_OBJECTIVE_DATA_SET_TAG)) {
      return;
    }

    // Clear existing objectives
    this.clear();

    // Load objectives
    ListTag objectiveDataList = compoundTag.getList(DATA_OBJECTIVE_DATA_SET_TAG, 10);
    for (int i = 0; i < objectiveDataList.size(); i++) {
      CompoundTag objectiveDataTag = objectiveDataList.getCompound(i);
      ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(objectiveDataTag);
      this.addObjective(objectiveDataEntry);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag objectiveDataList = new ListTag();
    for (ObjectiveDataEntry objectiveDataEntry : this.objectives.values()) {
      // Skip empty objectives
      if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
        continue;
      }
      objectiveDataList.add(objectiveDataEntry.createTag());
    }
    compoundTag.put(DATA_OBJECTIVE_DATA_SET_TAG, objectiveDataList);
    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "ObjectiveDataSet [ hasObjectives="
        + this.hasObjectives
        + ", hasTravelTarget="
        + this.hasTravelTarget
        + ", hasOwnerTarget="
        + this.hasOwnerTarget
        + ", hasEntityTarget="
        + this.hasEntityTarget
        + ", hasPlayerTarget="
        + this.hasPlayerTarget
        + ", targetedEntitySet="
        + this.targetedEntitySet
        + ", targetedPlayerSet="
        + this.targetedPlayerSet
        + ", data="
        + this.objectives
        + "]";
  }
}
