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
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
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
  private final HashMap<String, ObjectiveData> objectives = new HashMap<>();
  private final HashSet<String> targetedPlayerSet = new HashSet<>();
  private final HashSet<UUID> targetedEntitySet = new HashSet<>();
  // Data
  private boolean hasPlayerTarget = false;
  private boolean hasTravelTarget = false;
  private boolean hasObjectives = false;
  private boolean hasEntityTarget = false;
  private boolean hasOwnerTarget = false;

  public ObjectiveDataSet() {
  }

  public ObjectiveDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public Set<ObjectiveData> getObjectives() {
    return new HashSet<>(this.objectives.values());
  }

  public ObjectiveData getOrCreateObjective(ObjectiveType objectiveType, int priority) {
    if (this.hasObjective(objectiveType)) {
      return this.getObjective(objectiveType);
    }
    return new ObjectiveData(objectiveType, priority);
  }

  public ObjectiveData getObjective(ObjectiveType objectiveType) {
    return this.getObjective(objectiveType.name());
  }

  public ObjectiveData getObjective(String objectiveId) {
    ObjectiveData objectiveData = this.objectives.get(objectiveId);
    if (objectiveData != null && objectiveData.getType() != ObjectiveType.NONE) {
      return objectiveData;
    }
    return null;
  }

  public boolean hasObjective(String objectiveId) {
    ObjectiveData objectiveData = this.objectives.get(objectiveId);
    return objectiveData != null && objectiveData.getType() != ObjectiveType.NONE;
  }

  public boolean hasObjective(ObjectiveType objectiveType) {
    return hasObjective(objectiveType.name());
  }

  public boolean hasObjectives() {
    return this.hasObjectives;
  }

  public void addObjective(ObjectiveData objectiveData) {
    if (objectiveData == null || objectiveData.getType() == ObjectiveType.NONE) {
      return;
    }
    this.objectives.put(objectiveData.getId(), objectiveData);
    this.updateTargetFlags();
  }

  public boolean removeObjective(ObjectiveData objectiveData) {
    return this.removeObjective(objectiveData.getId());
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

  public boolean hasValidTarget(EasyNPCEntity easyNPCEntity) {
    for (ObjectiveData objectiveData : this.objectives.values()) {
      if (objectiveData == null || objectiveData.getType() == ObjectiveType.NONE) {
        continue;
      }
      if (!objectiveData.hasValidTarget(easyNPCEntity)) {
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
    for (ObjectiveData objectiveData : this.objectives.values()) {
      if (objectiveData == null || objectiveData.getType() == ObjectiveType.NONE) {
        continue;
      }

      // Check if we have any travel objectives
      if (objectiveData.hasTravelObjective()) {
        hasTravelObjectives = true;
      }

      // Check if we have any object with a targeted player or entity.
      if (objectiveData.hasPlayerTarget()) {
        targetedPlayerSet.add(objectiveData.getTargetPlayerName());
        hasPlayerTargetObjective = true;
      } else if (objectiveData.hasEntityTarget()) {
        targetedEntitySet.add(objectiveData.getTargetEntityUUID());
        hasEntityTargetObjective = true;
      } else if (objectiveData.hasOwnerTarget()) {
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
      ObjectiveData objectiveData = new ObjectiveData(objectiveDataTag);
      this.addObjective(objectiveData);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag objectiveDataList = new ListTag();
    for (ObjectiveData objectiveData : this.objectives.values()) {
      // Skip empty objectives
      if (objectiveData == null || objectiveData.getType() == ObjectiveType.NONE) {
        continue;
      }
      objectiveDataList.add(objectiveData.createTag());
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
