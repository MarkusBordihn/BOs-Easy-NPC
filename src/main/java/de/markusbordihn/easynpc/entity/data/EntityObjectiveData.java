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

import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.data.custom.CustomDataIndex;
import de.markusbordihn.easynpc.data.entity.CustomDataSerializers;
import de.markusbordihn.easynpc.data.entity.CustomEntityData;
import de.markusbordihn.easynpc.data.objective.ObjectiveData;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.goal.target.ResetUniversalAngerTargetGoal;

public interface EntityObjectiveData extends EntityDataInterface {

  // Synced entity data
  EntityDataAccessor<Boolean> DATA_HAS_OBJECTIVES =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_HAS_PLAYER_TARGET =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_HAS_ENTITY_TARGET =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // Custom entity data
  CustomDataAccessor<ObjectiveDataSet> CUSTOM_DATA_OBJECTIVE_DATA_SET =
      CustomEntityData.defineId(
          CustomDataIndex.OBJECTIVE_DATA_SET, CustomDataSerializers.OBJECTIVE_DATA_SET);
  CustomDataAccessor<HashSet<String>> CUSTOM_DATA_TARGETED_PLAYER_SET =
      CustomEntityData.defineId(
          CustomDataIndex.OBJECTIVE_PLAYER_SET, CustomDataSerializers.STRING_HASH_SET);
  CustomDataAccessor<HashSet<UUID>> CUSTOM_DATA_TARGETED_ENTITY_SET =
      CustomEntityData.defineId(
          CustomDataIndex.OBJECTIVE_ENTITY_SET, CustomDataSerializers.UUID_HASH_SET);

  // CompoundTags
  String DATA_OBJECTIVE_DATA_TAG = "ObjectiveData";
  String DATA_HAS_OBJECTIVE_TAG = "HasObjectives";
  String DATA_HAS_TRAVEL_TARGET_TAG = "HasTravelTarget";
  String DATA_HAS_PLAYER_TARGET_TAG = "HasPlayerTarget";
  String DATA_HAS_ENTITY_TARGET_TAG = "HasEntityTarget";

  default void clearObjectiveDataSet() {
    setCustomEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, new ObjectiveDataSet());
  }

  default ObjectiveDataSet getObjectiveDataSet() {
    return getCustomEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET);
  }

  default void setObjectiveDataSet(ObjectiveDataSet objectiveDataSet) {
    setCustomEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, objectiveDataSet);
  }

  default boolean hasObjective(String objectiveId) {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasObjective(objectiveId);
  }

  default boolean hasObjective(ObjectiveData objectiveData) {
    return getObjectiveDataSet() != null
        && getObjectiveDataSet().hasObjective(objectiveData.getId());
  }

  default boolean hasObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasObjectives();
  }

  default boolean hasTravelTargetObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasTravelTarget();
  }

  default boolean hasPlayerTargetObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasPlayerTarget();
  }

  default boolean hasEntityTargetObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasEntityTarget();
  }

  default boolean hasOwnerTargetObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasOwnerTarget();
  }

  default boolean hasValidTargetObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasValidTarget(this.getEntity());
  }

  default void onEasyNPCJoinUpdateObjective(EasyNPCEntity easyNPCEntity) {
    // Check if we need to re-register NPC based objectives.
    if (this.hasEntityTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget(this.getEntity())
        && getObjectiveDataSet().isTargetedEntity(easyNPCEntity.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void onEasyNPCLeaveUpdateObjective(EasyNPCEntity easyNPCEntity) {
    // Check if we need to re-register NPC based objectives.
    if (this.hasEntityTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget(this.getEntity())
        && getObjectiveDataSet().isTargetedEntity(easyNPCEntity.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void onPlayerJoinUpdateObjective(ServerPlayer serverPlayer) {
    // Check if we need to re-register owner and player based objectives.
    if (this.hasOwnerTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget(this.getEntity())
        && (this.getEntity().isOwner(serverPlayer)
            || this.getObjectiveDataSet().isTargetedPlayer(serverPlayer.getName().getString()))) {
      this.refreshCustomObjectives();
    }
  }

  default void onPlayerLeaveUpdateObjective(ServerPlayer serverPlayer) {
    // Check if we need to re-register owner and player based objectives.
    if (this.hasOwnerTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget(this.getEntity())
        && (this.getEntity().isOwner(serverPlayer)
            || this.getObjectiveDataSet().isTargetedPlayer(serverPlayer.getName().getString()))) {
      this.refreshCustomObjectives();
    }
  }

  default void onLivingEntityJoinUpdateObjective(LivingEntity livingEntity) {
    // Check if we need to re-register living entity based objectives.
    if (this.hasEntityTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget(this.getEntity())
        && this.getObjectiveDataSet().isTargetedEntity(livingEntity.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void onLivingEntityLeaveUpdateObjective(LivingEntity livingEntity) {
    // Check if we need to re-register living entity based objectives.
    if (this.hasObjectives()
        && this.getObjectiveDataSet().hasValidTarget(this.getEntity())
        && this.getObjectiveDataSet().isTargetedEntity(livingEntity.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void refreshCustomObjectives() {
    EasyNPCEntity entity = this.getEntity();
    if (entity == null || entity.isClientSide()) {
      return;
    }
    for (ObjectiveData objectiveData : getObjectiveDataSet().getObjectives()) {
      if (objectiveData != null
          && objectiveData.getType() != ObjectiveType.NONE
          && (!objectiveData.hasValidTarget(this.getEntity()) || !objectiveData.isRegistered())) {
        log.debug("Refreshing Objective {} for {}", objectiveData, entity);
        addOrUpdateCustomObjective(objectiveData);
      }
    }
  }

  default void registerAttributeBasedObjectives() {
    EasyNPCEntity entity = this.getEntity();
    if (entity == null || entity.isClientSide()) {
      return;
    }
    log.info("Register attribute based objectives for {}", entity);

    // Handle floating goals.
    ObjectiveData floatObjective = new ObjectiveData(ObjectiveType.FLOAT, 0);
    if (entity.getAttributeCanFloat()) {
      if (!this.hasObjective(floatObjective)) {
        this.addOrUpdateCustomObjective(floatObjective);
      }
    } else if (this.hasObjective(floatObjective)) {
      this.removeCustomObjective(floatObjective);
    }

    // Handle close door interaction goals.
    ObjectiveData closeDoorObjective = new ObjectiveData(ObjectiveType.CLOSE_DOOR, 8);
    if (entity.getAttributeCanCloseDoor()) {
      if (!this.hasObjective(closeDoorObjective)) {
        this.addOrUpdateCustomObjective(closeDoorObjective);
      }
    } else if (this.hasObjective(closeDoorObjective)) {
      this.removeCustomObjective(closeDoorObjective);
    }

    // Handle open door interaction goals.
    ObjectiveData openDoorObjective = new ObjectiveData(ObjectiveType.OPEN_DOOR, 8);
    if (entity.getAttributeCanOpenDoor()) {
      if (!this.hasObjective(closeDoorObjective)) {
        this.addOrUpdateCustomObjective(openDoorObjective);
      }
    } else if (this.hasObjective(openDoorObjective)) {
      this.removeCustomObjective(openDoorObjective);
    }
  }

  default void registerCustomObjectives() {
    EasyNPCEntity entity = this.getEntity();
    if (entity == null || entity.isClientSide()) {
      return;
    }
    Set<ObjectiveData> objectives = this.getObjectiveDataSet().getObjectives();
    if (objectives == null || objectives.isEmpty()) {
      return;
    }
    log.debug("Register custom objectives for {}", entity);
    GoalSelector targetSelector = this.getEntityTargetSelector();
    for (ObjectiveData objectiveData : objectives) {
      addOrUpdateCustomObjective(objectiveData);
    }

    // Reset targets if any target objective was registered.
    if (!targetSelector.getAvailableGoals().isEmpty()) {
      log.debug("- Register reset universal anger target for {}", entity);
      targetSelector.addGoal(4, new ResetUniversalAngerTargetGoal<>(entity, false));
    }
  }

  default boolean addOrUpdateCustomObjective(ObjectiveData objectiveData) {
    if (objectiveData == null || objectiveData.getType() == ObjectiveType.NONE) {
      log.error("- Unable to add custom objective {} for {}!", objectiveData, this.getEntity());
      return false;
    }

    boolean addedCustomObjective = false;

    // Handle goal specific objectives.
    Goal goal = objectiveData.getGoal(this.getEntity());
    if (goal != null) {
      GoalSelector goalSelector = this.getEntityGoalSelector();
      if (!objectiveData.hasValidTarget(this.getEntity())) {
        if (this.hasObjective(objectiveData.getId()) && objectiveData.isRegistered()) {
          log.warn(
              "- Removing existing goal {} for {} because target was not found! Will try later again.",
              goal,
              this.getEntity());
        }
        goalSelector.removeGoal(goal);
      } else {
        log.debug("- Adding goal {} for {}", goal, this.getEntity());
        goalSelector.removeGoal(goal);
        goalSelector.addGoal(objectiveData.getPriority(), goal);
        addedCustomObjective = true;
      }
    }

    // Handle target specific objectives.
    Goal target = objectiveData.getTarget(this.getEntity());
    if (target != null) {
      log.debug("- Adding target goal {} for {}", target, this.getEntity());
      GoalSelector targetSelector = this.getEntityTargetSelector();
      targetSelector.removeGoal(target);
      targetSelector.addGoal(objectiveData.getPriority(), target);
      addedCustomObjective = true;
    }

    // Set registered flag.
    objectiveData.setRegistered(addedCustomObjective);

    // Add objective data to set, regardless if goal or target was added.
    getObjectiveDataSet().addObjective(objectiveData);
    return objectiveData.isRegistered();
  }

  default boolean removeCustomObjective(ObjectiveData objectiveData) {
    if (objectiveData == null || objectiveData.getType() == ObjectiveType.NONE) {
      log.error("- Unable to remove custom objective {} for {}!", objectiveData, this.getEntity());
      return false;
    }

    // Make sure we have the correct objective data and not a copy or clone.
    if (objectiveData.getId() != null && !objectiveData.getId().isEmpty()) {
      objectiveData = this.getObjectiveDataSet().getObjective(objectiveData.getId());
      if (objectiveData == null) {
        log.error(
            "- Unable to remove non-existing custom objective {} for {}!",
            objectiveData,
            this.getEntity());
        return false;
      }
    }

    // Remove goal and target if available.
    Goal goal = objectiveData.getGoal(this.getEntity());
    Goal target = objectiveData.getTarget(this.getEntity());
    if (goal == null && target == null) {
      log.error("- Unable to remove custom objective for {}!", this.getEntity());
      return false;
    }

    if (goal != null) {
      log.debug("- Removing goal {} for {}", goal, this.getEntity());
      this.getEntityGoalSelector().removeGoal(goal);
    }

    if (target != null) {
      log.debug("- Removing target goal {} for {}", target, this.getEntity());
      this.getEntityTargetSelector().removeGoal(target);
    }

    return this.getObjectiveDataSet().removeObjective(objectiveData);
  }

  default void registerStandardObjectives() {
    log.debug("Register standard objectives for {}", this);
    this.addOrUpdateCustomObjective(new ObjectiveData(ObjectiveType.LOOK_AT_RESET, 9));
    this.addOrUpdateCustomObjective(new ObjectiveData(ObjectiveType.LOOK_AT_PLAYER, 9));
    this.addOrUpdateCustomObjective(new ObjectiveData(ObjectiveType.LOOK_AT_MOB, 10));
  }

  default void defineSynchedObjectiveData() {
    defineEntityData(DATA_HAS_OBJECTIVES, false);
    defineEntityData(DATA_HAS_PLAYER_TARGET, false);
    defineEntityData(DATA_HAS_ENTITY_TARGET, false);
  }

  default void defineCustomObjectiveData() {
    defineCustomEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, new ObjectiveDataSet());
    defineCustomEntityData(CUSTOM_DATA_TARGETED_PLAYER_SET, new HashSet<>());
    defineCustomEntityData(CUSTOM_DATA_TARGETED_ENTITY_SET, new HashSet<>());
  }

  default void addAdditionalObjectiveData(CompoundTag compoundTag) {
    CompoundTag objectiveTag = new CompoundTag();

    // Store objectives
    getObjectiveDataSet().save(objectiveTag);

    // Store debugging flags for objectives targeting.
    objectiveTag.putBoolean(DATA_HAS_OBJECTIVE_TAG, this.hasObjectives());
    if (this.hasTravelTargetObjectives()) {
      objectiveTag.putBoolean(DATA_HAS_TRAVEL_TARGET_TAG, this.hasTravelTargetObjectives());
    }
    if (this.hasPlayerTargetObjectives()) {
      objectiveTag.putBoolean(DATA_HAS_PLAYER_TARGET_TAG, this.hasPlayerTargetObjectives());
    }
    if (this.hasEntityTargetObjectives()) {
      objectiveTag.putBoolean(DATA_HAS_ENTITY_TARGET_TAG, this.hasEntityTargetObjectives());
    }

    compoundTag.put(DATA_OBJECTIVE_DATA_TAG, objectiveTag);
  }

  default void readAdditionalObjectiveData(CompoundTag compoundTag) {

    // Adding legacy support for old NPC data.
    if (this.getEntity().getNPCDataVersion() == -1
        && !compoundTag.contains(DATA_OBJECTIVE_DATA_TAG)) {
      log.info("Converting legacy objectives for {}", this.getEntity());
      this.registerStandardObjectives();
      return;
    }

    // Early exit if no objective data is available.
    if (!compoundTag.contains(DATA_OBJECTIVE_DATA_TAG)) {
      return;
    }

    // Read objective data set
    CompoundTag objectiveDataTag = compoundTag.getCompound(DATA_OBJECTIVE_DATA_TAG);
    if (objectiveDataTag.contains(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG)) {
      ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet(objectiveDataTag);
      this.setObjectiveDataSet(objectiveDataSet);
      this.registerCustomObjectives();
    }

    // Re-Register standard objectives for legacy NPCs.
    if (this.getEntity().getNPCDataVersion() == -1) {
      this.registerStandardObjectives();
    }
  }
}
