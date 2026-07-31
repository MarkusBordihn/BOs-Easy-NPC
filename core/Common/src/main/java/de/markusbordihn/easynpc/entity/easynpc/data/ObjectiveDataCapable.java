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

import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveGoalFactory;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.objective.factory.ObjectiveFactoryResolver;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ResetUniversalAngerTargetGoal;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GoalSelector;

public interface ObjectiveDataCapable<T extends Mob> extends EasyNPC<T> {

  ServerDataAccessor<ObjectiveDataSet> CUSTOM_DATA_OBJECTIVE_DATA_SET =
      ServerEntityData.defineId(
          ServerDataIndex.OBJECTIVE_DATA_SET, EntityDataSerializersManager.OBJECTIVE_DATA_SET);
  ServerDataAccessor<HashSet<UUID>> CUSTOM_DATA_TARGETED_ENTITY_SET =
      ServerEntityData.defineId(
          ServerDataIndex.OBJECTIVE_ENTITY_SET,
          EntityDataSerializersManager.TARGETED_ENTITY_HASH_SET);
  ServerDataAccessor<HashSet<String>> CUSTOM_DATA_TARGETED_PLAYER_SET =
      ServerEntityData.defineId(
          ServerDataIndex.OBJECTIVE_PLAYER_SET,
          EntityDataSerializersManager.TARGETED_PLAYER_HASH_SET);
  int CUSTOM_OBJECTIVE_DELAYED_REGISTRATION_TICK = 20 * 15;
  String DATA_HAS_ENTITY_TARGET_TAG = "HasEntityTarget";
  String DATA_HAS_OBJECTIVE_TAG = "HasObjectives";
  String DATA_HAS_PLAYER_TARGET_TAG = "HasPlayerTarget";
  String DATA_HAS_TRAVEL_TARGET_TAG = "HasTravelTarget";
  String DATA_OBJECTIVE_DATA_TAG = "ObjectiveData";

  default ObjectiveDataSet getObjectiveDataSet() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET);
  }

  default void setObjectiveDataSet(ObjectiveDataSet objectiveDataSet) {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, objectiveDataSet);
    LivingEntityManager.updateObjectiveEventInterest(this);
  }

  default boolean hasObjective(String objectiveId) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasObjective(objectiveId);
  }

  default boolean hasObjective(ObjectiveType objectiveType) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasObjective(objectiveType);
  }

  default boolean hasObjective(ObjectiveDataEntry objectiveDataEntry) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasObjective(objectiveDataEntry.getId());
  }

  default boolean hasObjectives() {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasObjectives();
  }

  default boolean hasObjectives(Set<ObjectiveType> objectiveTypes) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasObjectives(objectiveTypes);
  }

  default ObjectiveDataEntry getObjective(ObjectiveType objectiveType) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveType != null
        ? objectiveDataSet.getObjective(objectiveType)
        : null;
  }

  default Optional<ObjectiveDataEntry> getObjectiveEntry(ObjectiveType objectiveType) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null
        ? Optional.ofNullable(objectiveDataSet.getObjective(objectiveType))
        : Optional.empty();
  }

  default boolean removeObjective(ObjectiveType objectiveType) {
    if (objectiveType == null) {
      return false;
    }
    return getObjectiveDataSet().removeObjective(objectiveType);
  }

  default void addObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null) {
      return;
    }
    getObjectiveDataSet().addObjective(objectiveDataEntry);
  }

  default boolean hasTravelTargetObjectives() {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasTravelTarget();
  }

  default boolean hasPlayerTargetObjectives() {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasPlayerTarget();
  }

  default boolean hasEntityTargetObjectives() {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasEntityTarget();
  }

  default boolean hasOwnerTargetObjectives() {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    return objectiveDataSet != null && objectiveDataSet.hasOwnerTarget();
  }

  default void onEasyNPCJoinUpdateObjective(EasyNPC<?> easyNPC) {
    this.refreshOnTargetEntityJoin(easyNPC.getEntityUUID());
  }

  default void onEasyNPCLeaveUpdateObjective(EasyNPC<?> easyNPC) {
    this.refreshOnTargetEntityLeave(easyNPC.getEntityUUID());
  }

  default void onLivingEntityJoinUpdateObjective(LivingEntity livingEntity) {
    this.refreshOnTargetEntityJoin(livingEntity.getUUID());
  }

  default void onLivingEntityLeaveUpdateObjective(LivingEntity livingEntity) {
    this.refreshOnTargetEntityLeave(livingEntity.getUUID());
  }

  default void onPlayerJoinUpdateObjective(ServerPlayer serverPlayer) {
    this.refreshOnTargetPlayerChange(serverPlayer);
  }

  default void onPlayerLeaveUpdateObjective(ServerPlayer serverPlayer) {
    this.refreshOnTargetPlayerChange(serverPlayer);
  }

  private void refreshOnTargetEntityJoin(UUID entityUUID) {
    ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
    if (this.hasEntityTargetObjectives()
        && !objectiveDataSet.hasValidTarget(this)
        && objectiveDataSet.isTargetedEntity(entityUUID)) {
      this.refreshCustomObjectives();
    }
  }

  private void refreshOnTargetEntityLeave(UUID entityUUID) {
    if (this.hasEntityTargetObjectives()
        && this.getObjectiveDataSet().isTargetedEntity(entityUUID)) {
      this.refreshCustomObjectives();
    }
  }

  private void refreshOnTargetPlayerChange(ServerPlayer serverPlayer) {
    if ((this.hasOwnerTargetObjectives() || this.hasPlayerTargetObjectives())
        && (this.isObjectiveOwner(serverPlayer) || this.isObjectiveTargetedPlayer(serverPlayer))) {
      this.refreshCustomObjectives();
    }
  }

  private boolean isObjectiveOwner(ServerPlayer serverPlayer) {
    return this.getEasyNPCOwnerData() != null
        && this.getEasyNPCOwnerData().isNPCOwner(serverPlayer);
  }

  private boolean isObjectiveTargetedPlayer(ServerPlayer serverPlayer) {
    return this.getObjectiveDataSet().isTargetedPlayer(serverPlayer.getName().getString());
  }

  default void refreshCustomObjectives() {
    if (this.isClientSideInstance()) {
      return;
    }
    for (ObjectiveDataEntry objectiveDataEntry : getObjectiveDataSet().getObjectives()) {
      if (objectiveDataEntry != null
          && objectiveDataEntry.getType() != ObjectiveType.NONE
          && (!objectiveDataEntry.hasValidTarget(this) || !objectiveDataEntry.isRegistered())) {
        log.debug("Refresh Objective {} for {}", objectiveDataEntry, this);
        addOrUpdateCustomObjective(objectiveDataEntry);
      }
    }
    LivingEntityManager.updateObjectiveEventInterest(this);
  }

  default void registerAttributeBasedObjectives() {
    if (this.isClientSideInstance()) {
      return;
    }
    log.debug("Register attribute based objectives for {}", this);
    EntityAttributes attributeData = this.getEasyNPCAttributeData().getEntityAttributes();
    this.syncAttributeBasedObjective(
        attributeData.getEnvironmentalAttributes().canFloat(), ObjectiveType.FLOAT);
    this.syncAttributeBasedObjective(
        attributeData.getMovementAttributes().canOpenDoor(), ObjectiveType.OPEN_DOOR);
    this.syncAttributeBasedObjective(
        attributeData.getMovementAttributes().canCloseDoor(), ObjectiveType.CLOSE_DOOR);
  }

  private void syncAttributeBasedObjective(boolean enabled, ObjectiveType objectiveType) {
    ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(objectiveType);
    if (enabled) {
      if (!this.hasObjective(objectiveDataEntry)) {
        this.addOrUpdateCustomObjective(objectiveDataEntry);
      }
    } else if (this.hasObjective(objectiveDataEntry)) {
      this.removeCustomObjective(objectiveDataEntry);
    }
  }

  default void registerCustomObjectives() {
    if (this.isClientSideInstance()) {
      return;
    }
    Set<ObjectiveDataEntry> objectives = this.getObjectiveDataSet().getObjectives();
    if (objectives == null || objectives.isEmpty()) {
      return;
    }
    log.debug("Register custom objectives for {}", this);
    GoalSelector targetSelector = this.getEntityTargetSelector();
    for (ObjectiveDataEntry objectiveDataEntry : objectives) {
      addOrUpdateCustomObjective(objectiveDataEntry);
    }

    // Reset targets if any target objective was registered.
    if (!targetSelector.getAvailableGoals().isEmpty()) {
      log.debug("- Register reset universal anger target for {}", this);
      targetSelector.addGoal(4, new ResetUniversalAngerTargetGoal<>(this, false));
    }
    LivingEntityManager.updateObjectiveEventInterest(this);
  }

  default boolean addOrUpdateCustomObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
      log.error("- Unable to add custom objective {} for {}!", objectiveDataEntry, this);
      return false;
    }

    if (this.isClientSideInstance()) {
      return false;
    }

    boolean addedCustomObjective = false;
    boolean hasValidTarget = objectiveDataEntry.hasValidTarget(this);

    Goal goal = objectiveDataEntry.getGoal(this);
    if (goal != null) {
      GoalSelector goalSelector = this.getEntityGoalSelector();
      if (!hasValidTarget) {
        if (this.hasObjective(objectiveDataEntry.getId()) && objectiveDataEntry.isRegistered()) {
          log.debug(
              "Removing existing goal {} for {} because target was not found. Will try later again.",
              goal,
              this);
        }
        goalSelector.removeGoal(goal);
      } else {
        log.debug("- Adding goal {} for {}", goal, this);
        goalSelector.removeGoal(goal);
        goalSelector.addGoal(objectiveDataEntry.getPriority(), goal);
        addedCustomObjective = true;
      }
    }

    Goal target = objectiveDataEntry.getTarget(this);
    if (target != null) {
      GoalSelector targetSelector = this.getEntityTargetSelector();
      if (!hasValidTarget) {
        log.debug(
            "Removing existing target goal {} for {} because target was not found. Will try later"
                + " again.",
            target,
            this);
        targetSelector.removeGoal(target);
      } else {
        log.debug("- Adding target goal {} for {}", target, this);
        targetSelector.removeGoal(target);
        targetSelector.addGoal(objectiveDataEntry.getPriority(), target);
        addedCustomObjective = true;
      }
    }

    if (!addedCustomObjective && goal == null && target == null) {
      this.handleUnusedObjective(objectiveDataEntry, hasValidTarget);
    } else {
      objectiveDataEntry.setRegistered(addedCustomObjective);
    }

    // Add objective data to set, regardless if goal or target was added.
    getObjectiveDataSet().addObjective(objectiveDataEntry);
    return objectiveDataEntry.isRegistered();
  }

  private void handleUnusedObjective(
      ObjectiveDataEntry objectiveDataEntry, boolean hasValidTarget) {
    if (!hasValidTarget || objectiveDataEntry.isAwaitingRegistration()) {
      return;
    }

    ObjectiveGoalFactory goalFactory = ObjectiveFactoryResolver.resolve(objectiveDataEntry);
    if (goalFactory != null && !goalFactory.isCompatible(this)) {
      log.debug(
          "- Objective {} is not compatible with {} and will not be retried.",
          objectiveDataEntry.getType(),
          this);
      objectiveDataEntry.setRegistered(true);
      return;
    }

    if (objectiveDataEntry.markUnusableObjectiveLogged()) {
      log.warn(
          "- Objective {} of {} could not be created and will be retried later, please check its"
              + " configuration!",
          objectiveDataEntry.getType(),
          this);
    }
  }

  default boolean rebuildCustomObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null || this.isClientSideInstance()) {
      return false;
    }

    Goal previousGoal = objectiveDataEntry.getGoal(this);
    if (previousGoal != null) {
      this.getEntityGoalSelector().removeGoal(previousGoal);
    }

    Goal previousTarget = objectiveDataEntry.getTarget(this);
    if (previousTarget != null) {
      this.getEntityTargetSelector().removeGoal(previousTarget);
    }

    objectiveDataEntry.setRegistered(false);
    return this.addOrUpdateCustomObjective(objectiveDataEntry);
  }

  default void handleCustomObjectiveBaseTick() {
    TickerDataCapable<?> tickerData = this.getEasyNPCTickerData();
    if (tickerData.checkAndIncreaseTicker(
        TickerType.CUSTOM_OBJECTIVE_DELAYED_REGISTRATION,
        CUSTOM_OBJECTIVE_DELAYED_REGISTRATION_TICK)) {
      if (this.hasObjectives()) {
        this.refreshCustomObjectives();
      }
      tickerData.resetTicker(TickerType.CUSTOM_OBJECTIVE_DELAYED_REGISTRATION);
    }
  }

  default boolean removeCustomObjective(ObjectiveType objectiveType) {
    return removeCustomObjective(getObjective(objectiveType));
  }

  default boolean removeCustomObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
      log.error("- Unable to remove custom objective {} for {}!", objectiveDataEntry, this);
      return false;
    }

    if (objectiveDataEntry.getId() != null && !objectiveDataEntry.getId().isEmpty()) {
      objectiveDataEntry = this.getObjectiveDataSet().getObjective(objectiveDataEntry.getId());
      if (objectiveDataEntry == null) {
        log.error(
            "- Unable to remove non-existing custom objective {} for {}!",
            objectiveDataEntry,
            this);
        return false;
      }
    }

    Goal goal = objectiveDataEntry.getGoal(this);
    Goal target = objectiveDataEntry.getTarget(this);
    if (goal == null && target == null) {
      log.error("- Unable to remove custom objective for {}!", this);
      return false;
    }

    if (goal != null) {
      log.debug("- Removing goal {} for {}", goal, this);
      this.getEntityGoalSelector().removeGoal(goal);
    }

    if (target != null) {
      log.debug("- Removing target goal {} for {}", target, this);
      this.getEntityTargetSelector().removeGoal(target);
    }

    boolean removed = this.getObjectiveDataSet().removeObjective(objectiveDataEntry);
    LivingEntityManager.updateObjectiveEventInterest(this);
    return removed;
  }

  default void registerStandardObjectives() {
    log.debug("Register standard objectives for {}", this);
    this.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.LOOK_AT_RESET));
    this.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.LOOK_AT_PLAYER));
    this.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.LOOK_AT_MOB));
  }

  default void defineCustomObjectiveData() {
    getEasyNPCServerData()
        .defineServerEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, new ObjectiveDataSet());
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_TARGETED_PLAYER_SET, new HashSet<>());
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_TARGETED_ENTITY_SET, new HashSet<>());
  }

  default void addAdditionalObjectiveData(CompoundTag compoundTag) {
    CompoundTag objectiveTag = new CompoundTag();

    if (this.isServerSideInstance()) {
      ObjectiveDataSet objectiveDataSet = this.getObjectiveDataSet();
      if (objectiveDataSet != null) {
        objectiveDataSet.save(objectiveTag);
      }

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
    }

    compoundTag.put(DATA_OBJECTIVE_DATA_TAG, objectiveTag);
  }

  default void readAdditionalObjectiveData(CompoundTag compoundTag) {

    if (!compoundTag.contains(DATA_OBJECTIVE_DATA_TAG)) {
      return;
    }

    CompoundTag objectiveDataTag = compoundTag.getCompound(DATA_OBJECTIVE_DATA_TAG);
    if (objectiveDataTag.contains(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG)) {
      ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet(objectiveDataTag);
      this.setObjectiveDataSet(objectiveDataSet);
      this.registerCustomObjectives();
    }

    // Re-Register standard objectives for legacy NPCs.
    if (this.getNPCDataVersion() == -1) {
      this.registerStandardObjectives();
    }
  }
}
