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
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ResetUniversalAngerTargetGoal;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GoalSelector;

public interface ObjectiveData<T extends PathfinderMob> extends EasyNPC<T> {

  EntityDataSerializer<ObjectiveDataSet> OBJECTIVE_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, ObjectiveDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public ObjectiveDataSet read(FriendlyByteBuf buffer) {
          return new ObjectiveDataSet(buffer.readNbt());
        }

        public ObjectiveDataSet copy(ObjectiveDataSet value) {
          return value;
        }
      };
  EntityDataSerializer<HashSet<UUID>> TARGETED_ENTITY_HASH_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, HashSet<UUID> value) {
          for (UUID entry : value) {
            buffer.writeUUID(entry);
          }
        }

        public HashSet<UUID> read(FriendlyByteBuf buffer) {
          HashSet<UUID> value = new HashSet<>();
          while (buffer.isReadable()) {
            value.add(buffer.readUUID());
          }
          return value;
        }

        public HashSet<UUID> copy(HashSet<UUID> value) {
          return value;
        }
      };
  EntityDataSerializer<HashSet<String>> TARGETED_PLAYER_HASH_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, HashSet<String> value) {
          for (String entry : value) {
            buffer.writeUtf(entry);
          }
        }

        public HashSet<String> read(FriendlyByteBuf buffer) {
          HashSet<String> value = new HashSet<>();
          while (buffer.isReadable()) {
            value.add(buffer.readUtf());
          }
          return value;
        }

        public HashSet<String> copy(HashSet<String> value) {
          return value;
        }
      };

  ServerDataAccessor<ObjectiveDataSet> CUSTOM_DATA_OBJECTIVE_DATA_SET =
      ServerEntityData.defineId(ServerDataIndex.OBJECTIVE_DATA_SET, OBJECTIVE_DATA_SET);
  ServerDataAccessor<HashSet<UUID>> CUSTOM_DATA_TARGETED_ENTITY_SET =
      ServerEntityData.defineId(ServerDataIndex.OBJECTIVE_ENTITY_SET, TARGETED_ENTITY_HASH_SET);
  ServerDataAccessor<HashSet<String>> CUSTOM_DATA_TARGETED_PLAYER_SET =
      ServerEntityData.defineId(ServerDataIndex.OBJECTIVE_PLAYER_SET, TARGETED_PLAYER_HASH_SET);
  int CUSTOM_OBJECTIVE_DELAYED_REGISTRATION_TICK = 20 * 15;
  String DATA_HAS_ENTITY_TARGET_TAG = "HasEntityTarget";
  String DATA_HAS_OBJECTIVE_TAG = "HasObjectives";
  String DATA_HAS_PLAYER_TARGET_TAG = "HasPlayerTarget";
  String DATA_HAS_TRAVEL_TARGET_TAG = "HasTravelTarget";
  String DATA_OBJECTIVE_DATA_TAG = "ObjectiveData";

  static void registerObjectiveDataSerializer() {
    EntityDataSerializers.registerSerializer(OBJECTIVE_DATA_SET);
    EntityDataSerializers.registerSerializer(TARGETED_PLAYER_HASH_SET);
    EntityDataSerializers.registerSerializer(TARGETED_ENTITY_HASH_SET);
  }

  default ObjectiveDataSet getObjectiveDataSet() {
    return getEasyNPCServerData().getServerEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET);
  }

  default void setObjectiveDataSet(ObjectiveDataSet objectiveDataSet) {
    getEasyNPCServerData().setServerEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, objectiveDataSet);
  }

  default boolean hasObjective(String objectiveId) {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasObjective(objectiveId);
  }

  default boolean hasObjective(ObjectiveType objectiveType) {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasObjective(objectiveType);
  }

  default boolean hasObjective(ObjectiveDataEntry objectiveDataEntry) {
    return getObjectiveDataSet() != null
        && getObjectiveDataSet().hasObjective(objectiveDataEntry.getId());
  }

  default boolean hasObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasObjectives();
  }

  default ObjectiveDataEntry getObjective(ObjectiveType objectiveType) {
    return getObjectiveDataSet() != null ? getObjectiveDataSet().getObjective(objectiveType) : null;
  }

  default void removeObjective(ObjectiveType objectiveType) {
    if (objectiveType == null) {
      return;
    }
    getObjectiveDataSet().removeObjective(objectiveType);
  }

  default void addObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null) {
      return;
    }
    getObjectiveDataSet().addObjective(objectiveDataEntry);
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

  default void onEasyNPCJoinUpdateObjective(EasyNPC<?> easyNPC) {
    // Check if we need to re-register NPC based objectives.
    if (this.hasEntityTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget(this)
        && getObjectiveDataSet().isTargetedEntity(easyNPC.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void onEasyNPCLeaveUpdateObjective(EasyNPC<?> easyNPC) {
    // Check if we need to re-register NPC based objectives.
    if (this.hasEntityTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget(this)
        && getObjectiveDataSet().isTargetedEntity(this.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void onPlayerJoinUpdateObjective(ServerPlayer serverPlayer) {
    // Check if we need to re-register owner and player based objectives.
    if (this.hasOwnerTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget(this)
        && (isObjectiveOwner(serverPlayer) || isObjectiveTargetedPlayer(serverPlayer))) {
      this.refreshCustomObjectives();
    }
  }

  private boolean isObjectiveOwner(ServerPlayer serverPlayer) {
    return this.getEasyNPCOwnerData() != null && this.getEasyNPCOwnerData().isOwner(serverPlayer);
  }

  private boolean isObjectiveTargetedPlayer(ServerPlayer serverPlayer) {
    return this.getObjectiveDataSet().isTargetedPlayer(serverPlayer.getName().getString());
  }

  default void onPlayerLeaveUpdateObjective(ServerPlayer serverPlayer) {
    // Check if we need to re-register owner and player based objectives.
    if (this.hasOwnerTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget(this)
        && (isObjectiveOwner(serverPlayer) || isObjectiveTargetedPlayer(serverPlayer))) {
      this.refreshCustomObjectives();
    }
  }

  default void onLivingEntityJoinUpdateObjective(LivingEntity livingEntity) {
    // Check if we need to re-register living entity based objectives.
    if (this.hasEntityTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget(this)
        && this.getObjectiveDataSet().isTargetedEntity(livingEntity.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void onLivingEntityLeaveUpdateObjective(LivingEntity livingEntity) {
    // Check if we need to re-register living entity based objectives.
    if (this.hasObjectives()
        && this.getObjectiveDataSet().hasValidTarget(this)
        && this.getObjectiveDataSet().isTargetedEntity(livingEntity.getUUID())) {
      this.refreshCustomObjectives();
    }
  }

  default void refreshCustomObjectives() {
    if (this.isClientSide()) {
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
  }

  default void registerAttributeBasedObjectives() {
    if (this.isClientSide()) {
      return;
    }
    log.debug("Register attribute based objectives for {}", this);

    // Handle floating goals.
    ObjectiveDataEntry floatObjective = new ObjectiveDataEntry(ObjectiveType.FLOAT, 0);
    EntityAttributes attributeData = this.getEasyNPCAttributeData().getEntityAttributes();
    if (attributeData.getEnvironmentalAttributes().canFloat()) {
      if (!this.hasObjective(floatObjective)) {
        this.addOrUpdateCustomObjective(floatObjective);
      }
    } else if (this.hasObjective(floatObjective)) {
      this.removeCustomObjective(floatObjective);
    }

    // Handle close door interaction goals.
    ObjectiveDataEntry closeDoorObjective = new ObjectiveDataEntry(ObjectiveType.CLOSE_DOOR, 8);
    if (attributeData.getMovementAttributes().canCloseDoor()) {
      if (!this.hasObjective(closeDoorObjective)) {
        this.addOrUpdateCustomObjective(closeDoorObjective);
      }
    } else if (this.hasObjective(closeDoorObjective)) {
      this.removeCustomObjective(closeDoorObjective);
    }

    // Handle open door interaction goals.
    ObjectiveDataEntry openDoorObjective = new ObjectiveDataEntry(ObjectiveType.OPEN_DOOR, 8);
    if (attributeData.getMovementAttributes().canOpenDoor()) {
      if (!this.hasObjective(closeDoorObjective)) {
        this.addOrUpdateCustomObjective(openDoorObjective);
      }
    } else if (this.hasObjective(openDoorObjective)) {
      this.removeCustomObjective(openDoorObjective);
    }
  }

  default void registerCustomObjectives() {
    if (this.isClientSide()) {
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
  }

  default boolean addOrUpdateCustomObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
      log.error("- Unable to add custom objective {} for {}!", objectiveDataEntry, this);
      return false;
    }

    boolean addedCustomObjective = false;

    // Handle goal specific objectives.
    Goal goal = objectiveDataEntry.getGoal(this);
    if (goal != null) {
      GoalSelector goalSelector = this.getEntityGoalSelector();
      if (!objectiveDataEntry.hasValidTarget(this)) {
        if (this.hasObjective(objectiveDataEntry.getId()) && objectiveDataEntry.isRegistered()) {
          log.warn(
              "- Removing existing goal {} for {} because target was not found! Will try later again.",
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

    // Handle target specific objectives.
    Goal target = objectiveDataEntry.getTarget(this);
    if (target != null) {
      log.debug("- Adding target goal {} for {}", target, this);
      GoalSelector targetSelector = this.getEntityTargetSelector();
      targetSelector.removeGoal(target);
      targetSelector.addGoal(objectiveDataEntry.getPriority(), target);
      addedCustomObjective = true;
    }

    // Set registered flag.
    objectiveDataEntry.setRegistered(addedCustomObjective);

    // Add objective data to set, regardless if goal or target was added.
    getObjectiveDataSet().addObjective(objectiveDataEntry);
    return objectiveDataEntry.isRegistered();
  }

  default void handleCustomObjectiveBaseTick() {
    TickerData<?> tickerData = this.getEasyNPCTickerData();
    if (tickerData.checkAndIncreaseTicker(
        TickerType.CUSTOM_OBJECTIVE_DELAYED_REGISTRATION,
        CUSTOM_OBJECTIVE_DELAYED_REGISTRATION_TICK)) {
      if (this.hasObjectives()) {
        this.refreshCustomObjectives();
      }
      tickerData.resetTicker(TickerType.CUSTOM_OBJECTIVE_DELAYED_REGISTRATION);
    }
  }

  default boolean removeCustomObjective(ObjectiveDataEntry objectiveDataEntry) {
    if (objectiveDataEntry == null || objectiveDataEntry.getType() == ObjectiveType.NONE) {
      log.error("- Unable to remove custom objective {} for {}!", objectiveDataEntry, this);
      return false;
    }

    // Make sure we have the correct objective data and not a copy or clone.
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

    // Remove goal and target if available.
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

    return this.getObjectiveDataSet().removeObjective(objectiveDataEntry);
  }

  default void registerStandardObjectives() {
    log.debug("Register standard objectives for {}", this);
    this.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.LOOK_AT_RESET, 9));
    this.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.LOOK_AT_PLAYER, 9));
    this.addOrUpdateCustomObjective(new ObjectiveDataEntry(ObjectiveType.LOOK_AT_MOB, 10));
  }

  default void defineCustomObjectiveData() {
    getEasyNPCServerData()
        .defineServerEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, new ObjectiveDataSet());
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_TARGETED_PLAYER_SET, new HashSet<>());
    getEasyNPCServerData().defineServerEntityData(CUSTOM_DATA_TARGETED_ENTITY_SET, new HashSet<>());
  }

  default void addAdditionalObjectiveData(CompoundTag compoundTag) {
    CompoundTag objectiveTag = new CompoundTag();

    if (this.isServerSide()) {
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
    if (this.getNPCDataVersion() == -1) {
      this.registerStandardObjectives();
    }
  }
}
