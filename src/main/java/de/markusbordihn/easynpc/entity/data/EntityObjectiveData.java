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
import de.markusbordihn.easynpc.entity.ai.goal.CustomLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.ai.goal.ResetLookAtPlayerGoal;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.FloatGoal;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.goal.OpenDoorGoal;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.entity.player.Player;

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
  String DATA_HAS_PLAYER_TARGET_TAG = "HasPlayerTarget";
  String DATA_HAS_ENTITY_TARGET_TAG = "HasEntityTarget";

  default ObjectiveDataSet getObjectiveDataSet() {
    return getCustomEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET);
  }

  default void setObjectiveDataSet(ObjectiveDataSet objectiveDataSet) {
    setCustomEntityData(CUSTOM_DATA_OBJECTIVE_DATA_SET, objectiveDataSet);
  }

  default boolean hasObjectives() {
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasObjectives();
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
    return getObjectiveDataSet() != null && getObjectiveDataSet().hasValidTarget();
  }

  default void onEasyNPCJoinUpdateObjective(EasyNPCEntity easyNPCEntity) {
    // Check if we need to re-register NPC based objectives.
    if (this.hasEntityTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget()
        && getObjectiveDataSet().isTargetedEntity(easyNPCEntity.getUUID())) {
      this.refreshEntityObjectives();
    }
  }

  default void onEasyNPCLeaveUpdateObjective(EasyNPCEntity easyNPCEntity) {
    // Check if we need to re-register NPC based objectives.
    if (this.hasEntityTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget()
        && getObjectiveDataSet().isTargetedEntity(easyNPCEntity.getUUID())) {
      this.getObjectiveDataSet().updateTargetedEntitySet(easyNPCEntity.getUUID());
    }
  }

  default void onPlayerJoinUpdateObjective(ServerPlayer serverPlayer) {
    // Check if we need to re-register owner and player based objectives.
    boolean needRefresh = false;
    if (this.hasOwnerTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget()
        && this.getEntity().isOwner(serverPlayer)) {
      log.info("Update owner based objectives for {} ...", this.getEntity());
      needRefresh = true;
    } else if (this.hasPlayerTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget()
        && this.getObjectiveDataSet().isTargetedPlayer(serverPlayer.getName().getString())) {
      needRefresh = true;
    }

    // Refresh objectives only once if needed.
    if (needRefresh) {
      this.refreshEntityObjectives();
    }
  }

  default void onPlayerLeaveUpdateObjective(ServerPlayer serverPlayer) {
    // Check if we need to re-register owner based objectives.
    if (this.hasOwnerTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget()
        && this.getEntity().isOwner(serverPlayer)) {
      this.getObjectiveDataSet().updateTargetedOwnerSet(serverPlayer);
    }

    // Check if we need to re-register player based objectives.
    if (this.hasPlayerTargetObjectives()
        && this.getObjectiveDataSet().hasValidTarget()
        && this.getObjectiveDataSet().isTargetedPlayer(serverPlayer.getName().getString())) {
      this.getObjectiveDataSet().updateTargetedPlayerSet(serverPlayer.getName().getString());
    }
  }

  default void onLivingEntityJoinUpdateObjective(LivingEntity livingEntity) {
    // Check if we need to re-register living entity based objectives.
    if (this.hasEntityTargetObjectives()
        && !this.getObjectiveDataSet().hasValidTarget()
        && this.getObjectiveDataSet().isTargetedEntity(livingEntity.getUUID())) {
      this.refreshEntityObjectives();
    }
  }

  default void onLivingEntityLeaveUpdateObjective(LivingEntity livingEntity) {
    // Check if we need to re-register living entity based objectives.
    if (this.hasObjectives()
        && this.getObjectiveDataSet().hasValidTarget()
        && this.getObjectiveDataSet().isTargetedEntity(livingEntity.getUUID())) {
      this.getObjectiveDataSet().updateTargetedEntitySet(livingEntity.getUUID());
    }
  }

  default void refreshEntityObjectives() {
    EasyNPCEntity entity = this.getEntity();
    GroundPathNavigation groundPathNavigation = getEntityGroundPathNavigation();

    log.debug("Refresh Entity Objectives {} for {} ...", getObjectiveDataSet(), entity);

    // Stop navigation and clean up old objectives.
    this.cleanEntityObjectives();

    // Register new objectives.
    this.registerStandardObjectives();
    if (this.hasObjectives()) {
      this.registerCustomObjectives();
    }

    // Refresh ground navigation for new objectives.
    if (groundPathNavigation != null) {
      entity.refreshGroundNavigation();
    }
  }

  default void cleanEntityObjectives() {
    EasyNPCEntity entity = this.getEntity();
    if (entity == null || entity.getLevel() == null || entity.getLevel().isClientSide()) {
      return;
    }
    GoalSelector goalSelector = this.getEntityGoalSelector();
    if (!goalSelector.getAvailableGoals().isEmpty()) {
      goalSelector.getAvailableGoals().clear();
    }
    GoalSelector targetSelector = this.getEntityTargetSelector();
    if (!targetSelector.getAvailableGoals().isEmpty()) {
      targetSelector.getAvailableGoals().clear();
    }
  }

  default void registerStandardObjectives() {
    EasyNPCEntity entity = this.getEntity();
    if (entity == null || entity.getLevel() == null || entity.getLevel().isClientSide()) {
      return;
    }
    GoalSelector goalSelector = this.getEntityGoalSelector();

    // Consider entity attributes when synced data are loaded.
    if (entity.synchedDataLoaded()) {
      log.info("Register standard objectives based on entity attributes for {}", entity);

      // Handle floating goals.
      if (entity.getAttributeCanFloat()) {
        goalSelector.addGoal(0, new FloatGoal(entity));
      }

      // Handle door interaction goals.
      if (entity.getAttributeCanCloseDoor()) {
        goalSelector.addGoal(8, new OpenDoorGoal(entity, true));
      }
      if (entity.getAttributeCanOpenDoor()) {
        goalSelector.addGoal(8, new OpenDoorGoal(entity, false));
      }
    } else {
      log.debug("Register standard objectives for {}", entity);
      goalSelector.addGoal(0, new FloatGoal(entity));
      goalSelector.addGoal(9, new ResetLookAtPlayerGoal(entity));
      goalSelector.addGoal(9, new CustomLookAtPlayerGoal(entity, Player.class, 15.0F, 1.0F));
      goalSelector.addGoal(10, new CustomLookAtPlayerGoal(entity, Mob.class, 15.0F));
    }
  }

  default void registerCustomObjectives() {
    EasyNPCEntity entity = this.getEntity();
    if (entity == null || entity.getLevel() == null || entity.getLevel().isClientSide()) {
      return;
    }
    Set<ObjectiveData> objectives = this.getObjectiveDataSet().getObjectives();
    if (objectives == null || objectives.isEmpty()) {
      return;
    }
    log.debug("Register custom objectives for {}", entity);
    GoalSelector goalSelector = this.getEntityGoalSelector();
    GoalSelector targetSelector = this.getEntityTargetSelector();
    for (ObjectiveData objectiveData : objectives) {
      // Skip if objective data is not valid
      if (objectiveData == null
          || objectiveData.getType() == null
          || objectiveData.getType() == ObjectiveType.NONE) {
        continue;
      }

      // Create goal
      Goal goal = objectiveData.getGoal(entity, entity.getEntityServerLevel());
      if (goal == null) {
        log.error("- Unable to register goal for {}!", entity);
        continue;
      }

      // Register objective
      int priority = objectiveData.getPriority();
      if (objectiveData.isGoalSelector()) {
        log.debug("- Register goal {} with priority {} for {}", goal, priority, entity);
        goalSelector.addGoal(priority, goal);
      } else if (objectiveData.isTargetSelector()) {
        log.debug("- Register target goal {} with priority {} for {}", goal, priority, entity);
        targetSelector.addGoal(priority, goal);
      } else {
        log.error("- Unable to register goal {} for {}!", goal, entity);
      }
    }
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

    // Store misc
    objectiveTag.putBoolean(DATA_HAS_OBJECTIVE_TAG, this.hasObjectives());
    if (this.hasObjectives()) {
      objectiveTag.putBoolean(DATA_HAS_PLAYER_TARGET_TAG, this.hasPlayerTargetObjectives());
      objectiveTag.putBoolean(DATA_HAS_ENTITY_TARGET_TAG, this.hasEntityTargetObjectives());
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
  }
}
