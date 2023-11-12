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
import de.markusbordihn.easynpc.entity.EntityManager;
import de.markusbordihn.easynpc.entity.ai.goal.FollowLivingEntityGoal;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.goal.WaterAvoidingRandomStrollGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.animal.IronGolem;
import net.minecraft.world.entity.monster.Pillager;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.raid.Raider;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveData {

  // Objective Data Tags
  public static final String DATA_ID_TAG = "Id";
  public static final String DATA_OBJECTIVE_DATA_TAG = "ObjectiveData";
  public static final String DATA_PRIORITY_TAG = "Prio";
  public static final String DATA_SPEED_TAG = "Speed";
  public static final String DATA_START_DISTANCE_TAG = "StartDistance";
  public static final String DATA_STOP_DISTANCE_TAG = "StopDistance";
  public static final String DATA_TARGET_ENTITY_UUID_TAG = "TargetEntityUUID";
  public static final String DATA_TARGET_PLAYER_NAME_TAG = "TargetPlayerName";
  public static final String DATA_TARGET_TAG = "Target";
  public static final String DATA_TYPE_TAG = "Type";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Objective Data
  private ObjectiveTarget objectiveTarget = ObjectiveTarget.NONE;
  private ObjectiveType objectiveType = ObjectiveType.NONE;
  private String id = UUID.randomUUID().toString();
  private String targetPlayerName;
  private UUID targetEntityUUID;
  private double speedModifier = 1.0D;
  private float startDistance = 10.0F;
  private float stopDistance = 2.0F;
  private int priority = 0;

  // Cache
  private boolean hasValidTarget = false;

  public ObjectiveData() {}

  public ObjectiveData(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public ObjectiveType getType() {
    return this.objectiveType;
  }

  public void setType(ObjectiveType objectiveType) {
    this.objectiveType = objectiveType;
  }

  public int getPriority() {
    return this.priority;
  }

  public void setPriority(int priority) {
    this.priority = priority;
  }

  public double getSpeedModifier() {
    return this.speedModifier;
  }

  public void setSpeedModifier(double speedModifier) {
    this.speedModifier = speedModifier;
  }

  public float getStartDistance() {
    return this.startDistance;
  }

  public void setStartDistance(float startDistance) {
    this.startDistance = startDistance;
  }

  public float getStopDistance() {
    return this.stopDistance;
  }

  public void setStopDistance(float stopDistance) {
    this.stopDistance = stopDistance;
  }

  public String getId() {
    return this.id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getTargetPlayerName() {
    return this.targetPlayerName;
  }

  public void setTargetPlayerName(String targetPlayerName) {
    this.targetPlayerName = targetPlayerName;
  }

  public ServerPlayer getTargetPlayer() {
    if (this.hasPlayerTarget()) {
      return EntityManager.getPlayerByName(this.targetPlayerName);
    }
    return null;
  }

  public UUID getTargetEntityUUID() {
    return this.targetEntityUUID;
  }

  public void setTargetEntityUUID(UUID targetEntityUUID) {
    this.targetEntityUUID = targetEntityUUID;
  }

  public LivingEntity getTargetEntity(ServerLevel serverLevel) {
    if (this.hasEntityTarget()) {
      return EntityManager.getLivingEntityByUUID(this.targetEntityUUID, serverLevel);
    }
    return null;
  }

  public Entity getTargetOwner(EasyNPCEntity easyNPCEntity, ServerLevel serverLevel) {
    if (this.hasOwnerTarget() && easyNPCEntity.getOwnerUUID() != null && serverLevel != null) {
      return EntityManager.getPlayerByUUID(easyNPCEntity.getOwnerUUID(), serverLevel);
    }
    return null;
  }

  public boolean hasOwnerTarget() {
    return this.getType() == ObjectiveType.FOLLOW_OWNER;
  }

  public boolean hasPlayerTarget() {
    return this.getType() == ObjectiveType.FOLLOW_PLAYER
        && this.targetPlayerName != null
        && !this.targetPlayerName.isEmpty();
  }

  public boolean hasEntityTarget() {
    return this.getType() == ObjectiveType.FOLLOW_ENTITY_BY_UUID && this.targetEntityUUID != null;
  }

  public boolean hasValidTarget() {
    return (hasOwnerTarget() || hasPlayerTarget() || hasEntityTarget()) && this.hasValidTarget;
  }

  public void setHasValidTarget(boolean hasValidTarget) {
    this.hasValidTarget = hasValidTarget;
  }

  public Goal getGoal(EasyNPCEntity easyNPCEntity, ServerLevel serverLevel) {
    switch (this.objectiveType) {
      case FOLLOW_PLAYER:
        ServerPlayer targetServerPlayer = getTargetPlayer();
        if (targetServerPlayer != null && !targetServerPlayer.isRemoved()) {
          this.hasValidTarget = true;
          return new FollowLivingEntityGoal(
              easyNPCEntity,
              targetServerPlayer,
              this.speedModifier,
              this.stopDistance,
              this.startDistance,
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          this.hasValidTarget = false;
          log.error("Unable to find player {} for {}!", this.targetPlayerName, this);
        }
        break;
      case FOLLOW_OWNER:
        Entity targetOwner = getTargetOwner(easyNPCEntity, serverLevel);
        if (targetOwner != null
            && targetOwner instanceof LivingEntity livingEntity
            && !livingEntity.isRemoved()) {
          this.hasValidTarget = true;
          return new FollowLivingEntityGoal(
              easyNPCEntity,
              livingEntity,
              this.speedModifier,
              this.stopDistance,
              this.startDistance,
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          this.hasValidTarget = false;
          log.error(
              "Unable to find valid owner {} for {} with {}!", targetOwner, easyNPCEntity, this);
        }
        break;
      case FOLLOW_ENTITY_BY_UUID:
        LivingEntity targetEntityMob = getTargetEntity(serverLevel);
        if (targetEntityMob != null && !targetEntityMob.isRemoved()) {
          this.hasValidTarget = true;
          return new FollowLivingEntityGoal(
              easyNPCEntity,
              targetEntityMob,
              this.speedModifier,
              this.stopDistance,
              this.startDistance,
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          this.hasValidTarget = false;
          log.error("Unable to find living entity {} for {}!", this.targetEntityUUID, this);
        }
        break;
      case RANDOM_STROLL:
        return new RandomStrollGoal(easyNPCEntity, this.speedModifier);
      case WATER_AVOIDING_RANDOM_STROLL:
        return new WaterAvoidingRandomStrollGoal(easyNPCEntity, this.speedModifier);
      default:
        return null;
    }
    return null;
  }

  public boolean isTargetSelector() {
    return objectiveType.isTargetSelector();
  }

  public boolean isTargetedPlayer(String playerName) {
    return this.hasPlayerTarget() && this.targetPlayerName.equals(playerName);
  }

  public boolean isTargetedEntity(UUID entityUUID) {
    return this.hasEntityTarget() && this.targetEntityUUID.equals(entityUUID);
  }

  public boolean isGoalSelector() {
    return objectiveType.isGoalSelector();
  }

  public ObjectiveTarget getTarget() {
    return this.objectiveTarget;
  }

  public void setTarget(ObjectiveTarget objectiveTarget) {
    this.objectiveTarget = objectiveTarget;
  }

  public Class<?> getTargetClass() {
    return getTargetClass(this.objectiveTarget);
  }

  public Class<?> getTargetClass(ObjectiveTarget objectiveTarget) {
    return switch (objectiveTarget) {
      case ANIMAL -> Animal.class;
      case PLAYER -> Player.class;
      case MOB -> Mob.class;
      case VILLAGER -> AbstractVillager.class;
      case IRON_GOLEM -> IronGolem.class;
      case PILLAGER -> Pillager.class;
      case RAIDER -> Raider.class;
      default -> null;
    };
  }

  public void load(CompoundTag compoundTag) {
    // Main data
    this.id = compoundTag.getString(DATA_ID_TAG);
    this.objectiveType = ObjectiveType.get(compoundTag.getString(DATA_TYPE_TAG));
    this.priority = compoundTag.getInt(DATA_PRIORITY_TAG);

    // Target class
    if (compoundTag.contains(DATA_TARGET_TAG)) {
      this.objectiveTarget = ObjectiveTarget.get(compoundTag.getString(DATA_TARGET_TAG));
    }

    // Additional parameters
    if (compoundTag.contains(DATA_SPEED_TAG)) {
      this.speedModifier = compoundTag.getDouble(DATA_SPEED_TAG);
    }
    if (compoundTag.contains(DATA_START_DISTANCE_TAG)) {
      this.startDistance = compoundTag.getFloat(DATA_START_DISTANCE_TAG);
    }
    if (compoundTag.contains(DATA_STOP_DISTANCE_TAG)) {
      this.stopDistance = compoundTag.getFloat(DATA_STOP_DISTANCE_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_ENTITY_UUID_TAG)) {
      this.targetEntityUUID = compoundTag.getUUID(DATA_TARGET_ENTITY_UUID_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_PLAYER_NAME_TAG)) {
      this.targetPlayerName = compoundTag.getString(DATA_TARGET_PLAYER_NAME_TAG);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    // Main data
    compoundTag.putString(DATA_ID_TAG, this.id);
    compoundTag.putString(DATA_TYPE_TAG, this.objectiveType.name());
    compoundTag.putInt(DATA_PRIORITY_TAG, this.priority);

    // Target class
    if (this.objectiveTarget != ObjectiveTarget.NONE) {
      compoundTag.putString(DATA_TARGET_TAG, this.objectiveTarget.name());
    }

    // Additional parameters
    if (this.speedModifier != 1.0D) {
      compoundTag.putDouble(DATA_SPEED_TAG, this.speedModifier);
    }
    if (this.startDistance != 10.0F) {
      compoundTag.putFloat(DATA_START_DISTANCE_TAG, this.startDistance);
    }
    if (this.stopDistance != 2.0F) {
      compoundTag.putFloat(DATA_STOP_DISTANCE_TAG, this.stopDistance);
    }
    if (this.targetEntityUUID != null) {
      compoundTag.putUUID(DATA_TARGET_ENTITY_UUID_TAG, this.targetEntityUUID);
    }
    if (this.targetPlayerName != null && !this.targetPlayerName.isEmpty()) {
      compoundTag.putString(DATA_TARGET_PLAYER_NAME_TAG, this.targetPlayerName);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "ObjectiveData [id="
        + this.id
        + ", type="
        + this.objectiveType
        + ", priority="
        + this.priority
        + ", target="
        + this.objectiveTarget
        + ", targetPlayerName="
        + this.targetPlayerName
        + ", targetEntityUUID="
        + this.targetEntityUUID
        + ", hasValidTarget="
        + this.hasValidTarget
        + ", speedModifier="
        + this.speedModifier
        + "]";
  }
}
