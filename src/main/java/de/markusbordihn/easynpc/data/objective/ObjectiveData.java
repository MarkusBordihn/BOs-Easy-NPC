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
import java.util.UUID;
import java.util.function.BooleanSupplier;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveData {
  // Objective Data Tags
  public static final String DATA_ID_TAG = "Id";
  public static final String DATA_PRIORITY_TAG = "Prio";
  public static final String DATA_SPEED_TAG = "Speed";
  public static final String DATA_START_DISTANCE_TAG = "StartDistance";
  public static final String DATA_STOP_DISTANCE_TAG = "StopDistance";
  public static final String DATA_TARGET_OWNER_UUID_TAG = "TargetOwnerUUID";
  public static final String DATA_TARGET_ENTITY_UUID_TAG = "TargetEntityUUID";
  public static final String DATA_TARGET_PLAYER_NAME_TAG = "TargetPlayerName";
  public static final String DATA_TYPE_TAG = "Type";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Objective Data
  private boolean mustSee = true;
  private boolean followingTargetEvenIfNotSeen = false;
  private int interval = 10;
  private ObjectiveType objectiveType = ObjectiveType.NONE;
  private String id = UUID.randomUUID().toString();
  private String targetPlayerName;
  private UUID targetOwnerUUID;
  private UUID targetEntityUUID;
  private double speedModifier = 1.0D;
  private float startDistance = 10.0F;
  private float stopDistance = 2.0F;
  private boolean onlyAtNight = false;
  private int distanceToPoi = 16;
  private BooleanSupplier canDealWithDoors = () -> false;
  private int priority = 1;

  // Cache
  private boolean isRegistered = false;
  private Goal goal = null;
  private Goal target = null;

  public ObjectiveData() {}

  public ObjectiveData(ObjectiveType objectiveType) {
    this.id = objectiveType.name();
    this.objectiveType = objectiveType;
  }

  public ObjectiveData(ObjectiveType objectiveType, int priority) {
    this(objectiveType);
    this.priority = priority;
  }

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

  public boolean isOnlyAtNight() {
    return this.onlyAtNight;
  }

  public int getDistanceToPoi() {
    return this.distanceToPoi;
  }

  public void setDistanceToPoi(int distanceToPoi) {
    this.distanceToPoi = distanceToPoi;
  }

  public BooleanSupplier getCanDealWithDoors() {
    return this.canDealWithDoors;
  }

  public void setCanDealWithDoors(BooleanSupplier canDealWithDoors) {
    this.canDealWithDoors = canDealWithDoors;
  }

  public boolean getOnlyAtNight() {
    return this.onlyAtNight;
  }

  public void setOnlyAtNight(boolean onlyAtNight) {
    this.onlyAtNight = onlyAtNight;
  }

  public int getInterval() {
    return this.interval;
  }

  public void setInterval(int interval) {
    this.interval = interval;
  }

  public boolean isFollowingTargetEvenIfNotSeen() {
    return this.followingTargetEvenIfNotSeen;
  }

  public void setFollowingTargetEvenIfNotSeen(boolean followingTargetEvenIfNotSeen) {
    this.followingTargetEvenIfNotSeen = followingTargetEvenIfNotSeen;
  }

  public boolean isMustSee() {
    return this.mustSee;
  }

  public void setMustSee(boolean mustSee) {
    this.mustSee = mustSee;
  }

  public String getId() {
    return this.id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setId(ObjectiveType objectiveType) {
    this.id = objectiveType.name();
  }

  public boolean isRegistered() {
    return this.isRegistered;
  }

  public void setRegistered(boolean isRegistered) {
    this.isRegistered = isRegistered;
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

  public UUID getTargetOwnerUUID() {
    return this.targetOwnerUUID;
  }

  public void setTargetOwnerUUID(UUID targetOwnerUUID) {
    this.targetOwnerUUID = targetOwnerUUID;
  }

  public UUID getTargetEntityUUID() {
    return this.targetEntityUUID;
  }

  public void setTargetEntityUUID(UUID targetEntityUUID) {
    this.targetEntityUUID = targetEntityUUID;
  }

  public LivingEntity getTargetEntity(EasyNPCEntity easyNPCEntity) {
    return this.getTargetEntity(easyNPCEntity.getEntityServerLevel());
  }

  public LivingEntity getTargetEntity(ServerLevel serverLevel) {
    if (this.hasEntityTarget() && serverLevel != null) {
      return EntityManager.getLivingEntityByUUID(this.targetEntityUUID, serverLevel);
    }
    return null;
  }

  public Entity getTargetOwner(EasyNPCEntity easyNPCEntity) {
    return this.getTargetOwner(easyNPCEntity, easyNPCEntity.getEntityServerLevel());
  }

  public Entity getTargetOwner(EasyNPCEntity easyNPCEntity, ServerLevel serverLevel) {
    if (this.hasOwnerTarget() && this.targetOwnerUUID != null && serverLevel != null) {
      return EntityManager.getPlayerByUUID(this.targetOwnerUUID, serverLevel);
    }
    return null;
  }

  public boolean hasOwnerTarget() {
    return this.getType() == ObjectiveType.FOLLOW_OWNER && this.targetOwnerUUID != null;
  }

  public boolean hasPlayerTarget() {
    return this.getType() == ObjectiveType.FOLLOW_PLAYER
        && this.targetPlayerName != null
        && !this.targetPlayerName.isEmpty();
  }

  public boolean hasEntityTarget() {
    return this.getType() == ObjectiveType.FOLLOW_ENTITY_BY_UUID && this.targetEntityUUID != null;
  }

  public boolean hasValidTarget(EasyNPCEntity easyNPCEntity) {
    ServerLevel serverLevel = easyNPCEntity.getEntityServerLevel();
    return serverLevel != null && this.hasValidTarget(serverLevel);
  }

  public boolean hasValidTarget(ServerLevel serverLevel) {
    // Assume valid targeting if no owner, player or entity target is set.
    if (!hasOwnerTarget() && !hasPlayerTarget() && !hasEntityTarget()) {
      return true;
    }

    // Check if we have still a valid target.
    if (hasOwnerTarget()) {
      ServerPlayer serverPlayer = EntityManager.getPlayerByUUID(this.targetOwnerUUID, serverLevel);
      return serverPlayer != null && serverPlayer.isAlive();
    } else if (hasPlayerTarget()) {
      ServerPlayer serverPlayer = EntityManager.getPlayerByName(this.targetPlayerName);
      return serverPlayer != null && serverPlayer.isAlive();
    } else if (hasEntityTarget()) {
      LivingEntity livingEntity =
          EntityManager.getLivingEntityByUUID(this.targetEntityUUID, serverLevel);
      return livingEntity != null && livingEntity.isAlive();
    }

    return this.goal != null;
  }

  public Goal getGoal(EasyNPCEntity easyNPCEntity) {
    if (this.goal == null) {
      this.goal = ObjectiveUtils.createObjectiveGoal(this, easyNPCEntity);
    }
    return this.goal;
  }

  public Goal getTarget(EasyNPCEntity easyNPCEntity) {
    if (this.target == null) {
      this.target = ObjectiveUtils.createObjectiveTarget(this, easyNPCEntity);
    }
    return this.target;
  }

  public boolean isTargetedPlayer(String playerName) {
    return this.hasPlayerTarget() && this.targetPlayerName.equals(playerName);
  }

  public boolean isTargetedEntity(UUID entityUUID) {
    return this.hasEntityTarget() && this.targetEntityUUID.equals(entityUUID);
  }

  public void load(CompoundTag compoundTag) {
    // Main data
    this.id = compoundTag.getString(DATA_ID_TAG);
    this.objectiveType = ObjectiveType.get(compoundTag.getString(DATA_TYPE_TAG));
    this.priority = compoundTag.getInt(DATA_PRIORITY_TAG);

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
    if (compoundTag.contains(DATA_TARGET_OWNER_UUID_TAG)) {
      this.targetOwnerUUID = compoundTag.getUUID(DATA_TARGET_OWNER_UUID_TAG);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    // Main data
    compoundTag.putString(DATA_ID_TAG, this.id);
    compoundTag.putString(DATA_TYPE_TAG, this.objectiveType.name());
    compoundTag.putInt(DATA_PRIORITY_TAG, this.priority);

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
    if (this.targetOwnerUUID != null) {
      compoundTag.putUUID(DATA_TARGET_OWNER_UUID_TAG, this.targetOwnerUUID);
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
        + ", targetPlayerName="
        + this.targetPlayerName
        + ", targetEntityUUID="
        + this.targetEntityUUID
        + ", targetOwnerUUID="
        + this.targetOwnerUUID
        + ", isRegistered="
        + this.isRegistered
        + ", speedModifier="
        + this.speedModifier
        + ", startDistance="
        + this.startDistance
        + ", stopDistance="
        + this.stopDistance
        + ", onlyAtNight="
        + this.onlyAtNight
        + ", distanceToPoi="
        + this.distanceToPoi
        + ", canDealWithDoors="
        + this.canDealWithDoors
        + "]";
  }
}
