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
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
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

public class ObjectiveDataEntry {

  // Objective Data Tags
  public static final String DATA_ID_TAG = "Id";
  public static final String DATA_PRIORITY_TAG = "Prio";
  public static final String DATA_SPEED_MODIFIER_TAG = "SpeedModifier";
  public static final String DATA_TARGET_OWNER_UUID_TAG = "TargetOwnerUUID";
  public static final String DATA_TARGET_ENTITY_UUID_TAG = "TargetEntityUUID";
  public static final String DATA_TARGET_PLAYER_NAME_TAG = "TargetPlayerName";
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_START_DISTANCE_TAG = "StartDistance";
  public static final String DATA_STOP_DISTANCE_TAG = "StopDistance";
  public static final String DATA_ONLY_AT_NIGHT_TAG = "OnlyAtNight";
  public static final String DATA_DISTANCE_TO_POI_TAG = "DistanceToPoi";
  public static final String DATA_CAN_DEAL_WITH_DOORS_TAG = "CanDealWithDoors";
  public static final String DATA_LOOK_DISTANCE_TAG = "LookDistance";
  public static final String DATA_ATTACK_INTERVAL_TAG = "AttackInterval";
  public static final String DATA_ATTACK_RADIUS_TAG = "AttackRadius";
  public static final String DATA_INTERVAL_TAG = "Interval";
  public static final String DATA_MUST_SEE_TARGET_TAG = "MustSeeTarget";
  public static final String DATA_PROBABILITY_TAG = "Probability";

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  // Objective Data
  private boolean mustSeeTarget = true;
  private int interval = 10;
  private ObjectiveType objectiveType = ObjectiveType.NONE;
  private String id = UUID.randomUUID().toString();
  private String targetPlayerName;
  private UUID targetOwnerUUID;
  private UUID targetEntityUUID;
  private double speedModifier = 0.7D;
  private float startDistance = 16.0F;
  private float stopDistance = 2.0F;
  private int attackInterval = 20;
  private float attackRadius = 8.0F;
  private boolean onlyAtNight = false;
  private int distanceToPoi = 16;
  private BooleanSupplier canDealWithDoors = () -> false;
  private int priority = 1;
  private float lookDistance = 15.0F;
  private float probability = 1.0F;

  // Cache
  private boolean isRegistered = false;
  private Goal goal = null;
  private Goal target = null;

  public ObjectiveDataEntry() {
  }

  public ObjectiveDataEntry(ObjectiveType objectiveType) {
    this.id = objectiveType.name();
    this.objectiveType = objectiveType;
  }

  public ObjectiveDataEntry(ObjectiveType objectiveType, int priority) {
    this(objectiveType);
    this.priority = priority;
  }

  public ObjectiveDataEntry(CompoundTag compoundTag) {
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

  public float getProbability() {
    return this.probability;
  }

  public void setProbability(float probability) {
    this.probability = probability;
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

  public boolean isMustSeeTarget() {
    return this.mustSeeTarget;
  }

  public void setMustSeeTarget(boolean mustSeeTarget) {
    this.mustSeeTarget = mustSeeTarget;
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

  public float getLookDistance() {
    return this.lookDistance;
  }

  public void setLookDistance(float lookDistance) {
    this.lookDistance = lookDistance;
  }

  public float getAttackRadius() {
    return this.attackRadius;
  }

  public void setAttackRadius(float attackRadius) {
    this.attackRadius = attackRadius;
  }

  public int getAttackInterval() {
    return this.attackInterval;
  }

  public void setAttackInterval(int attackInterval) {
    this.attackInterval = attackInterval;
  }

  public ServerPlayer getTargetPlayer() {
    if (this.hasPlayerTarget()) {
      return LivingEntityManager.getPlayerByName(this.targetPlayerName);
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

  public LivingEntity getTargetEntity(EasyNPC<?> easyNPC) {
    return this.getTargetEntity(easyNPC.getServerLevel());
  }

  public LivingEntity getTargetEntity(ServerLevel serverLevel) {
    if (this.hasEntityTarget() && serverLevel != null) {
      return LivingEntityManager.getLivingEntityByUUID(this.targetEntityUUID, serverLevel);
    }
    return null;
  }

  public Entity getTargetOwner(EasyNPC<?> easyNPC) {
    return this.getTargetOwner(easyNPC.getServerLevel());
  }

  public Entity getTargetOwner(ServerLevel serverLevel) {
    if (this.hasOwnerTarget() && this.targetOwnerUUID != null && serverLevel != null) {
      return LivingEntityManager.getPlayerByUUID(this.targetOwnerUUID, serverLevel);
    }
    return null;
  }

  public boolean hasTravelObjective() {
    return this.objectiveType.hasTravelObjective();
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

  public boolean hasValidTarget(EasyNPC<?> easyNPC) {
    ServerLevel serverLevel = easyNPC.getServerLevel();
    return serverLevel != null && this.hasValidTarget(serverLevel);
  }

  public boolean hasValidTarget(ServerLevel serverLevel) {
    // Assume valid targeting if no owner, player or entity target is set.
    if (!hasOwnerTarget() && !hasPlayerTarget() && !hasEntityTarget()) {
      return true;
    }

    // Check if we have still a valid target.
    if (hasOwnerTarget()) {
      ServerPlayer serverPlayer =
          LivingEntityManager.getPlayerByUUID(this.targetOwnerUUID, serverLevel);
      return serverPlayer != null && serverPlayer.isAlive();
    } else if (hasPlayerTarget()) {
      ServerPlayer serverPlayer = LivingEntityManager.getPlayerByName(this.targetPlayerName);
      return serverPlayer != null && serverPlayer.isAlive();
    } else if (hasEntityTarget()) {
      LivingEntity livingEntity =
          LivingEntityManager.getLivingEntityByUUID(this.targetEntityUUID, serverLevel);
      return livingEntity != null && livingEntity.isAlive();
    }

    return this.goal != null;
  }

  public Goal getGoal(EasyNPC<?> easyNPC) {
    if (this.goal == null) {
      this.goal = ObjectiveUtils.createObjectiveGoal(this, easyNPC);
    }
    return this.goal;
  }

  public Goal getTarget(EasyNPC<?> easyNPC) {
    if (this.target == null) {
      this.target = ObjectiveUtils.createObjectiveTarget(this, easyNPC);
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
    this.objectiveType = ObjectiveType.get(compoundTag.getString(DATA_TYPE_TAG));
    this.priority = compoundTag.getInt(DATA_PRIORITY_TAG);

    // Restore id, if no id is set, use the objective type.
    if (compoundTag.contains(DATA_ID_TAG) && !compoundTag.getString(DATA_ID_TAG).isEmpty()) {
      this.id = compoundTag.getString(DATA_ID_TAG);
    } else {
      this.id = this.objectiveType.name();
    }

    // Targeting parameters
    if (compoundTag.contains(DATA_TARGET_ENTITY_UUID_TAG)) {
      this.targetEntityUUID = compoundTag.getUUID(DATA_TARGET_ENTITY_UUID_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_PLAYER_NAME_TAG)) {
      this.targetPlayerName = compoundTag.getString(DATA_TARGET_PLAYER_NAME_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_OWNER_UUID_TAG)) {
      this.targetOwnerUUID = compoundTag.getUUID(DATA_TARGET_OWNER_UUID_TAG);
    }

    // Additional parameters
    if (compoundTag.contains(DATA_SPEED_MODIFIER_TAG)) {
      this.speedModifier = compoundTag.getDouble(DATA_SPEED_MODIFIER_TAG);
    }
    if (compoundTag.contains(DATA_START_DISTANCE_TAG)) {
      this.startDistance = compoundTag.getFloat(DATA_START_DISTANCE_TAG);
    }
    if (compoundTag.contains(DATA_STOP_DISTANCE_TAG)) {
      this.stopDistance = compoundTag.getFloat(DATA_STOP_DISTANCE_TAG);
    }
    if (compoundTag.contains(DATA_ONLY_AT_NIGHT_TAG)) {
      this.onlyAtNight = compoundTag.getBoolean(DATA_ONLY_AT_NIGHT_TAG);
    }
    if (compoundTag.contains(DATA_DISTANCE_TO_POI_TAG)) {
      this.distanceToPoi = compoundTag.getInt(DATA_DISTANCE_TO_POI_TAG);
    }
    if (compoundTag.contains(DATA_CAN_DEAL_WITH_DOORS_TAG)) {
      this.canDealWithDoors = () -> compoundTag.getBoolean(DATA_CAN_DEAL_WITH_DOORS_TAG);
    }
    if (compoundTag.contains(DATA_LOOK_DISTANCE_TAG)) {
      this.lookDistance = compoundTag.getFloat(DATA_LOOK_DISTANCE_TAG);
    }
    if (compoundTag.contains(DATA_ATTACK_INTERVAL_TAG)) {
      this.attackInterval = compoundTag.getInt(DATA_ATTACK_INTERVAL_TAG);
    }
    if (compoundTag.contains(DATA_ATTACK_RADIUS_TAG)) {
      this.attackRadius = compoundTag.getFloat(DATA_ATTACK_RADIUS_TAG);
    }
    if (compoundTag.contains(DATA_INTERVAL_TAG)) {
      this.interval = compoundTag.getInt(DATA_INTERVAL_TAG);
    }
    if (compoundTag.contains(DATA_MUST_SEE_TARGET_TAG)) {
      this.mustSeeTarget = compoundTag.getBoolean(DATA_MUST_SEE_TARGET_TAG);
    }
    if (compoundTag.contains(DATA_PROBABILITY_TAG)) {
      this.probability = compoundTag.getFloat(DATA_PROBABILITY_TAG);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_TYPE_TAG, this.objectiveType.name());
    compoundTag.putInt(DATA_PRIORITY_TAG, this.priority);

    // Store id only if it is not the same as the objective type.
    if (this.id != null && !this.id.isEmpty() && !this.id.equals(this.objectiveType.name())) {
      compoundTag.putString(DATA_ID_TAG, this.id);
    }

    // Targeting parameters
    if (this.targetEntityUUID != null) {
      compoundTag.putUUID(DATA_TARGET_ENTITY_UUID_TAG, this.targetEntityUUID);
    }
    if (this.targetPlayerName != null && !this.targetPlayerName.isEmpty()) {
      compoundTag.putString(DATA_TARGET_PLAYER_NAME_TAG, this.targetPlayerName);
    }
    if (this.targetOwnerUUID != null) {
      compoundTag.putUUID(DATA_TARGET_OWNER_UUID_TAG, this.targetOwnerUUID);
    }

    // Additional parameters
    if (this.speedModifier != 0.7D) {
      compoundTag.putDouble(DATA_SPEED_MODIFIER_TAG, this.speedModifier);
    }
    if (this.startDistance != 16.0F) {
      compoundTag.putFloat(DATA_START_DISTANCE_TAG, this.startDistance);
    }
    if (this.stopDistance != 2.0F) {
      compoundTag.putFloat(DATA_STOP_DISTANCE_TAG, this.stopDistance);
    }
    if (this.onlyAtNight) {
      compoundTag.putBoolean(DATA_ONLY_AT_NIGHT_TAG, this.onlyAtNight);
    }
    if (this.distanceToPoi != 16) {
      compoundTag.putInt(DATA_DISTANCE_TO_POI_TAG, this.distanceToPoi);
    }
    if (this.canDealWithDoors.getAsBoolean()) {
      compoundTag.putBoolean(DATA_CAN_DEAL_WITH_DOORS_TAG, this.canDealWithDoors.getAsBoolean());
    }
    if (this.lookDistance != 15.0F) {
      compoundTag.putFloat(DATA_LOOK_DISTANCE_TAG, this.lookDistance);
    }
    if (this.attackInterval != 20) {
      compoundTag.putInt(DATA_ATTACK_INTERVAL_TAG, this.attackInterval);
    }
    if (this.attackRadius != 8.0F) {
      compoundTag.putFloat(DATA_ATTACK_RADIUS_TAG, this.attackRadius);
    }
    if (this.interval != 10) {
      compoundTag.putInt(DATA_INTERVAL_TAG, this.interval);
    }
    if (!this.mustSeeTarget) {
      compoundTag.putBoolean(DATA_MUST_SEE_TARGET_TAG, this.mustSeeTarget);
    }
    if (this.probability != 1.0F) {
      compoundTag.putFloat(DATA_PROBABILITY_TAG, this.probability);
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
        + ", lookDistance="
        + this.lookDistance
        + ", attackInterval="
        + this.attackInterval
        + ", attackRadius="
        + this.attackRadius
        + ", interval="
        + this.interval
        + ", mustSeeTarget="
        + this.mustSeeTarget
        + ", probability="
        + this.probability
        + "]";
  }
}
