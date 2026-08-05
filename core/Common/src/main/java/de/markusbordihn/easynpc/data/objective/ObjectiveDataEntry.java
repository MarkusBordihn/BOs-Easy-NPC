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
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.UUID;
import java.util.function.BooleanSupplier;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ObjectiveDataEntry {

  public static final String DATA_ATTACK_INTERVAL_TAG = "AttackInterval";
  public static final String DATA_ATTACK_RADIUS_TAG = "AttackRadius";
  public static final String DATA_CAN_DEAL_WITH_DOORS_TAG = "CanDealWithDoors";
  public static final String DATA_DISTANCE_TO_POI_TAG = "DistanceToPoi";
  public static final String DATA_ID_TAG = "Id";
  public static final String DATA_INTERVAL_TAG = "Interval";
  public static final String DATA_LOOK_DISTANCE_TAG = "LookDistance";
  public static final String DATA_MUST_REACH_TARGET_TAG = "MustReachTarget";
  public static final String DATA_MUST_SEE_TARGET_TAG = "MustSeeTarget";
  public static final String DATA_ONLY_AT_NIGHT_TAG = "OnlyAtNight";
  public static final String DATA_PRIORITY_TAG = "Prio";
  public static final String DATA_PROBABILITY_TAG = "Probability";
  public static final String DATA_SPEED_MODIFIER_TAG = "SpeedModifier";
  public static final String DATA_START_DISTANCE_TAG = "StartDistance";
  public static final String DATA_STOP_DISTANCE_TAG = "StopDistance";
  public static final String DATA_TARGET_ENTITY_TAG_TAG = "TargetEntityTag";
  public static final String DATA_TARGET_ENTITY_UUID_TAG = "TargetEntityUUID";
  public static final String DATA_TARGET_OWNER_UUID_TAG = "TargetOwnerUUID";
  public static final String DATA_TARGET_PLAYER_NAME_TAG = "TargetPlayerName";
  public static final String DATA_TARGET_TEAM_NAME_TAG = "TargetTeamName";
  public static final String DATA_TARGET_ITEM_TAG = "TargetItemTag";
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_TELEPORT_DISTANCE_TAG = "TeleportDistance";
  public static final String DATA_FOLLOW_OFFSET_TAG = "FollowOffset";
  public static final String DATA_ONLY_WITHOUT_OWNER_TAG = "OnlyWithoutOwner";
  public static final String DATA_CAN_SCARE_TAG = "CanScare";
  public static final String DATA_CUSTOM_OBJECTIVE_ID_TAG = "CustomObjectiveId";
  public static final double DEFAULT_SPEED_MODIFIER = 0.7D;
  public static final float DEFAULT_ATTACK_RADIUS = 8.0F;
  public static final float DEFAULT_LOOK_DISTANCE = 15.0F;
  public static final float DEFAULT_PROBABILITY = 1.0F;
  public static final float DEFAULT_START_DISTANCE = 0.0F;
  public static final float DEFAULT_STOP_DISTANCE = 2.0F;
  public static final float DEFAULT_TELEPORT_DISTANCE = 12.0F;
  public static final int DEFAULT_ATTACK_INTERVAL = 20;
  public static final int DEFAULT_DISTANCE_TO_POI = 16;
  public static final int DEFAULT_INTERVAL = 10;
  public static final int DEFAULT_PRIORITY = 1;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private double speedModifier = DEFAULT_SPEED_MODIFIER;
  private float attackRadius = DEFAULT_ATTACK_RADIUS;
  private float lookDistance = DEFAULT_LOOK_DISTANCE;
  private float probability = DEFAULT_PROBABILITY;
  private float startDistance = DEFAULT_START_DISTANCE;
  private float stopDistance = DEFAULT_STOP_DISTANCE;
  private float teleportDistance = DEFAULT_TELEPORT_DISTANCE;
  private Vec3 followOffset = Vec3.ZERO;
  private int attackInterval = DEFAULT_ATTACK_INTERVAL;
  private int distanceToPoi = DEFAULT_DISTANCE_TO_POI;
  private int interval = DEFAULT_INTERVAL;
  private int priority = DEFAULT_PRIORITY;

  private ObjectiveType objectiveType = ObjectiveType.NONE;
  private String unresolvedType;
  private ResourceLocation customObjectiveId;
  private BooleanSupplier canDealWithDoors = () -> false;
  private boolean isRegistered = false;
  private boolean mustReachTarget = true;
  private boolean mustSeeTarget = true;
  private boolean onlyAtNight = false;
  private boolean onlyWithoutOwner = false;
  private boolean canScare = false;

  private Goal goal = null;
  private String id = UUID.randomUUID().toString();
  private Goal target = null;
  private UUID targetEntityUUID;
  private UUID targetOwnerUUID;
  private String targetPlayerName;
  private String targetTeamName;
  private String targetEntityTag;
  private String targetItemTag;
  private boolean unusableObjectiveLogged = false;

  public ObjectiveDataEntry() {}

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

  public ObjectiveDataEntry(ResourceLocation customObjectiveId) {
    this.objectiveType = ObjectiveType.CUSTOM;
    this.customObjectiveId = customObjectiveId;
    this.id =
        customObjectiveId != null ? customObjectiveId.toString() : ObjectiveType.CUSTOM.name();
    this.priority = ObjectiveRegistry.getDefaultPriority(customObjectiveId);
  }

  private static int clampToMinimum(String name, int value, int minimum) {
    if (value < minimum) {
      log.warn(
          "Objective value {}={} is below {}, using {} instead.", name, value, minimum, minimum);
      return minimum;
    }

    return value;
  }

  private static float clampToMinimum(String name, float value, float minimum) {
    if (value < minimum) {
      log.warn(
          "Objective value {}={} is below {}, using {} instead.", name, value, minimum, minimum);
      return minimum;
    }

    return value;
  }

  private static double clampToMinimum(String name, double value, double minimum) {
    if (value < minimum) {
      log.warn(
          "Objective value {}={} is below {}, using {} instead.", name, value, minimum, minimum);
      return minimum;
    }

    return value;
  }

  private static float clampToRange(String name, float value, float minimum, float maximum) {
    if (value < minimum || value > maximum) {
      float clampedValue = Math.min(maximum, Math.max(minimum, value));
      log.warn(
          "Objective value {}={} is outside of {}..{}, using {} instead.",
          name,
          value,
          minimum,
          maximum,
          clampedValue);
      return clampedValue;
    }

    return value;
  }

  public ResourceLocation getCustomObjectiveId() {
    return this.customObjectiveId;
  }

  public boolean isAwaitingRegistration() {
    return this.objectiveType == ObjectiveType.CUSTOM
        && !ObjectiveRegistry.isRegistered(this.customObjectiveId);
  }

  public ObjectiveType getType() {
    return this.objectiveType;
  }

  public boolean hasUnresolvedType() {
    return this.unresolvedType != null && !this.unresolvedType.isEmpty();
  }

  public String getTypeName() {
    return this.hasUnresolvedType() ? this.unresolvedType : this.objectiveType.name();
  }

  public int getPriority() {
    if (this.objectiveType == ObjectiveType.CUSTOM || this.objectiveType == ObjectiveType.NONE) {
      return this.priority;
    }
    return this.objectiveType.getDefaultPriority();
  }

  public ObjectiveDataEntry setPriority(int priority) {
    this.priority = clampToMinimum("priority", priority, 0);
    return this.invalidateGoals();
  }

  public double getSpeedModifier() {
    return this.speedModifier;
  }

  public ObjectiveDataEntry setSpeedModifier(double speedModifier) {
    this.speedModifier = clampToMinimum("speedModifier", speedModifier, 0.0D);
    return this.invalidateGoals();
  }

  /** Maximum distance for following, where anything up to zero means no limit. */
  public float getStartDistance() {
    return this.startDistance;
  }

  public ObjectiveDataEntry setStartDistance(float startDistance) {
    this.startDistance = clampToMinimum("startDistance", startDistance, 0.0F);
    return this.invalidateGoals();
  }

  public float getStopDistance() {
    return this.stopDistance;
  }

  public ObjectiveDataEntry setStopDistance(float stopDistance) {
    this.stopDistance = clampToMinimum("stopDistance", stopDistance, 0.0F);
    return this.invalidateGoals();
  }

  public float getTeleportDistance() {
    return this.teleportDistance;
  }

  public ObjectiveDataEntry setTeleportDistance(float teleportDistance) {
    this.teleportDistance = clampToMinimum("teleportDistance", teleportDistance, 0.0F);
    return this.invalidateGoals();
  }

  public Vec3 getFollowOffset() {
    return this.followOffset;
  }

  public ObjectiveDataEntry setFollowOffset(Vec3 followOffset) {
    this.followOffset = followOffset != null ? followOffset : Vec3.ZERO;
    return this.invalidateGoals();
  }

  public float getProbability() {
    return this.probability;
  }

  public ObjectiveDataEntry setProbability(float probability) {
    this.probability = clampToRange("probability", probability, 0.0F, 1.0F);
    return this.invalidateGoals();
  }

  public int getDistanceToPoi() {
    return this.distanceToPoi;
  }

  public ObjectiveDataEntry setDistanceToPoi(int distanceToPoi) {
    this.distanceToPoi = clampToMinimum("distanceToPoi", distanceToPoi, 1);
    return this.invalidateGoals();
  }

  public BooleanSupplier getCanDealWithDoors() {
    return this.canDealWithDoors;
  }

  public ObjectiveDataEntry setCanDealWithDoors(boolean canDealWithDoors) {
    this.canDealWithDoors = () -> canDealWithDoors;
    return this.invalidateGoals();
  }

  public boolean getOnlyAtNight() {
    return this.onlyAtNight;
  }

  public ObjectiveDataEntry setOnlyAtNight(boolean onlyAtNight) {
    this.onlyAtNight = onlyAtNight;
    return this.invalidateGoals();
  }

  public boolean getOnlyWithoutOwner() {
    return this.onlyWithoutOwner;
  }

  public ObjectiveDataEntry setOnlyWithoutOwner(boolean onlyWithoutOwner) {
    this.onlyWithoutOwner = onlyWithoutOwner;
    return this.invalidateGoals();
  }

  public boolean getCanScare() {
    return this.canScare;
  }

  public ObjectiveDataEntry setCanScare(boolean canScare) {
    this.canScare = canScare;
    return this.invalidateGoals();
  }

  public int getInterval() {
    return this.interval;
  }

  public ObjectiveDataEntry setInterval(int interval) {
    this.interval = clampToMinimum("interval", interval, 1);
    return this.invalidateGoals();
  }

  public boolean isMustSeeTarget() {
    return this.mustSeeTarget;
  }

  public ObjectiveDataEntry setMustSeeTarget(boolean mustSeeTarget) {
    this.mustSeeTarget = mustSeeTarget;
    return this.invalidateGoals();
  }

  public boolean isMustReachTarget() {
    return this.mustReachTarget;
  }

  public ObjectiveDataEntry setMustReachTarget(boolean mustReachTarget) {
    this.mustReachTarget = mustReachTarget;
    return this.invalidateGoals();
  }

  public String getId() {
    return this.id;
  }

  public boolean isRegistered() {
    return this.isRegistered;
  }

  public void setRegistered(boolean isRegistered) {
    this.isRegistered = isRegistered;
    if (!isRegistered) {
      this.goal = null;
      this.target = null;
      this.unusableObjectiveLogged = false;
    }
  }

  public boolean markUnusableObjectiveLogged() {
    if (this.unusableObjectiveLogged) {
      return false;
    }

    this.unusableObjectiveLogged = true;
    return true;
  }

  public String getTargetPlayerName() {
    return this.targetPlayerName;
  }

  public void setTargetPlayerName(String targetPlayerName) {
    this.targetPlayerName = targetPlayerName;
  }

  public String getTargetTeamName() {
    return this.targetTeamName;
  }

  public void setTargetTeamName(String targetTeamName) {
    this.targetTeamName = targetTeamName;
  }

  public String getTargetEntityTag() {
    return this.targetEntityTag;
  }

  public void setTargetEntityTag(String targetEntityTag) {
    this.targetEntityTag = targetEntityTag;
  }

  public float getLookDistance() {
    return this.lookDistance;
  }

  public ObjectiveDataEntry setLookDistance(float lookDistance) {
    this.lookDistance = clampToMinimum("lookDistance", lookDistance, 0.0F);
    return this.invalidateGoals();
  }

  public float getAttackRadius() {
    return this.attackRadius;
  }

  public ObjectiveDataEntry setAttackRadius(float attackRadius) {
    this.attackRadius = clampToMinimum("attackRadius", attackRadius, 0.0F);
    return this.invalidateGoals();
  }

  public int getAttackInterval() {
    return this.attackInterval;
  }

  public ObjectiveDataEntry setAttackInterval(int attackInterval) {
    this.attackInterval = clampToMinimum("attackInterval", attackInterval, 1);
    return this.invalidateGoals();
  }

  private ObjectiveDataEntry invalidateGoals() {
    this.setRegistered(false);
    return this;
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

  public String getTargetItemTag() {
    return this.targetItemTag;
  }

  public void setTargetItemTag(String targetItemTag) {
    this.targetItemTag = targetItemTag;
  }

  public LivingEntity getTargetEntity(EasyNPC<?> easyNPC) {
    return this.getTargetEntity(easyNPC.getEntityServerLevel());
  }

  public LivingEntity getTargetEntity(ServerLevel serverLevel) {
    if (this.hasEntityTarget() && serverLevel != null) {
      return LivingEntityManager.getLivingEntityByUUID(this.targetEntityUUID, serverLevel);
    }
    return null;
  }

  public Entity getTargetOwner(EasyNPC<?> easyNPC) {
    UUID ownerUUID = this.resolveTargetOwnerUUID(easyNPC);
    ServerLevel serverLevel = easyNPC != null ? easyNPC.getEntityServerLevel() : null;
    if (ownerUUID == null || serverLevel == null) {
      return null;
    }

    return LivingEntityManager.getPlayerByUUID(ownerUUID, serverLevel);
  }

  private UUID resolveTargetOwnerUUID(EasyNPC<?> easyNPC) {
    if (!this.hasOwnerTarget()) {
      return null;
    }

    if (this.targetOwnerUUID != null) {
      return this.targetOwnerUUID;
    }

    OwnerDataCapable<?> ownerData = easyNPC != null ? easyNPC.getEasyNPCOwnerData() : null;
    return ownerData != null ? ownerData.getOwnerUUID() : null;
  }

  public boolean hasTravelObjective() {
    if (this.objectiveType == ObjectiveType.CUSTOM) {
      ObjectiveGoalFactory goalFactory = ObjectiveRegistry.get(this.customObjectiveId);
      return goalFactory == null || goalFactory.hasTravelObjective();
    }

    return this.objectiveType.hasTravelObjective();
  }

  public boolean hasOwnerTarget() {
    return ObjectiveGroup.OWNER_TARGET.contains(this.getType());
  }

  public boolean hasPlayerTarget() {
    return ObjectiveGroup.PLAYER_TARGET.contains(this.getType())
        && this.targetPlayerName != null
        && !this.targetPlayerName.isEmpty();
  }

  public boolean hasEntityTarget() {
    return ObjectiveGroup.ENTITY_TARGET.contains(this.getType()) && this.targetEntityUUID != null;
  }

  public boolean hasValidTarget(EasyNPC<?> easyNPC) {
    ServerLevel serverLevel = easyNPC != null ? easyNPC.getEntityServerLevel() : null;
    if (serverLevel == null) {
      return false;
    }

    if (!hasOwnerTarget() && !hasPlayerTarget() && !hasEntityTarget()) {
      return true;
    }

    if (hasOwnerTarget()) {
      UUID ownerUUID = this.resolveTargetOwnerUUID(easyNPC);
      if (ownerUUID == null) {
        return false;
      }

      ServerPlayer serverPlayer = LivingEntityManager.getPlayerByUUID(ownerUUID, serverLevel);
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

  public void load(CompoundTag compoundTag) {
    String storedType = compoundTag.getString(DATA_TYPE_TAG);
    this.objectiveType = ObjectiveType.get(storedType);
    this.unresolvedType =
        this.objectiveType == ObjectiveType.NONE
                && storedType != null
                && !storedType.isEmpty()
                && !storedType.equalsIgnoreCase(ObjectiveType.NONE.name())
            ? storedType
            : null;
    if (compoundTag.contains(DATA_CUSTOM_OBJECTIVE_ID_TAG)) {
      this.customObjectiveId =
          ResourceLocation.tryParse(compoundTag.getString(DATA_CUSTOM_OBJECTIVE_ID_TAG));
    }
    this.setPriority(compoundTag.getInt(DATA_PRIORITY_TAG));

    if (compoundTag.contains(DATA_ID_TAG) && !compoundTag.getString(DATA_ID_TAG).isEmpty()) {
      this.id = compoundTag.getString(DATA_ID_TAG);
    } else {
      this.id =
          this.customObjectiveId != null ? this.customObjectiveId.toString() : this.getTypeName();
    }

    if (compoundTag.contains(DATA_TARGET_ENTITY_UUID_TAG)) {
      this.targetEntityUUID = compoundTag.getUUID(DATA_TARGET_ENTITY_UUID_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_PLAYER_NAME_TAG)) {
      this.targetPlayerName = compoundTag.getString(DATA_TARGET_PLAYER_NAME_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_TEAM_NAME_TAG)) {
      this.targetTeamName = compoundTag.getString(DATA_TARGET_TEAM_NAME_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_ENTITY_TAG_TAG)) {
      this.targetEntityTag = compoundTag.getString(DATA_TARGET_ENTITY_TAG_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_OWNER_UUID_TAG)) {
      this.targetOwnerUUID = compoundTag.getUUID(DATA_TARGET_OWNER_UUID_TAG);
    }
    if (compoundTag.contains(DATA_TARGET_ITEM_TAG)) {
      this.targetItemTag = compoundTag.getString(DATA_TARGET_ITEM_TAG);
    }

    if (compoundTag.contains(DATA_SPEED_MODIFIER_TAG)) {
      this.setSpeedModifier(compoundTag.getDouble(DATA_SPEED_MODIFIER_TAG));
    }
    if (compoundTag.contains(DATA_START_DISTANCE_TAG)) {
      this.setStartDistance(compoundTag.getFloat(DATA_START_DISTANCE_TAG));
    }
    if (compoundTag.contains(DATA_STOP_DISTANCE_TAG)) {
      this.setStopDistance(compoundTag.getFloat(DATA_STOP_DISTANCE_TAG));
    }
    if (compoundTag.contains(DATA_TELEPORT_DISTANCE_TAG)) {
      this.setTeleportDistance(compoundTag.getFloat(DATA_TELEPORT_DISTANCE_TAG));
    }
    if (compoundTag.contains(DATA_FOLLOW_OFFSET_TAG, Tag.TAG_LIST)) {
      ListTag followOffsetTag = compoundTag.getList(DATA_FOLLOW_OFFSET_TAG, Tag.TAG_DOUBLE);
      if (followOffsetTag.size() == 3) {
        this.followOffset =
            new Vec3(
                followOffsetTag.getDouble(0),
                followOffsetTag.getDouble(1),
                followOffsetTag.getDouble(2));
      }
    }
    if (compoundTag.contains(DATA_ONLY_AT_NIGHT_TAG)) {
      this.onlyAtNight = compoundTag.getBoolean(DATA_ONLY_AT_NIGHT_TAG);
    }
    if (compoundTag.contains(DATA_ONLY_WITHOUT_OWNER_TAG)) {
      this.onlyWithoutOwner = compoundTag.getBoolean(DATA_ONLY_WITHOUT_OWNER_TAG);
    }
    if (compoundTag.contains(DATA_CAN_SCARE_TAG)) {
      this.canScare = compoundTag.getBoolean(DATA_CAN_SCARE_TAG);
    }
    if (compoundTag.contains(DATA_DISTANCE_TO_POI_TAG)) {
      this.setDistanceToPoi(compoundTag.getInt(DATA_DISTANCE_TO_POI_TAG));
    }
    if (compoundTag.contains(DATA_CAN_DEAL_WITH_DOORS_TAG)) {
      this.setCanDealWithDoors(compoundTag.getBoolean(DATA_CAN_DEAL_WITH_DOORS_TAG));
    }
    if (compoundTag.contains(DATA_LOOK_DISTANCE_TAG)) {
      this.setLookDistance(compoundTag.getFloat(DATA_LOOK_DISTANCE_TAG));
    }
    if (compoundTag.contains(DATA_ATTACK_INTERVAL_TAG)) {
      this.setAttackInterval(compoundTag.getInt(DATA_ATTACK_INTERVAL_TAG));
    }
    if (compoundTag.contains(DATA_ATTACK_RADIUS_TAG)) {
      this.setAttackRadius(compoundTag.getFloat(DATA_ATTACK_RADIUS_TAG));
    }
    if (compoundTag.contains(DATA_INTERVAL_TAG)) {
      this.setInterval(compoundTag.getInt(DATA_INTERVAL_TAG));
    }
    if (compoundTag.contains(DATA_MUST_SEE_TARGET_TAG)) {
      this.mustSeeTarget = compoundTag.getBoolean(DATA_MUST_SEE_TARGET_TAG);
    }
    if (compoundTag.contains(DATA_MUST_REACH_TARGET_TAG)) {
      this.mustReachTarget = compoundTag.getBoolean(DATA_MUST_REACH_TARGET_TAG);
    }
    if (compoundTag.contains(DATA_PROBABILITY_TAG)) {
      this.setProbability(compoundTag.getFloat(DATA_PROBABILITY_TAG));
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    String typeName = this.getTypeName();
    compoundTag.putString(DATA_TYPE_TAG, typeName);
    if (this.customObjectiveId != null) {
      compoundTag.putString(DATA_CUSTOM_OBJECTIVE_ID_TAG, this.customObjectiveId.toString());
    }
    if (this.objectiveType == ObjectiveType.CUSTOM || this.objectiveType == ObjectiveType.NONE) {
      compoundTag.putInt(DATA_PRIORITY_TAG, this.priority);
    }

    if (this.id != null && !this.id.isEmpty() && !this.id.equalsIgnoreCase(typeName)) {
      compoundTag.putString(DATA_ID_TAG, this.id);
    }

    if (this.targetEntityUUID != null) {
      compoundTag.putUUID(DATA_TARGET_ENTITY_UUID_TAG, this.targetEntityUUID);
    }
    if (this.targetPlayerName != null && !this.targetPlayerName.isEmpty()) {
      compoundTag.putString(DATA_TARGET_PLAYER_NAME_TAG, this.targetPlayerName);
    }
    if (this.targetTeamName != null && !this.targetTeamName.isEmpty()) {
      compoundTag.putString(DATA_TARGET_TEAM_NAME_TAG, this.targetTeamName);
    }
    if (this.targetEntityTag != null && !this.targetEntityTag.isEmpty()) {
      compoundTag.putString(DATA_TARGET_ENTITY_TAG_TAG, this.targetEntityTag);
    }
    if (this.targetOwnerUUID != null) {
      compoundTag.putUUID(DATA_TARGET_OWNER_UUID_TAG, this.targetOwnerUUID);
    }
    if (this.targetItemTag != null && !this.targetItemTag.isEmpty()) {
      compoundTag.putString(DATA_TARGET_ITEM_TAG, this.targetItemTag);
    }

    if (this.speedModifier != DEFAULT_SPEED_MODIFIER) {
      compoundTag.putDouble(DATA_SPEED_MODIFIER_TAG, this.speedModifier);
    }
    if (this.startDistance != DEFAULT_START_DISTANCE) {
      compoundTag.putFloat(DATA_START_DISTANCE_TAG, this.startDistance);
    }
    if (this.stopDistance != DEFAULT_STOP_DISTANCE) {
      compoundTag.putFloat(DATA_STOP_DISTANCE_TAG, this.stopDistance);
    }
    if (this.teleportDistance != DEFAULT_TELEPORT_DISTANCE) {
      compoundTag.putFloat(DATA_TELEPORT_DISTANCE_TAG, this.teleportDistance);
    }
    if (this.followOffset.lengthSqr() != 0.0D) {
      ListTag followOffsetTag = new ListTag();
      followOffsetTag.add(DoubleTag.valueOf(this.followOffset.x));
      followOffsetTag.add(DoubleTag.valueOf(this.followOffset.y));
      followOffsetTag.add(DoubleTag.valueOf(this.followOffset.z));
      compoundTag.put(DATA_FOLLOW_OFFSET_TAG, followOffsetTag);
    }
    if (this.onlyAtNight) {
      compoundTag.putBoolean(DATA_ONLY_AT_NIGHT_TAG, true);
    }
    if (this.onlyWithoutOwner) {
      compoundTag.putBoolean(DATA_ONLY_WITHOUT_OWNER_TAG, true);
    }
    if (this.canScare) {
      compoundTag.putBoolean(DATA_CAN_SCARE_TAG, true);
    }
    if (this.distanceToPoi != DEFAULT_DISTANCE_TO_POI) {
      compoundTag.putInt(DATA_DISTANCE_TO_POI_TAG, this.distanceToPoi);
    }
    if (this.canDealWithDoors.getAsBoolean()) {
      compoundTag.putBoolean(DATA_CAN_DEAL_WITH_DOORS_TAG, true);
    }
    if (this.lookDistance != DEFAULT_LOOK_DISTANCE) {
      compoundTag.putFloat(DATA_LOOK_DISTANCE_TAG, this.lookDistance);
    }
    if (this.attackInterval != DEFAULT_ATTACK_INTERVAL) {
      compoundTag.putInt(DATA_ATTACK_INTERVAL_TAG, this.attackInterval);
    }
    if (this.attackRadius != DEFAULT_ATTACK_RADIUS) {
      compoundTag.putFloat(DATA_ATTACK_RADIUS_TAG, this.attackRadius);
    }
    if (this.interval != DEFAULT_INTERVAL) {
      compoundTag.putInt(DATA_INTERVAL_TAG, this.interval);
    }
    if (!this.mustSeeTarget) {
      compoundTag.putBoolean(DATA_MUST_SEE_TARGET_TAG, false);
    }
    if (!this.mustReachTarget) {
      compoundTag.putBoolean(DATA_MUST_REACH_TARGET_TAG, false);
    }
    if (this.probability != DEFAULT_PROBABILITY) {
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
        + ", targetTeamName="
        + this.targetTeamName
        + ", targetEntityTag="
        + this.targetEntityTag
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
