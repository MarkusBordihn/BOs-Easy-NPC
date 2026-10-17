/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import java.util.EnumSet;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.player.Player;

public class SitOnOwnerShoulderGoal extends Goal {

  public static final int MAX_TICKS_WITHOUT_PROGRESS = 100;
  private static final double DEFAULT_MAX_DISTANCE = 16.0D;
  // Matches the 100 ticks after which vanilla clears LivingEntity.lastHurtByMob.
  private static final int COMBAT_DURATION_TICKS = 100;
  private static final float MAX_SHOULDER_WIDTH = 0.8F;
  private static final float MAX_SHOULDER_HEIGHT = 1.0F;
  private static final int PATH_RECALCULATION_DELAY = 10;
  private static final double SHOULDER_REACH = 0.5D;
  private static final double SPEED_MODIFIER = 1.0D;

  private final EasyNPC<?> easyNPC;
  private final Mob mob;
  private final int cooldownTicks;
  private final double maxDistance;
  private final boolean onlyOutOfCombat;
  private Player owner;
  private int timeToRecalcPath;
  private double closestDistanceSqr;
  private long lastProgressGameTime;
  private long retryAllowedGameTime;

  public SitOnOwnerShoulderGoal(EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
    this.easyNPC = easyNPC;
    this.mob = easyNPC.getMob();
    this.cooldownTicks = objectiveDataEntry.getCooldownTicks();
    this.maxDistance =
        objectiveDataEntry.getStartDistance() > 0.0F
            ? objectiveDataEntry.getStartDistance()
            : DEFAULT_MAX_DISTANCE;
    this.onlyOutOfCombat = objectiveDataEntry.getOnlyOutOfCombat();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE, Goal.Flag.LOOK));
  }

  public static boolean fitsOnShoulder(Entity entity) {
    return entity.getBbWidth() <= MAX_SHOULDER_WIDTH && entity.getBbHeight() <= MAX_SHOULDER_HEIGHT;
  }

  private static boolean canCarryOnShoulder(Player player) {
    return player.isAlive()
        && !player.isSpectator()
        && !player.getAbilities().flying
        && !player.isSleeping()
        && !player.isPassenger()
        && !player.isInWater()
        && !player.isInPowderSnow
        && player.onGround()
        && (player.getShoulderEntityLeft().isEmpty() || player.getShoulderEntityRight().isEmpty());
  }

  private Player getOwner() {
    OwnerDataCapable<?> ownerData = this.easyNPC.getEasyNPCOwnerData();
    return ownerData != null && ownerData.getOwner() instanceof Player player ? player : null;
  }

  private boolean isMobReady() {
    return this.mob.isAlive()
        && this.mob.getTarget() == null
        && !this.mob.isLeashed()
        && !this.mob.isPassenger()
        && !this.mob.isVehicle()
        && fitsOnShoulder(this.mob);
  }

  private static boolean isInCombat(Player player) {
    return player.getLastHurtByMob() != null
        || (player.getLastHurtMob() != null
            && player.tickCount - player.getLastHurtMobTimestamp() < COMBAT_DURATION_TICKS);
  }

  private boolean canCarry(Player player) {
    return canCarryOnShoulder(player) && !(this.onlyOutOfCombat && isInCombat(player));
  }

  private boolean isWithinMaxDistance(Player player) {
    return this.mob.distanceToSqr(player) <= this.maxDistance * this.maxDistance;
  }

  private boolean isRemountAllowed() {
    long gameTime = this.mob.level().getGameTime();
    return gameTime >= this.retryAllowedGameTime
        && gameTime >= EasyNPCShoulderHandler.getRemountAllowedGameTime(this.easyNPC);
  }

  private boolean abortAndDelayRetry() {
    this.retryAllowedGameTime = this.mob.level().getGameTime() + this.cooldownTicks;
    return false;
  }

  @Override
  public boolean canUse() {
    if (this.mob.tickCount <= this.cooldownTicks
        || !this.isMobReady()
        || !this.isRemountAllowed()) {
      return false;
    }

    this.owner = this.getOwner();
    return this.owner != null && this.canCarry(this.owner) && this.isWithinMaxDistance(this.owner);
  }

  @Override
  public boolean canContinueToUse() {
    if (this.owner == null
        || this.owner.level() != this.mob.level()
        || !this.isMobReady()
        || !this.canCarry(this.owner)) {
      return false;
    }

    if (!this.isWithinMaxDistance(this.owner)
        || this.mob.level().getGameTime() - this.lastProgressGameTime
            >= MAX_TICKS_WITHOUT_PROGRESS) {
      return this.abortAndDelayRetry();
    }

    return true;
  }

  @Override
  public void start() {
    this.timeToRecalcPath = 0;
    this.closestDistanceSqr = this.mob.distanceToSqr(this.owner);
    this.lastProgressGameTime = this.mob.level().getGameTime();
  }

  @Override
  public void stop() {
    this.mob.getNavigation().stop();
    this.owner = null;
  }

  @Override
  public void tick() {
    double distanceSqr = this.mob.distanceToSqr(this.owner);
    if (distanceSqr < this.closestDistanceSqr) {
      this.closestDistanceSqr = distanceSqr;
      this.lastProgressGameTime = this.mob.level().getGameTime();
    }

    this.mob.getLookControl().setLookAt(this.owner, 10.0F, this.mob.getMaxHeadXRot());
    if (this.mob.getBoundingBox().intersects(this.owner.getBoundingBox().inflate(SHOULDER_REACH))) {
      EasyNPCShoulderHandler.sitOnShoulder(this.easyNPC, this.owner);
      return;
    }

    if (--this.timeToRecalcPath > 0) {
      return;
    }

    this.timeToRecalcPath = this.adjustedTickDelay(PATH_RECALCULATION_DELAY);
    this.mob.getNavigation().moveTo(this.owner, SPEED_MODIFIER);
  }
}
