/*
 * Copyright 2024 Markus Bordihn
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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.control.JumpEasyNPCMoveControl;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import java.util.EnumSet;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.level.pathfinder.Path;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MoveToPositionGoal<T extends EasyNPC<?>> extends Goal {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Move To Position Goal]";
  private static final int PATHFINDING_RANGE = 48;
  private static final int DEFAULT_TIMEOUT_TICKS = 200;
  private static final float DEFAULT_ARRIVAL_RADIUS = 1.5F;
  private static final float MIN_VERTICAL_ARRIVAL_DISTANCE = 2.0F;
  private static final int REPATH_INTERVAL_TICKS = 20;
  private static final int TICKS_WITHOUT_PROGRESS_LIMIT = 60;
  private static final double PROGRESS_DISTANCE = 0.5D;

  private final NavigationDataCapable<?> navigationData;
  private final Mob mob;
  private final BlockPos targetPos;
  private final double speedModifier;
  private final float arrivalRadius;
  private final int timeoutTicks;
  private final boolean teleportOnTimeout;
  private final Runnable onArrival;
  private final boolean canJump;
  private BlockPos navigationTargetPos;
  private Vec3 lastProgressPosition;
  private boolean jumpControlled;
  private int ticksRunning;
  private int ticksSincePathUpdate;
  private int ticksWithoutProgress;
  private double closestDistanceToTarget;
  private boolean arrived;

  public MoveToPositionGoal(
      T easyNPC, BlockPos targetPos, double speedModifier, Runnable onArrival) {
    this(
        easyNPC,
        targetPos,
        speedModifier,
        DEFAULT_ARRIVAL_RADIUS,
        DEFAULT_TIMEOUT_TICKS,
        true,
        onArrival);
  }

  public MoveToPositionGoal(
      T easyNPC,
      BlockPos targetPos,
      double speedModifier,
      float arrivalRadius,
      int timeoutTicks,
      boolean teleportOnTimeout,
      Runnable onArrival) {
    this.navigationData = easyNPC.getEasyNPCNavigationData();
    this.mob = easyNPC.getMob();
    this.targetPos = targetPos;
    this.speedModifier = speedModifier;
    this.arrivalRadius = arrivalRadius;
    this.timeoutTicks = timeoutTicks;
    this.teleportOnTimeout = teleportOnTimeout;
    this.onArrival = onArrival;
    this.canJump = this.navigationData != null && this.navigationData.canJump();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE));
  }

  @Override
  public boolean canUse() {
    return this.targetPos != null && !this.arrived;
  }

  @Override
  public boolean canContinueToUse() {
    return !this.arrived && !reachedTarget() && this.ticksRunning < this.timeoutTicks;
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return true;
  }

  @Override
  public void start() {
    this.ticksRunning = 0;
    this.ticksSincePathUpdate = 0;
    this.ticksWithoutProgress = 0;
    this.closestDistanceToTarget = Double.MAX_VALUE;
    this.lastProgressPosition = this.mob.position();
    this.arrived = false;
    this.navigationTargetPos = GroundTargetResolver.resolveGroundTarget(this.mob, this.targetPos);
    this.jumpControlled =
        this.canJump && this.mob.getMoveControl() instanceof JumpEasyNPCMoveControl;

    if (reachedTarget()) {
      arrive();
      return;
    }

    double distance = this.mob.position().distanceTo(Vec3.atCenterOf(this.navigationTargetPos));
    if (this.teleportOnTimeout && distance > PATHFINDING_RANGE) {
      teleportAndArrive();
      return;
    }

    if (this.jumpControlled
        && this.mob.getMoveControl() instanceof JumpEasyNPCMoveControl jumpMoveControl) {
      double dx = this.navigationTargetPos.getX() + 0.5 - this.mob.getX();
      double dz = this.navigationTargetPos.getZ() + 0.5 - this.mob.getZ();
      jumpMoveControl.setDirection((float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F, false);
      jumpMoveControl.setWantedMovement(this.speedModifier);
    } else {
      startNavigation();
    }

    log.debug("{} NPC moving to position {}", LOG_PREFIX, this.navigationTargetPos);
  }

  @Override
  public void tick() {
    this.ticksRunning++;

    if (reachedTarget()) {
      arrive();
      return;
    }

    if (this.ticksRunning >= this.timeoutTicks) {
      log.debug("{} Timeout reached for target position {}", LOG_PREFIX, this.targetPos);
      teleportAndArrive();
      return;
    }

    if (this.jumpControlled) {
      return;
    }

    this.ticksSincePathUpdate++;
    updateProgress();

    if (this.ticksWithoutProgress >= TICKS_WITHOUT_PROGRESS_LIMIT) {
      log.debug("{} NPC is not getting closer to target position {}", LOG_PREFIX, this.targetPos);
      teleportAndArrive();
      return;
    }

    if (this.mob.getNavigation().isDone() && this.ticksSincePathUpdate >= REPATH_INTERVAL_TICKS) {
      startNavigation();
    }
  }

  private boolean startNavigation() {
    this.ticksSincePathUpdate = 0;
    Path path = this.mob.getNavigation().createPath(this.navigationTargetPos, 1, PATHFINDING_RANGE);
    return path != null && this.mob.getNavigation().moveTo(path, this.speedModifier);
  }

  private void updateProgress() {
    Vec3 position = this.mob.position();
    double distance = position.distanceTo(Vec3.atCenterOf(this.navigationTargetPos));
    boolean closerToTarget = distance < this.closestDistanceToTarget - PROGRESS_DISTANCE;
    if (closerToTarget
        || position.distanceToSqr(this.lastProgressPosition)
            > PROGRESS_DISTANCE * PROGRESS_DISTANCE) {
      if (closerToTarget) {
        this.closestDistanceToTarget = distance;
      }
      this.lastProgressPosition = position;
      this.ticksWithoutProgress = 0;
      return;
    }

    this.ticksWithoutProgress++;
  }

  @Override
  public void stop() {
    this.mob.getNavigation().stop();
  }

  private boolean reachedTarget() {
    BlockPos blockPos =
        this.navigationTargetPos != null ? this.navigationTargetPos : this.targetPos;
    Vec3 position = this.mob.position();
    double distanceX = blockPos.getX() + 0.5 - position.x;
    double distanceZ = blockPos.getZ() + 0.5 - position.z;
    if (distanceX * distanceX + distanceZ * distanceZ
        > (double) this.arrivalRadius * this.arrivalRadius) {
      return false;
    }

    return Math.abs(blockPos.getY() - position.y)
        <= Math.max(this.arrivalRadius, MIN_VERTICAL_ARRIVAL_DISTANCE);
  }

  private void teleportAndArrive() {
    if (this.teleportOnTimeout && this.navigationData != null) {
      BlockPos blockPos =
          this.navigationTargetPos != null ? this.navigationTargetPos : this.targetPos;
      this.navigationData.setPosition(
          new Vec3(blockPos.getX() + 0.5, blockPos.getY(), blockPos.getZ() + 0.5));
    }
    arrive();
  }

  private void arrive() {
    if (!this.arrived) {
      this.arrived = true;
      this.mob.getNavigation().stop();
      log.debug("{} NPC arrived at position {}", LOG_PREFIX, this.targetPos);
      if (this.onArrival != null) {
        this.onArrival.run();
      }
    }
  }

  public boolean hasArrived() {
    return this.arrived;
  }
}
