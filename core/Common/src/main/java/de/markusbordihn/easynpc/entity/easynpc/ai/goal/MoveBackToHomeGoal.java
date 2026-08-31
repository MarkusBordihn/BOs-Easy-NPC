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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

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

public class MoveBackToHomeGoal<T extends EasyNPC<?>> extends Goal {

  private static final int DEFAULT_INTERVAL = 60;
  private static final int PATHFINDING_RANGE = 48;
  private static final float MIN_VERTICAL_ARRIVAL_DISTANCE = 2.0F;
  private static final int TICKS_WITHOUT_PROGRESS_LIMIT = 60;
  private static final double PROGRESS_DISTANCE = 0.5D;

  private final float stopDistance;
  private final NavigationDataCapable<?> navigationData;
  private final Mob mob;
  private final double speedModifier;
  private final int interval;
  private final boolean canJump;
  private BlockPos homeTargetPos;
  private Vec3 lastProgressPosition;
  private int ticksWithoutProgress;

  public MoveBackToHomeGoal(T easyNPCEntity, double speedModifier, float stopDistance) {
    this(easyNPCEntity, speedModifier, stopDistance, DEFAULT_INTERVAL);
  }

  public MoveBackToHomeGoal(
      T easyNPCEntity, double speedModifier, float stopDistance, int interval) {
    this.stopDistance = stopDistance;
    this.speedModifier = speedModifier;
    this.interval = interval;
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
    this.mob = easyNPCEntity.getMob();
    this.canJump = this.navigationData.canJump();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE));
  }

  @Override
  public boolean canUse() {
    if (this.mob.isVehicle()
        || this.mob.getRandom().nextInt(reducedTickDelay(this.interval)) != 0
        || (this.mob.isAggressive() && this.mob.getTarget() != null)
        || this.navigationData == null
        || !this.navigationData.hasHomePosition()) {
      return false;
    }

    this.homeTargetPos =
        GroundTargetResolver.resolveGroundTarget(this.mob, this.navigationData.getHomePosition());
    return !reachedHome();
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return true;
  }

  @Override
  public boolean canContinueToUse() {
    return !this.mob.getNavigation().isDone()
        && !this.mob.isVehicle()
        && this.mob.getTarget() == null
        && this.ticksWithoutProgress < TICKS_WITHOUT_PROGRESS_LIMIT
        && !reachedHome();
  }

  @Override
  public void start() {
    this.ticksWithoutProgress = 0;
    this.lastProgressPosition = this.mob.position();

    if (this.canJump
        && this.mob.getMoveControl() instanceof JumpEasyNPCMoveControl jumpMoveControl) {
      double dx = this.homeTargetPos.getX() + 0.5 - this.mob.getX();
      double dz = this.homeTargetPos.getZ() + 0.5 - this.mob.getZ();
      jumpMoveControl.setDirection((float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F, false);
      jumpMoveControl.setWantedMovement(this.speedModifier);
      return;
    }

    Path path = this.mob.getNavigation().createPath(this.homeTargetPos, 1, PATHFINDING_RANGE);
    if (path != null) {
      this.mob.getNavigation().moveTo(path, this.speedModifier);
    }
  }

  @Override
  public void tick() {
    Vec3 position = this.mob.position();
    if (position.distanceToSqr(this.lastProgressPosition) > PROGRESS_DISTANCE * PROGRESS_DISTANCE) {
      this.lastProgressPosition = position;
      this.ticksWithoutProgress = 0;
      return;
    }

    this.ticksWithoutProgress++;
  }

  @Override
  public void stop() {
    this.mob.getNavigation().stop();
    super.stop();
  }

  private boolean reachedHome() {
    if (this.navigationData == null || !this.navigationData.hasHomePosition()) {
      return true;
    }

    BlockPos blockPos =
        this.homeTargetPos != null ? this.homeTargetPos : this.navigationData.getHomePosition();
    Vec3 position = this.mob.position();
    double distanceX = blockPos.getX() + 0.5 - position.x;
    double distanceZ = blockPos.getZ() + 0.5 - position.z;
    double stopDistanceSqr = (double) this.stopDistance * this.stopDistance;
    if (distanceX * distanceX + distanceZ * distanceZ > stopDistanceSqr) {
      return false;
    }

    return Math.abs(blockPos.getY() - position.y)
        <= Math.max(this.stopDistance, MIN_VERTICAL_ARRIVAL_DISTANCE);
  }
}
