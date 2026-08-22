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

package de.markusbordihn.easynpc.entity.easynpc.ai.control;

import de.markusbordihn.easynpc.entity.easynpc.ai.SwimDepthRange;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.control.MoveControl;
import net.minecraft.world.entity.ai.control.SmoothSwimmingMoveControl;
import net.minecraft.world.phys.Vec3;

public class EasyNPCSwimmingMoveControl extends SmoothSwimmingMoveControl {

  public static final int MAX_TURN_X = 45;
  public static final int MAX_TURN_Y = 10;

  private static final float IN_WATER_SPEED_MODIFIER = 0.02F;
  private static final float OUTSIDE_WATER_SPEED_MODIFIER = 0.1F;
  private static final double DEPTH_EASING = 0.2D;
  private static final double DEPTH_MAX_SPEED = 0.12D;
  private static final double MIN_HORIZONTAL_STEERING_DISTANCE = 0.25D;

  public EasyNPCSwimmingMoveControl(Mob mob) {
    super(mob, MAX_TURN_X, MAX_TURN_Y, IN_WATER_SPEED_MODIFIER, OUTSIDE_WATER_SPEED_MODIFIER, true);
  }

  @Override
  public void setWantedPosition(double x, double y, double z, double speed) {
    SwimDepthRange swimDepthRange = SwimDepthRange.of(this.mob, BlockPos.containing(x, y, z));
    super.setWantedPosition(x, swimDepthRange.clamp(y), z, speed);
  }

  @Override
  public void tick() {
    if (this.operation == MoveControl.Operation.MOVE_TO && !this.mob.getNavigation().isDone()) {
      this.tickFollowingPath();
      return;
    }

    super.tick();
    if (!this.mob.isInWater()) {
      return;
    }

    SwimDepthRange swimDepthRange = SwimDepthRange.of(this.mob, this.mob.blockPosition());
    if (swimDepthRange.isRestricted()) {
      this.applyDepthCorrection(swimDepthRange.clamp(this.mob.getY()));
    }
  }

  private void tickFollowingPath() {
    float previousYRot = this.mob.getYRot();
    float previousYBodyRot = this.mob.yBodyRot;
    float previousYHeadRot = this.mob.yHeadRot;
    super.tick();
    if (!this.isHorizontallyAtWantedPosition()) {
      return;
    }

    this.mob.setYRot(previousYRot);
    this.mob.yBodyRot = previousYBodyRot;
    this.mob.yHeadRot = previousYHeadRot;
  }

  private boolean isHorizontallyAtWantedPosition() {
    double distanceX = this.wantedX - this.mob.getX();
    double distanceZ = this.wantedZ - this.mob.getZ();
    return distanceX * distanceX + distanceZ * distanceZ
        < MIN_HORIZONTAL_STEERING_DISTANCE * MIN_HORIZONTAL_STEERING_DISTANCE;
  }

  private void applyDepthCorrection(double targetY) {
    Vec3 deltaMovement = this.mob.getDeltaMovement();
    double heightDifference = targetY - this.mob.getY();
    this.mob.setDeltaMovement(
        deltaMovement.x,
        Mth.clamp(heightDifference * DEPTH_EASING, -DEPTH_MAX_SPEED, DEPTH_MAX_SPEED),
        deltaMovement.z);
  }
}
