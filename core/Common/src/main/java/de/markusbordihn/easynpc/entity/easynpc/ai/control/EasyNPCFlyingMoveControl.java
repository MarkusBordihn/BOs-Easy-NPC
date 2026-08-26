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

import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.control.FlyingMoveControl;
import net.minecraft.world.entity.ai.control.MoveControl;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class EasyNPCFlyingMoveControl extends FlyingMoveControl {

  public static final int MAX_TURN = 20;

  private static final int SURFACE_SCAN_MARGIN = 4;
  private static final double HOVER_TOLERANCE = 0.25D;
  private static final double HOVER_EASING = 0.2D;
  private static final double HOVER_MAX_SPEED = 0.12D;
  private static final double HOVER_DAMPING = 0.6D;

  public EasyNPCFlyingMoveControl(Mob mob) {
    super(mob, MAX_TURN, true);
  }

  @Override
  public void tick() {
    double minHoverHeight = this.getMinHoverHeight();
    double hoverHeight = this.getHoverHeight();
    if (hoverHeight > 0.0D) {
      this.tickFixedHover(Math.max(hoverHeight, minHoverHeight));
      return;
    }

    if (minHoverHeight <= 0.0D) {
      super.tick();
      return;
    }

    double minHoverTargetY = this.getHoverTargetY(minHoverHeight);
    if (this.operation == MoveControl.Operation.MOVE_TO) {
      this.wantedY = Math.max(this.wantedY, minHoverTargetY);
      super.tick();
      return;
    }

    super.tick();
    this.applyMinHoverCorrection(minHoverTargetY);
  }

  private void tickFixedHover(double hoverHeight) {
    double hoverTargetY = this.getHoverTargetY(hoverHeight);
    if (this.operation == MoveControl.Operation.MOVE_TO) {
      this.wantedY =
          Mth.clamp(this.wantedY, hoverTargetY - HOVER_TOLERANCE, hoverTargetY + HOVER_TOLERANCE);
      super.tick();
      return;
    }

    super.tick();
    this.applyHoverCorrection(hoverTargetY);
  }

  private double getHoverHeight() {
    if (this.mob instanceof NavigationDataCapable<?> navigationData) {
      return navigationData.getHoverHeight();
    }
    return 0.0D;
  }

  private double getMinHoverHeight() {
    if (this.mob instanceof NavigationDataCapable<?> navigationData) {
      return navigationData.getMinHoverHeight();
    }
    return 0.0D;
  }

  private double getHoverTargetY(double hoverHeight) {
    Level level = this.mob.level();
    BlockPos.MutableBlockPos blockPos = this.mob.blockPosition().mutable();
    int lowestScannedY =
        Math.max(
            level.getMinBuildHeight(),
            Mth.floor(this.mob.getY() - hoverHeight - SURFACE_SCAN_MARGIN));
    while (blockPos.getY() >= lowestScannedY) {
      if (!level.getBlockState(blockPos).getCollisionShape(level, blockPos).isEmpty()) {
        return blockPos.getY() + 1.0D + hoverHeight;
      }
      blockPos.move(Direction.DOWN);
    }

    return this.mob.getY();
  }

  private void applyHoverCorrection(double hoverTargetY) {
    Vec3 deltaMovement = this.mob.getDeltaMovement();
    double heightDifference = hoverTargetY - this.mob.getY();
    if (Math.abs(heightDifference) <= HOVER_TOLERANCE) {
      this.mob.setDeltaMovement(deltaMovement.multiply(1.0D, HOVER_DAMPING, 1.0D));
      return;
    }

    this.mob.setDeltaMovement(
        deltaMovement.x,
        Mth.clamp(heightDifference * HOVER_EASING, -HOVER_MAX_SPEED, HOVER_MAX_SPEED),
        deltaMovement.z);
  }

  private void applyMinHoverCorrection(double minHoverTargetY) {
    double heightDifference = minHoverTargetY - this.mob.getY();
    if (heightDifference <= HOVER_TOLERANCE) {
      return;
    }

    Vec3 deltaMovement = this.mob.getDeltaMovement();
    this.mob.setDeltaMovement(
        deltaMovement.x,
        Math.max(
            deltaMovement.y, Mth.clamp(heightDifference * HOVER_EASING, 0.0D, HOVER_MAX_SPEED)),
        deltaMovement.z);
  }
}
