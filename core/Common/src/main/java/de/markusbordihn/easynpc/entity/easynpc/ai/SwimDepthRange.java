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

package de.markusbordihn.easynpc.entity.easynpc.ai;

import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.tags.FluidTags;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.Level;

public record SwimDepthRange(double lowestY, double highestY) {

  public static final SwimDepthRange UNRESTRICTED =
      new SwimDepthRange(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY);

  private static final int VERTICAL_SCAN_RANGE = 36;

  public static SwimDepthRange of(Mob mob, BlockPos waterColumnPosition) {
    if (!(mob instanceof NavigationDataCapable<?> navigationData)) {
      return UNRESTRICTED;
    }

    double depthBelowSurface = navigationData.getSwimDepthBelowSurface();
    double heightAboveFloor = navigationData.getSwimHeightAboveFloor();
    if (depthBelowSurface <= 0.0D && heightAboveFloor <= 0.0D) {
      return UNRESTRICTED;
    }

    Level level = mob.level();
    if (depthBelowSurface <= 0.0D) {
      double floorLevel = getFloorY(level, waterColumnPosition) + heightAboveFloor;
      return new SwimDepthRange(floorLevel, floorLevel);
    }

    double surfaceLevel = getSurfaceY(level, waterColumnPosition) - depthBelowSurface;
    if (heightAboveFloor <= 0.0D) {
      return new SwimDepthRange(surfaceLevel, surfaceLevel);
    }

    double floorLevel = getFloorY(level, waterColumnPosition) + heightAboveFloor;
    return new SwimDepthRange(
        Math.min(floorLevel, surfaceLevel), Math.max(floorLevel, surfaceLevel));
  }

  private static double getFloorY(Level level, BlockPos waterColumnPosition) {
    BlockPos.MutableBlockPos blockPos = waterColumnPosition.mutable();
    int lowestScannedY =
        Math.max(level.getMinY(), waterColumnPosition.getY() - VERTICAL_SCAN_RANGE);
    while (blockPos.getY() >= lowestScannedY) {
      if (!level.getBlockState(blockPos).getCollisionShape(level, blockPos).isEmpty()) {
        return blockPos.getY() + 1.0D;
      }
      blockPos.move(Direction.DOWN);
    }

    return lowestScannedY;
  }

  private static double getSurfaceY(Level level, BlockPos waterColumnPosition) {
    BlockPos.MutableBlockPos blockPos = waterColumnPosition.mutable();
    int highestScannedY =
        Math.min(level.getMaxY(), waterColumnPosition.getY() + VERTICAL_SCAN_RANGE);
    while (blockPos.getY() <= highestScannedY) {
      if (!level.getFluidState(blockPos).is(FluidTags.WATER)) {
        return blockPos.getY();
      }
      blockPos.move(Direction.UP);
    }

    return highestScannedY;
  }

  public boolean isRestricted() {
    return this.lowestY != Double.NEGATIVE_INFINITY || this.highestY != Double.POSITIVE_INFINITY;
  }

  public double clamp(double y) {
    return Mth.clamp(y, this.lowestY, this.highestY);
  }
}
