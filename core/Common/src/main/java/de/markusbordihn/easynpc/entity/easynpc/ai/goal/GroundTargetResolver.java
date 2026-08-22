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

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.pathfinder.PathComputationType;

final class GroundTargetResolver {

  private static final int MAX_SNAP_DISTANCE = 16;

  private GroundTargetResolver() {}

  static BlockPos resolveGroundTarget(Mob mob, BlockPos targetPos) {
    if (!(mob.getNavigation() instanceof GroundPathNavigation)) {
      return targetPos;
    }

    Level level = mob.level();
    if (isBlocked(level, targetPos)) {
      BlockPos surfacePos = targetPos.above();
      int maxSurfaceY = Math.min(targetPos.getY() + MAX_SNAP_DISTANCE, level.getMaxBuildHeight());
      while (surfacePos.getY() < maxSurfaceY && isBlocked(level, surfacePos)) {
        surfacePos = surfacePos.above();
      }
      return isBlocked(level, surfacePos) ? targetPos : surfacePos;
    }

    BlockPos groundPos = targetPos.below();
    int minGroundY = Math.max(targetPos.getY() - MAX_SNAP_DISTANCE, level.getMinBuildHeight());
    while (groundPos.getY() > minGroundY && !isBlocked(level, groundPos)) {
      groundPos = groundPos.below();
    }
    return isBlocked(level, groundPos) ? groundPos.above() : targetPos;
  }

  private static boolean isBlocked(Level level, BlockPos blockPos) {
    return !level.getBlockState(blockPos).isPathfindable(level, blockPos, PathComputationType.LAND);
  }
}
