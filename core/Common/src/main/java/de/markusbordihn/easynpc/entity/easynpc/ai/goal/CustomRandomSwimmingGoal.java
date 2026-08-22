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

import de.markusbordihn.easynpc.entity.easynpc.ai.SwimDepthRange;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.RandomSwimmingGoal;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.pathfinder.PathComputationType;
import net.minecraft.world.phys.Vec3;

public class CustomRandomSwimmingGoal extends RandomSwimmingGoal {

  public CustomRandomSwimmingGoal(PathfinderMob pathfinderMob, double speedModifier, int interval) {
    super(pathfinderMob, speedModifier, interval);
  }

  @Override
  protected Vec3 getPosition() {
    Vec3 position = super.getPosition();
    if (position == null) {
      return null;
    }

    SwimDepthRange swimDepthRange = SwimDepthRange.of(this.mob, BlockPos.containing(position));
    double clampedY = swimDepthRange.clamp(position.y);
    if (clampedY == position.y) {
      return position;
    }

    Level level = this.mob.level();
    BlockPos clampedPosition = BlockPos.containing(position.x, clampedY, position.z);
    if (!level
        .getBlockState(clampedPosition)
        .isPathfindable(level, clampedPosition, PathComputationType.WATER)) {
      return null;
    }

    return new Vec3(position.x, clampedY, position.z);
  }
}
