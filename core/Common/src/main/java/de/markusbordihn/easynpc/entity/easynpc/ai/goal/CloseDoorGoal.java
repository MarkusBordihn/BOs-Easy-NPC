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

import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.DoorBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.DoubleBlockHalf;

public class CloseDoorGoal extends Goal {

  private static final int SCAN_HORIZONTAL_RADIUS = 2;
  private static final int SCAN_INTERVAL = 4;
  private static final double TRACK_DISTANCE_SQR = 2.25D;
  private static final double CLOSE_DISTANCE_SQR = 4.0D;

  private final Mob mob;
  private final Set<BlockPos> trackedDoors = new HashSet<>();
  private int scanCooldown;

  public CloseDoorGoal(Mob mob) {
    this.mob = mob;
    this.setFlags(EnumSet.noneOf(Goal.Flag.class));
  }

  @Override
  public boolean canUse() {
    return !this.trackedDoors.isEmpty() || this.mob.getNavigation().isInProgress();
  }

  @Override
  public boolean canContinueToUse() {
    return this.canUse();
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return true;
  }

  @Override
  public void stop() {
    this.trackedDoors.clear();
  }

  @Override
  public void tick() {
    if (--this.scanCooldown <= 0) {
      this.scanCooldown = SCAN_INTERVAL;
      BlockPos passedDoor = this.findNearbyOpenDoor();
      if (passedDoor != null) {
        this.trackedDoors.add(passedDoor);
      }
    }
    this.closeDistantDoors();
  }

  private BlockPos findNearbyOpenDoor() {
    Level level = this.mob.level();
    BlockPos mobPos = this.mob.blockPosition();
    BlockPos.MutableBlockPos cursor = new BlockPos.MutableBlockPos();
    for (int offsetX = -SCAN_HORIZONTAL_RADIUS; offsetX <= SCAN_HORIZONTAL_RADIUS; offsetX++) {
      for (int offsetZ = -SCAN_HORIZONTAL_RADIUS; offsetZ <= SCAN_HORIZONTAL_RADIUS; offsetZ++) {
        for (int offsetY = 0; offsetY <= 1; offsetY++) {
          cursor.set(mobPos.getX() + offsetX, mobPos.getY() + offsetY, mobPos.getZ() + offsetZ);
          BlockState blockState = level.getBlockState(cursor);
          if (!(blockState.getBlock() instanceof DoorBlock)
              || !DoorBlock.isWoodenDoor(level, cursor)
              || !blockState.getValue(DoorBlock.OPEN)) {
            continue;
          }
          BlockPos lowerPos =
              blockState.getValue(DoorBlock.HALF) == DoubleBlockHalf.UPPER
                  ? cursor.below()
                  : cursor.immutable();
          if (this.horizontalDistanceSqr(lowerPos) <= TRACK_DISTANCE_SQR) {
            return lowerPos;
          }
        }
      }
    }
    return null;
  }

  private void closeDistantDoors() {
    if (this.trackedDoors.isEmpty()) {
      return;
    }
    Level level = this.mob.level();
    Iterator<BlockPos> iterator = this.trackedDoors.iterator();
    while (iterator.hasNext()) {
      BlockPos doorPos = iterator.next();
      BlockState blockState = level.getBlockState(doorPos);
      if (!(blockState.getBlock() instanceof DoorBlock doorBlock)
          || !blockState.getValue(DoorBlock.OPEN)) {
        iterator.remove();
        continue;
      }
      if (this.horizontalDistanceSqr(doorPos) > CLOSE_DISTANCE_SQR) {
        doorBlock.setOpen(this.mob, level, blockState, doorPos, false);
        iterator.remove();
      }
    }
  }

  private double horizontalDistanceSqr(BlockPos blockPos) {
    return this.mob.distanceToSqr(blockPos.getX() + 0.5D, this.mob.getY(), blockPos.getZ() + 0.5D);
  }
}
