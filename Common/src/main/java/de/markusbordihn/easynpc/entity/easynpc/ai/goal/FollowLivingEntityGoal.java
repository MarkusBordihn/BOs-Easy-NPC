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
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import java.util.EnumSet;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.navigation.PathNavigation;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.LeavesBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.pathfinder.BlockPathTypes;
import net.minecraft.world.level.pathfinder.WalkNodeEvaluator;

public class FollowLivingEntityGoal extends Goal {

  private final PathfinderMob pathfinderMob;
  private final LivingEntity livingEntity;
  private final double speedModifier;
  private final float stopDistance;
  private final float startDistance;
  private final boolean canFly;
  private final PathNavigation pathNavigation;
  private final LevelReader level;
  private float oldWaterCost;
  private int timeToRecalcPath;

  public FollowLivingEntityGoal(
      EasyNPC<?> easyNPC,
      LivingEntity livingEntity,
      double speedModifier,
      float stopDistance,
      float startDistance) {
    this.pathfinderMob = easyNPC.getPathfinderMob();
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    this.livingEntity = livingEntity;
    this.speedModifier = speedModifier;
    this.stopDistance = stopDistance;
    this.startDistance = startDistance;
    this.canFly = navigationData.canFly();
    this.pathNavigation = this.pathfinderMob.getNavigation();
    this.level = easyNPC.getServerLevel();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE, Goal.Flag.LOOK));
  }

  @Override
  public boolean canUse() {
    return this.pathfinderMob != null
        && this.pathfinderMob.isAlive()
        && this.livingEntity != null
        && this.livingEntity.isAlive()
        && this.pathfinderMob.distanceToSqr(this.livingEntity)
        > (this.stopDistance * this.stopDistance)
        && this.pathfinderMob.distanceToSqr(this.livingEntity)
        < (this.startDistance * this.startDistance);
  }

  @Override
  public boolean canContinueToUse() {
    if (this.pathNavigation.isDone()) {
      return false;
    } else {
      return this.pathfinderMob.distanceToSqr(this.livingEntity)
          > this.stopDistance * this.stopDistance;
    }
  }

  @Override
  public void start() {
    this.timeToRecalcPath = 0;
    this.oldWaterCost = this.pathfinderMob.getPathfindingMalus(BlockPathTypes.WATER);
    this.pathfinderMob.setPathfindingMalus(BlockPathTypes.WATER, 0.0F);
  }

  @Override
  public void stop() {
    this.pathNavigation.stop();
    this.pathfinderMob.setPathfindingMalus(BlockPathTypes.WATER, this.oldWaterCost);
  }

  @Override
  public void tick() {
    this.pathfinderMob
        .getLookControl()
        .setLookAt(this.livingEntity, 10.0F, this.pathfinderMob.getMaxHeadXRot());
    if (--this.timeToRecalcPath <= 0) {
      this.timeToRecalcPath = this.adjustedTickDelay(10);
      if (!this.pathfinderMob.isLeashed() && !this.pathfinderMob.isPassenger()) {
        if (this.pathfinderMob.distanceToSqr(this.livingEntity) >= 144.0D) {
          this.teleportToLivingEntity();
        } else {
          this.pathNavigation.moveTo(this.livingEntity, this.speedModifier);
        }
      }
    }
  }

  private void teleportToLivingEntity() {
    BlockPos blockPos = this.livingEntity.blockPosition();

    for (int i = 0; i < 10; ++i) {
      int j = this.randomIntInclusive(-3, 3);
      int k = this.randomIntInclusive(-1, 1);
      int l = this.randomIntInclusive(-3, 3);
      boolean flag =
          this.maybeTeleportTo(blockPos.getX() + j, blockPos.getY() + k, blockPos.getZ() + l);
      if (flag) {
        return;
      }
    }
  }

  private boolean maybeTeleportTo(int posX, int posY, int posZ) {
    if (Math.abs(posX - this.livingEntity.getX()) < 2.0D
        && Math.abs(posZ - this.livingEntity.getZ()) < 2.0D) {
      return false;
    } else if (!this.canTeleportTo(new BlockPos(posX, posY, posZ))) {
      return false;
    } else {
      this.pathfinderMob.moveTo(
          posX + 0.5D,
          posY,
          posZ + 0.5D,
          this.pathfinderMob.getYRot(),
          this.pathfinderMob.getXRot());
      this.pathNavigation.stop();
      return true;
    }
  }

  private boolean canTeleportTo(BlockPos blockPos) {
    BlockPathTypes blockPathTypes =
        WalkNodeEvaluator.getBlockPathTypeStatic(this.level, blockPos.mutable());
    if (!this.canFly && blockPathTypes != BlockPathTypes.WALKABLE) {
      return false;
    } else {
      BlockState blockState = this.level.getBlockState(blockPos.below());
      if (!this.canFly && blockState.getBlock() instanceof LeavesBlock) {
        return false;
      } else {
        BlockPos targetBlockPos = blockPos.subtract(this.pathfinderMob.blockPosition());
        return this.level.noCollision(
            this.pathfinderMob, this.pathfinderMob.getBoundingBox().move(targetBlockPos));
      }
    }
  }

  private int randomIntInclusive(int fromRange, int toRange) {
    return this.pathfinderMob.getRandom().nextInt(toRange - fromRange + 1) + fromRange;
  }
}
