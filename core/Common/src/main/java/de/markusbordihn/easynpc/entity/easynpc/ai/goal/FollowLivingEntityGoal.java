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
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.navigation.PathNavigation;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.LeavesBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.pathfinder.PathType;
import net.minecraft.world.level.pathfinder.WalkNodeEvaluator;

public class FollowLivingEntityGoal extends Goal {

  private static final int COMBAT_COOLDOWN_DURATION = 3 * 20;

  private final Mob mob;
  private final NavigationDataCapable<?> navigationData;
  private final LivingEntity livingEntity;
  private final double speedModifier;
  private final float stopDistance;
  private final float startDistance;
  private final boolean canFly;
  private final boolean canJump;
  private final PathNavigation pathNavigation;
  private final LevelReader level;
  private float oldWaterCost;
  private int timeToRecalcPath;
  private int combatCooldownTicks = 0;

  public FollowLivingEntityGoal(
      EasyNPC<?> easyNPC,
      LivingEntity livingEntity,
      double speedModifier,
      float stopDistance,
      float startDistance) {
    this.mob = easyNPC.getMob();
    this.navigationData = easyNPC.getEasyNPCNavigationData();
    this.livingEntity = livingEntity;
    this.speedModifier = speedModifier;
    this.stopDistance = stopDistance;
    this.startDistance = startDistance;
    this.canFly = this.navigationData.canFly();
    this.canJump = this.navigationData.canJump();
    this.pathNavigation = this.mob.getNavigation();
    this.level = easyNPC.getEntityServerLevel();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE, Goal.Flag.LOOK));
  }

  @Override
  public boolean canUse() {
    if (this.mob.getTarget() != null) {
      this.combatCooldownTicks = COMBAT_COOLDOWN_DURATION;
      return false;
    }
    if (this.combatCooldownTicks > 0) {
      this.combatCooldownTicks--;
      return false;
    }
    double distanceSq = this.mob.distanceToSqr(this.livingEntity);
    return this.mob.isAlive()
        && this.livingEntity != null
        && this.livingEntity.isAlive()
        && distanceSq > this.stopDistance * this.stopDistance
        && distanceSq < this.startDistance * this.startDistance;
  }

  @Override
  public boolean canContinueToUse() {
    if (this.mob.getTarget() != null) {
      this.combatCooldownTicks = COMBAT_COOLDOWN_DURATION;
      return false;
    }
    if (this.combatCooldownTicks > 0) {
      this.combatCooldownTicks--;
      return false;
    }
    return !this.pathNavigation.isDone()
        && this.mob.distanceToSqr(this.livingEntity) > this.stopDistance * this.stopDistance;
  }

  @Override
  public void start() {
    this.timeToRecalcPath = 0;
    if (this.mob instanceof PathfinderMob pathfinderMob) {
      this.oldWaterCost = pathfinderMob.getPathfindingMalus(PathType.WATER);
      pathfinderMob.setPathfindingMalus(PathType.WATER, 0.0F);
    }
  }

  @Override
  public void stop() {
    this.pathNavigation.stop();
    if (this.mob instanceof PathfinderMob pathfinderMob) {
      pathfinderMob.setPathfindingMalus(PathType.WATER, this.oldWaterCost);
    }
  }

  @Override
  public void tick() {
    this.mob.getLookControl().setLookAt(this.livingEntity, 10.0F, this.mob.getMaxHeadXRot());
    if (--this.timeToRecalcPath <= 0) {
      this.timeToRecalcPath = this.adjustedTickDelay(10);
      if (!this.mob.isLeashed() && !this.mob.isPassenger()) {
        if (this.mob.distanceToSqr(this.livingEntity) >= 144.0D) {
          this.teleportToLivingEntity();
        } else if (this.canJump
            && this.mob.getMoveControl() instanceof JumpEasyNPCMoveControl jumpMoveControl) {
          double dx = this.livingEntity.getX() - this.mob.getX();
          double dz = this.livingEntity.getZ() - this.mob.getZ();
          jumpMoveControl.setDirection(
              (float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F, false);
          jumpMoveControl.setWantedMovement(this.speedModifier);
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
      this.mob.moveTo(posX + 0.5D, posY, posZ + 0.5D, this.mob.getYRot(), this.mob.getXRot());
      this.pathNavigation.stop();
      return true;
    }
  }

  private boolean canTeleportTo(BlockPos blockPos) {
    PathType blockPathTypes = WalkNodeEvaluator.getPathTypeStatic(this.mob, blockPos.mutable());
    if (!this.canFly && blockPathTypes != PathType.WALKABLE) {
      return false;
    } else {
      BlockState blockState = this.level.getBlockState(blockPos.below());
      if (!this.canFly && blockState.getBlock() instanceof LeavesBlock) {
        return false;
      } else {
        BlockPos targetBlockPos = blockPos.subtract(this.mob.blockPosition());
        return this.level.noCollision(this.mob, this.mob.getBoundingBox().move(targetBlockPos));
      }
    }
  }

  private int randomIntInclusive(int fromRange, int toRange) {
    return this.mob.getRandom().nextInt(toRange - fromRange + 1) + fromRange;
  }
}
