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
import de.markusbordihn.easynpc.handler.PlacementHandler;
import java.util.EnumSet;
import net.minecraft.core.BlockPos;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.LeavesBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.pathfinder.BlockPathTypes;
import net.minecraft.world.level.pathfinder.WalkNodeEvaluator;
import net.minecraft.world.phys.Vec3;

public class FollowLivingEntityGoal extends Goal {

  private static final int COMBAT_COOLDOWN_DURATION = 3 * 20;
  private static final int PATH_RECALCULATION_DELAY = 10;
  private static final int TELEPORT_ATTEMPTS = 10;
  private static final float NO_DISTANCE_LIMIT = 0.0F;

  private final Mob mob;
  private final NavigationDataCapable<?> navigationData;
  private final LivingEntity livingEntity;
  private final double speedModifier;
  private final float stopDistance;
  private final float maxFollowDistance;
  private final float teleportDistance;
  private final Vec3 followOffset;
  private final LevelReader level;
  private float oldWaterCost;
  private int timeToRecalcPath;
  private int combatCooldownTicks = 0;

  public FollowLivingEntityGoal(
      EasyNPC<?> easyNPC,
      LivingEntity livingEntity,
      double speedModifier,
      float stopDistance,
      float maxFollowDistance,
      float teleportDistance,
      Vec3 followOffset) {
    this.mob = easyNPC.getMob();
    this.navigationData = easyNPC.getEasyNPCNavigationData();
    this.livingEntity = livingEntity;
    this.speedModifier = speedModifier;
    this.stopDistance = stopDistance;
    this.maxFollowDistance =
        maxFollowDistance > stopDistance ? maxFollowDistance : NO_DISTANCE_LIMIT;
    this.teleportDistance = teleportDistance;
    this.followOffset = followOffset != null ? followOffset : Vec3.ZERO;
    this.level = easyNPC.getEntityServerLevel();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE, Goal.Flag.LOOK));
  }

  private Vec3 getTargetPosition() {
    Vec3 targetPosition = this.livingEntity.position();
    if (this.followOffset.lengthSqr() == 0.0D) {
      return targetPosition;
    }

    float targetYaw = this.livingEntity.getYRot() * Mth.DEG_TO_RAD;
    double sin = Mth.sin(targetYaw);
    double cos = Mth.cos(targetYaw);
    return targetPosition.add(
        -sin * this.followOffset.z - cos * this.followOffset.x,
        this.followOffset.y,
        cos * this.followOffset.z - sin * this.followOffset.x);
  }

  private double distanceToTargetSqr() {
    return this.mob.position().distanceToSqr(this.getTargetPosition());
  }

  private boolean isOnCombatCooldown() {
    if (this.mob.getTarget() != null) {
      this.combatCooldownTicks = COMBAT_COOLDOWN_DURATION;
      return true;
    }

    if (this.combatCooldownTicks > 0) {
      this.combatCooldownTicks--;
      return true;
    }

    return false;
  }

  @Override
  public boolean canUse() {
    if (this.isOnCombatCooldown()) {
      return false;
    }

    if (this.livingEntity == null || !this.livingEntity.isAlive() || !this.mob.isAlive()) {
      return false;
    }

    double distanceSq = this.distanceToTargetSqr();
    if (distanceSq <= this.stopDistance * this.stopDistance) {
      return false;
    }

    return this.maxFollowDistance <= NO_DISTANCE_LIMIT
        || distanceSq < this.maxFollowDistance * this.maxFollowDistance;
  }

  @Override
  public boolean canContinueToUse() {
    if (this.isOnCombatCooldown()) {
      return false;
    }

    if (this.livingEntity == null) {
      return false;
    }

    return !this.mob.getNavigation().isDone()
        && this.distanceToTargetSqr() > this.stopDistance * this.stopDistance;
  }

  @Override
  public void start() {
    this.timeToRecalcPath = 0;
    if (this.mob instanceof PathfinderMob pathfinderMob) {
      this.oldWaterCost = pathfinderMob.getPathfindingMalus(BlockPathTypes.WATER);
      pathfinderMob.setPathfindingMalus(BlockPathTypes.WATER, 0.0F);
    }
  }

  @Override
  public void stop() {
    this.mob.getNavigation().stop();
    if (this.mob instanceof PathfinderMob pathfinderMob) {
      pathfinderMob.setPathfindingMalus(BlockPathTypes.WATER, this.oldWaterCost);
    }
  }

  @Override
  public void tick() {
    this.mob.getLookControl().setLookAt(this.livingEntity, 10.0F, this.mob.getMaxHeadXRot());
    if (--this.timeToRecalcPath > 0) {
      return;
    }

    this.timeToRecalcPath = this.adjustedTickDelay(PATH_RECALCULATION_DELAY);
    if (this.mob.isLeashed() || this.mob.isPassenger()) {
      return;
    }

    Vec3 targetPosition = this.getTargetPosition();
    if (this.teleportDistance > NO_DISTANCE_LIMIT
        && this.mob.position().distanceToSqr(targetPosition)
            >= this.teleportDistance * this.teleportDistance) {
      this.teleportTo(targetPosition);
      return;
    }

    if (this.navigationData.canJump()
        && this.mob.getMoveControl() instanceof JumpEasyNPCMoveControl jumpMoveControl) {
      double dx = targetPosition.x - this.mob.getX();
      double dz = targetPosition.z - this.mob.getZ();
      jumpMoveControl.setDirection((float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F, false);
      jumpMoveControl.setWantedMovement(this.speedModifier);
      return;
    }

    if (!this.mob
            .getNavigation()
            .moveTo(targetPosition.x, targetPosition.y, targetPosition.z, this.speedModifier)
        && this.navigationData.canFly()) {
      // A flying NPC regularly has no path through open air, so steer it directly instead.
      this.mob
          .getMoveControl()
          .setWantedPosition(
              targetPosition.x, targetPosition.y, targetPosition.z, this.speedModifier);
    }
  }

  private void teleportTo(Vec3 targetPosition) {
    BlockPos blockPos = BlockPos.containing(targetPosition);

    for (int i = 0; i < TELEPORT_ATTEMPTS; ++i) {
      int offsetX = this.randomIntInclusive(-3, 3);
      int offsetY = this.randomIntInclusive(-1, 1);
      int offsetZ = this.randomIntInclusive(-3, 3);
      if (this.maybeTeleportTo(
          targetPosition,
          blockPos.getX() + offsetX,
          blockPos.getY() + offsetY,
          blockPos.getZ() + offsetZ)) {
        return;
      }
    }
  }

  private boolean maybeTeleportTo(Vec3 targetPosition, int posX, int posY, int posZ) {
    if (Math.abs(posX - targetPosition.x) < 2.0D && Math.abs(posZ - targetPosition.z) < 2.0D) {
      return false;
    }

    if (!this.canTeleportTo(new BlockPos(posX, posY, posZ))) {
      return false;
    }

    this.mob.moveTo(posX + 0.5D, posY, posZ + 0.5D, this.mob.getYRot(), this.mob.getXRot());
    this.mob.getNavigation().stop();
    return true;
  }

  private boolean canTeleportTo(BlockPos blockPos) {
    if (!this.navigationData.canFly()) {
      BlockPathTypes blockPathTypes =
          WalkNodeEvaluator.getBlockPathTypeStatic(this.level, blockPos.mutable());
      if (blockPathTypes != BlockPathTypes.WALKABLE) {
        return false;
      }

      BlockState blockState = this.level.getBlockState(blockPos.below());
      if (blockState.getBlock() instanceof LeavesBlock) {
        return false;
      }
    }

    return PlacementHandler.isFree(
        this.level, blockPos, this.mob.getDimensions(this.mob.getPose()));
  }

  private int randomIntInclusive(int fromRange, int toRange) {
    return this.mob.getRandom().nextInt(toRange - fromRange + 1) + fromRange;
  }
}
