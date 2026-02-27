/*
 * Copyright 2024 Markus Bordihn
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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.control.JumpEasyNPCMoveControl;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import java.util.EnumSet;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class MoveToPositionGoal<T extends EasyNPC<?>> extends Goal {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String LOG_PREFIX = "[Move To Position Goal]";
  private static final int TELEPORT_DISTANCE = 32;
  private static final int TIMEOUT_TICKS = 200;
  private static final float STOP_DISTANCE = 1.5F;

  private final NavigationDataCapable<?> navigationData;
  private final Mob mob;
  private final PathfinderMob pathfinderMob;
  private final BlockPos targetPos;
  private final double speedModifier;
  private final Runnable onArrival;
  private final boolean canJump;
  private int ticksRunning;
  private boolean arrived;

  public MoveToPositionGoal(
      T easyNPC, BlockPos targetPos, double speedModifier, Runnable onArrival) {
    this.navigationData = easyNPC.getEasyNPCNavigationData();
    this.mob = easyNPC.getMob();
    this.pathfinderMob = easyNPC.getPathfinderMob();
    this.targetPos = targetPos;
    this.speedModifier = speedModifier;
    this.onArrival = onArrival;
    this.canJump = this.navigationData != null && this.navigationData.canJump();
    this.setFlags(EnumSet.of(Goal.Flag.MOVE));
  }

  @Override
  public boolean canUse() {
    return this.targetPos != null && !this.arrived && !reachedTarget();
  }

  @Override
  public boolean canContinueToUse() {
    return !this.arrived && !reachedTarget() && this.ticksRunning < TIMEOUT_TICKS;
  }

  @Override
  public void start() {
    this.ticksRunning = 0;
    this.arrived = false;

    double distance = this.mob.position().distanceTo(Vec3.atCenterOf(this.targetPos));
    if (distance > TELEPORT_DISTANCE) {
      teleportAndArrive();
      return;
    }

    if (this.canJump
        && this.mob.getMoveControl() instanceof JumpEasyNPCMoveControl jumpMoveControl) {
      double dx = this.targetPos.getX() + 0.5 - this.mob.getX();
      double dz = this.targetPos.getZ() + 0.5 - this.mob.getZ();
      jumpMoveControl.setDirection((float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F, false);
      jumpMoveControl.setWantedMovement(this.speedModifier);
    } else {
      this.mob
          .getNavigation()
          .moveTo(
              this.targetPos.getX() + 0.5,
              this.targetPos.getY(),
              this.targetPos.getZ() + 0.5,
              this.speedModifier);
    }

    log.debug("{} NPC moving to position {}", LOG_PREFIX, this.targetPos);
  }

  @Override
  public void tick() {
    this.ticksRunning++;

    if (reachedTarget()) {
      arrive();
      return;
    }

    if (this.ticksRunning >= TIMEOUT_TICKS) {
      log.debug("{} Timeout reached, teleporting to {}", LOG_PREFIX, this.targetPos);
      teleportAndArrive();
    }
  }

  @Override
  public void stop() {
    this.mob.getNavigation().stop();
  }

  private boolean reachedTarget() {
    BlockPos blockPos = this.mob.blockPosition();
    Vec3i vec3i = new Vec3i(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    return this.targetPos.closerThan(vec3i, STOP_DISTANCE);
  }

  private void teleportAndArrive() {
    if (this.navigationData != null) {
      this.navigationData.setPosition(
          new Vec3(
              this.targetPos.getX() + 0.5, this.targetPos.getY(), this.targetPos.getZ() + 0.5));
    }
    arrive();
  }

  private void arrive() {
    if (!this.arrived) {
      this.arrived = true;
      this.mob.getNavigation().stop();
      log.debug("{} NPC arrived at position {}", LOG_PREFIX, this.targetPos);
      if (this.onArrival != null) {
        this.onArrival.run();
      }
    }
  }

  public boolean hasArrived() {
    return this.arrived;
  }
}
