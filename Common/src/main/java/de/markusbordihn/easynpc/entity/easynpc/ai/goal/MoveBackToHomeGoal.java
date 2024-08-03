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
import net.minecraft.core.BlockPos;
import net.minecraft.core.SectionPos;
import net.minecraft.core.Vec3i;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.util.DefaultRandomPos;
import net.minecraft.world.phys.Vec3;

public class MoveBackToHomeGoal<T extends EasyNPC<?>> extends Goal {

  private final float stopDistance;
  private final NavigationData<?> navigationData;
  private final PathfinderMob pathfinderMob;
  private final double speedModifier;
  private final int interval;
  protected double wantedX;
  protected double wantedY;
  protected double wantedZ;

  public MoveBackToHomeGoal(T easyNPCEntity, double speedModifier, float stopDistance) {
    this(easyNPCEntity, speedModifier, stopDistance, 240);
  }

  public MoveBackToHomeGoal(
      T easyNPCEntity, double speedModifier, float stopDistance, int interval) {
    this.stopDistance = stopDistance;
    this.speedModifier = speedModifier;
    this.interval = interval;
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
    this.pathfinderMob = easyNPCEntity.getPathfinderMob();
  }

  @Override
  public boolean canUse() {
    if (this.pathfinderMob.isVehicle()
        || reachedHome()
        || this.pathfinderMob.getRandom().nextInt(reducedTickDelay(this.interval)) != 0
        || (this.pathfinderMob.isAggressive() && this.pathfinderMob.getTarget() != null)) {
      return false;
    }

    // Check if we have a valid home position.
    Vec3 vec3 = getPosition();
    if (vec3 == null) {
      return false;
    }
    BlockPos blockPos = this.navigationData.getHomePosition();
    this.wantedX = blockPos.getX();
    this.wantedY = blockPos.getY();
    this.wantedZ = blockPos.getZ();
    return true;
  }

  @Override
  public boolean canContinueToUse() {
    return !reachedHome()
        && !this.pathfinderMob.getNavigation().isDone()
        && this.pathfinderMob.getTarget() == null;
  }

  @Override
  public void start() {
    this.pathfinderMob
        .getNavigation()
        .moveTo(this.wantedX, this.wantedY, this.wantedZ, this.speedModifier);
  }

  @Override
  public void stop() {
    this.pathfinderMob.getNavigation().stop();
    super.stop();
  }

  private Vec3 getPosition() {
    if (reachedHome()) {
      return null;
    }
    SectionPos currentPosition = SectionPos.of(this.pathfinderMob.blockPosition());
    SectionPos homePosition = SectionPos.of(this.navigationData.getHomePosition());
    return currentPosition != homePosition
        ? DefaultRandomPos.getPosTowards(
            this.pathfinderMob, 10, 7, Vec3.atBottomCenterOf(homePosition), 1.5707963705062866)
        : null;
  }

  private boolean reachedHome() {
    if (this.navigationData == null) {
      return true;
    } else if (!this.navigationData.hasHomePosition()) {
      return this.navigationData.getGroundPathNavigation().isDone();
    }

    BlockPos blockPos = this.pathfinderMob.blockPosition();
    Vec3i vec3i = new Vec3i(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    return this.navigationData.getHomePosition().closerThan(vec3i, this.stopDistance);
  }
}
