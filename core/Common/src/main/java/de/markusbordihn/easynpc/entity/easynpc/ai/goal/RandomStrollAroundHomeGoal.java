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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.util.AirRandomPos;
import net.minecraft.world.entity.ai.util.LandRandomPos;
import net.minecraft.world.phys.Vec3;

public class RandomStrollAroundHomeGoal<T extends EasyNPC<?>> extends RandomStrollGoal {

  private final NavigationData<?> navigationData;
  private final Entity entity;

  public RandomStrollAroundHomeGoal(T easyNPCEntity, double speedModifier) {
    super(easyNPCEntity.getPathfinderMob(), speedModifier, 240, false);
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
    this.entity = easyNPCEntity.getEntity();
  }

  @Override
  public boolean canContinueToUse() {
    return !this.mob.getNavigation().isDone()
        && !this.mob.isVehicle()
        && (!this.mob.isAggressive() || this.mob.getTarget() == null);
  }

  @Override
  protected Vec3 getPosition() {
    if (this.mob.level().random.nextFloat() < 0.5F) {
      return this.getPositionTowardsAnywhere();
    } else {
      Vec3 targetPosition = this.getPositionTowardsHome();
      return targetPosition == null ? this.getPositionTowardsAnywhere() : targetPosition;
    }
  }

  protected Vec3 getPositionTowardsHome() {
    BlockPos homeBlockPos = this.navigationData.getHomePosition();
    Vec3 homePosition = new Vec3(homeBlockPos.getX(), homeBlockPos.getY(), homeBlockPos.getZ());
    if (this.navigationData.isFlying()) {
      BlockPos blockPos = this.entity.blockPosition();
      int homePositionDifference = (int) (homePosition.y - blockPos.getY());
      int flyingZ = 0;
      if (homePositionDifference > 2) {
        flyingZ = 4;
      } else if (homePositionDifference < -2) {
        flyingZ = -4;
      }
      int flyingX = 6;
      int flyingY = 8;
      int distanceToHome = blockPos.distManhattan(homeBlockPos);
      if (distanceToHome < 15) {
        flyingX = distanceToHome / 2;
        flyingY = distanceToHome / 2;
      }
      return AirRandomPos.getPosTowards(
          this.mob, flyingX, flyingY, flyingZ, homePosition, Constants.HALF_OF_PI);
    }
    return LandRandomPos.getPosTowards(this.mob, 10, 7, homePosition);
  }

  private Vec3 getPositionTowardsAnywhere() {
    return LandRandomPos.getPos(this.mob, 10, 7);
  }
}
