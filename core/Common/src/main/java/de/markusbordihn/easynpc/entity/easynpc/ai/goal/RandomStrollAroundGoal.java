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
import de.markusbordihn.easynpc.entity.easynpc.ai.control.JumpEasyNPCMoveControl;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import java.util.EnumSet;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.control.MoveControl;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.util.AirAndWaterRandomPos;
import net.minecraft.world.entity.ai.util.DefaultRandomPos;
import net.minecraft.world.entity.ai.util.HoverRandomPos;
import net.minecraft.world.phys.Vec3;

public class RandomStrollAroundGoal<T extends EasyNPC<?>> extends Goal {

  private final NavigationDataCapable<?> navigationData;
  private final Mob mob;
  private final PathfinderMob pathfinderMob;
  private final double speedModifier;
  private final int interval;

  public RandomStrollAroundGoal(T easyNPCEntity, double speedModifier) {
    this(easyNPCEntity, speedModifier, 120);
  }

  public RandomStrollAroundGoal(T easyNPCEntity, double speedModifier, int interval) {
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
    this.mob = easyNPCEntity.getMob();
    this.pathfinderMob = easyNPCEntity.getPathfinderMob();
    this.speedModifier = speedModifier;
    this.interval = interval;
    this.setFlags(EnumSet.of(Goal.Flag.MOVE));
  }

  @Override
  public boolean canUse() {
    if (this.mob.isVehicle()
        || this.mob.getRandom().nextInt(reducedTickDelay(this.interval)) != 0) {
      return false;
    }

    if (this.pathfinderMob != null) {
      Vec3 vec3 = this.getPosition();
      if (vec3 == null) {
        return false;
      }
      return this.pathfinderMob.getNavigation().moveTo(vec3.x, vec3.y, vec3.z, this.speedModifier);
    }

    MoveControl moveControl = this.mob.getMoveControl();
    if (!moveControl.hasWanted()) {
      return true;
    }
    double dx = moveControl.getWantedX() - this.mob.getX();
    double dy = moveControl.getWantedY() - this.mob.getY();
    double dz = moveControl.getWantedZ() - this.mob.getZ();
    double distanceSq = dx * dx + dy * dy + dz * dz;
    return distanceSq < 1.0D || distanceSq > 3600.0D;
  }

  @Override
  public boolean canContinueToUse() {
    if (this.pathfinderMob != null) {
      return !this.pathfinderMob.getNavigation().isDone()
          && !this.pathfinderMob.isVehicle()
          && (!this.pathfinderMob.isAggressive() || this.pathfinderMob.getTarget() == null);
    }
    return false;
  }

  @Override
  public void start() {
    if (this.pathfinderMob == null) {
      RandomSource random = this.mob.getRandom();
      double x = this.mob.getX() + ((random.nextFloat() * 2.0F - 1.0F) * 16.0F);
      double y = this.mob.getY() + ((random.nextFloat() * 2.0F - 1.0F) * 16.0F);
      double z = this.mob.getZ() + ((random.nextFloat() * 2.0F - 1.0F) * 16.0F);

      MoveControl moveControl = this.mob.getMoveControl();
      if (moveControl instanceof JumpEasyNPCMoveControl jumpMoveControl) {
        jumpMoveControl.setDirection(
            (float) (Mth.atan2(z - this.mob.getZ(), x - this.mob.getX()) * (180.0 / Math.PI))
                - 90.0F,
            false);
        jumpMoveControl.setWantedMovement(this.speedModifier);
      } else {
        moveControl.setWantedPosition(x, y, z, this.speedModifier);
      }
    }
  }

  @Override
  public void stop() {
    if (this.pathfinderMob != null) {
      this.pathfinderMob.getNavigation().stop();
    }
  }

  protected Vec3 getPosition() {
    if (this.navigationData.canFly()) {
      Vec3 vec3 = this.pathfinderMob.getViewVector(0.0F);
      Vec3 randomHoverPos =
          HoverRandomPos.getPos(
              this.pathfinderMob, 8, 7, vec3.x, vec3.z, Constants.HALF_OF_PI, 3, 1);
      return randomHoverPos != null
          ? randomHoverPos
          : AirAndWaterRandomPos.getPos(
              this.pathfinderMob, 8, 4, -2, vec3.x, vec3.z, Constants.HALF_OF_PI);
    }
    return DefaultRandomPos.getPos(this.pathfinderMob, 10, 7);
  }
}
