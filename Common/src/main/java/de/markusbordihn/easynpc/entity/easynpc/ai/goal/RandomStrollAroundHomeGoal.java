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
import javax.annotation.Nullable;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.util.LandRandomPos;
import net.minecraft.world.phys.Vec3;

public class RandomStrollAroundHomeGoal<T extends EasyNPC<?>> extends RandomStrollGoal {

  private final NavigationData<?> navigationData;

  public RandomStrollAroundHomeGoal(T easyNPCEntity, double speed) {
    super(easyNPCEntity.getPathfinderMob(), speed, 240, false);
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
  }

  @Nullable
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
    return LandRandomPos.getPosTowards(this.mob, 10, 7, homePosition);
  }

  @Nullable
  private Vec3 getPositionTowardsAnywhere() {
    return LandRandomPos.getPos(this.mob, 10, 7);
  }
}
