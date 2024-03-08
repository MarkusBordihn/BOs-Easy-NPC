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
import net.minecraft.core.SectionPos;
import net.minecraft.core.Vec3i;
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.util.DefaultRandomPos;
import net.minecraft.world.phys.Vec3;

public class MoveBackToHomeGoal<T extends EasyNPC<?>> extends RandomStrollGoal {

  private final float stopDistance;
  private final NavigationData<?> navigationData;

  public MoveBackToHomeGoal(T easyNPCEntity, double speedModifier, float stopDistance) {
    super(easyNPCEntity.getPathfinderMob(), speedModifier, 120);
    this.stopDistance = stopDistance;
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
  }

  private boolean reachedHome() {
    if (this.navigationData == null) {
      return true;
    } else if (!this.navigationData.hasHomePosition()) {
      return this.navigationData.getGroundPathNavigation().isDone();
    }

    BlockPos blockPos = this.mob.blockPosition();
    Vec3i vec3i = new Vec3i(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    return this.navigationData.getHomePosition().closerThan(vec3i, this.stopDistance);
  }

  @Override
  public boolean canUse() {
    if (reachedHome()) {
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
    return reachedHome() && super.canContinueToUse();
  }

  @Override
  @Nullable
  protected Vec3 getPosition() {
    if (reachedHome()) {
      return null;
    }
    SectionPos currentPosition = SectionPos.of(this.mob.blockPosition());
    SectionPos homePosition = SectionPos.of(this.navigationData.getHomePosition());
    return currentPosition != homePosition
        ? DefaultRandomPos.getPosTowards(
            this.mob, 10, 7, Vec3.atBottomCenterOf(homePosition), 1.5707963705062866)
        : null;
  }
}
