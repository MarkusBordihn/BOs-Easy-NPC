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
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.util.AirAndWaterRandomPos;
import net.minecraft.world.entity.ai.util.DefaultRandomPos;
import net.minecraft.world.entity.ai.util.HoverRandomPos;
import net.minecraft.world.phys.Vec3;

public class RandomStrollAroundGoal<T extends EasyNPC<?>> extends RandomStrollGoal {

  private final NavigationData<?> navigationData;

  public RandomStrollAroundGoal(T easyNPCEntity, double speedModifier) {
    this(easyNPCEntity, speedModifier, 120);
  }

  public RandomStrollAroundGoal(T easyNPCEntity, double speedModifier, int interval) {
    super(easyNPCEntity.getPathfinderMob(), speedModifier, interval);
    this.navigationData = easyNPCEntity.getEasyNPCNavigationData();
  }

  @Override
  protected Vec3 getPosition() {
    if (this.navigationData.canFly()) {
      Vec3 vec3 = this.mob.getViewVector(0.0F);
      Vec3 randomHoverPos =
          HoverRandomPos.getPos(this.mob, 8, 7, vec3.x, vec3.z, Constants.HALF_OF_PI, 3, 1);
      return randomHoverPos != null
          ? randomHoverPos
          : AirAndWaterRandomPos.getPos(this.mob, 8, 4, -2, vec3.x, vec3.z, Constants.HALF_OF_PI);
    }
    return DefaultRandomPos.getPos(this.mob, 10, 7);
  }
}
