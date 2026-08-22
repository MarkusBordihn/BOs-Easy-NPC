/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.entity.easynpc.ai.navigation;

import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.navigation.WaterBoundPathNavigation;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public class EasyNPCWaterBoundPathNavigation extends WaterBoundPathNavigation {

  private static final int MAX_TICKS_WITHOUT_NODE_PROGRESS = 60;

  private int trackedNodeIndex = -1;
  private int trackedNodeTick;

  public EasyNPCWaterBoundPathNavigation(Mob mob, Level level) {
    super(mob, level);
  }

  @Override
  protected void doStuckDetection(Vec3 position) {
    super.doStuckDetection(position);
    if (this.path == null || this.path.isDone()) {
      this.trackedNodeIndex = -1;
      return;
    }

    int nextNodeIndex = this.path.getNextNodeIndex();
    if (nextNodeIndex != this.trackedNodeIndex) {
      this.trackedNodeIndex = nextNodeIndex;
      this.trackedNodeTick = this.tick;
      return;
    }

    if (this.tick - this.trackedNodeTick > MAX_TICKS_WITHOUT_NODE_PROGRESS) {
      this.trackedNodeIndex = -1;
      this.stop();
    }
  }
}
