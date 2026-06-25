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

package de.markusbordihn.easynpc.entity.easynpc.ai.goal;

import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.DoorInteractGoal;

public class CustomDoorInteractGoal extends DoorInteractGoal {

  private static final int MAX_DOOR_OPEN_TICKS = 100;

  private final boolean closeDoor;
  private int openTicks;

  public CustomDoorInteractGoal(Mob mob, boolean closeDoor) {
    super(mob);
    this.closeDoor = closeDoor;
  }

  @Override
  public boolean canContinueToUse() {
    return this.closeDoor
        && this.hasDoor
        && this.openTicks < MAX_DOOR_OPEN_TICKS
        && super.canContinueToUse();
  }

  @Override
  public void start() {
    super.start();
    this.openTicks = 0;
    this.setOpen(true);
  }

  @Override
  public void stop() {
    if (this.closeDoor) {
      this.setOpen(false);
    }
  }

  @Override
  public void tick() {
    this.openTicks++;
    super.tick();
  }
}
