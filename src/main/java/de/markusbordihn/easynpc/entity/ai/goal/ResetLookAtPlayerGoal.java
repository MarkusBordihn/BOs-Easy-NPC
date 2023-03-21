/**
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

package de.markusbordihn.easynpc.entity.ai.goal;

import net.minecraft.world.entity.ai.goal.Goal;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;

public class ResetLookAtPlayerGoal extends Goal {

  EasyNPCEntity easyNPCEntity;
  private int resetLookTime = 40;

  public ResetLookAtPlayerGoal(EasyNPCEntity easyNPCEntity) {
    super();
    this.easyNPCEntity = easyNPCEntity;
  }

  @Override
  public void start() {
    this.resetLookTime = 40;
  }

  @Override
  public void stop() {
    this.resetLookTime = 0;
  }

  @Override
  public boolean canUse() {
    return this.easyNPCEntity.getModelLockRotation();
  }

  @Override
  public boolean canContinueToUse() {
    return this.easyNPCEntity.getModelLockRotation() && this.resetLookTime > 0;
  }

  @Override
  public void tick() {
    if (this.resetLookTime > 0) {
      this.easyNPCEntity.getLookControl().setLookAt(0, 0, 0);
      this.resetLookTime--;
    }
  }

}
