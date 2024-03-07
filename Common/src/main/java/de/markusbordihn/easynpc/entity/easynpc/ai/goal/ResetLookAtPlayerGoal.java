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
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import net.minecraft.world.entity.ai.control.LookControl;
import net.minecraft.world.entity.ai.goal.Goal;

public class ResetLookAtPlayerGoal<T extends EasyNPC<?>> extends Goal {

  private final T easyNPC;
  private final ModelData<?> modelData;
  private int resetLookTime = 40;

  public ResetLookAtPlayerGoal(T easyNPCEntity) {
    super();
    this.easyNPC = easyNPCEntity;
    this.modelData = easyNPCEntity.getEasyNPCModelData();
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
    return this.modelData.getModelLockRotation();
  }

  @Override
  public boolean canContinueToUse() {
    return this.modelData.getModelLockRotation() && this.resetLookTime > 0;
  }

  @Override
  public void tick() {
    if (this.resetLookTime > 0) {
      LookControl lookControl = this.easyNPC.getLookControl();
      if (lookControl != null) {
        lookControl.setLookAt(0, 0, 0);
      }
      this.resetLookTime--;
    }
  }
}
