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
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.LookAtPlayerGoal;

public class CustomLookAtPlayerGoal<T extends EasyNPC<?>> extends LookAtPlayerGoal {

  private final ModelData<?> modelData;

  public CustomLookAtPlayerGoal(
      T easyNPC, Class<? extends LivingEntity> lookAtType, float lookDistance, float probability) {
    this(easyNPC, lookAtType, lookDistance, probability, false);
  }

  public CustomLookAtPlayerGoal(
      T easyNPC,
      Class<? extends LivingEntity> lookAtType,
      float lookDistance,
      float probability,
      boolean onlyHorizontal) {
    super(easyNPC.getMob(), lookAtType, lookDistance, probability, onlyHorizontal);
    this.modelData = easyNPC.getEasyNPCModelData();
  }

  @Override
  public boolean canUse() {
    return !this.modelData.getModelLockRotation() && super.canUse();
  }

  @Override
  public boolean canContinueToUse() {
    return !this.modelData.getModelLockRotation() && super.canContinueToUse();
  }

  @Override
  public void tick() {
    if (!this.modelData.getModelLockRotation()) {
      super.tick();
    }
  }
}
