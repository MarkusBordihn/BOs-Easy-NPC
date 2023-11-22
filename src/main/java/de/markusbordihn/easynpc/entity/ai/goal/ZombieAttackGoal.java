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

package de.markusbordihn.easynpc.entity.ai.goal;

import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import net.minecraft.world.entity.ai.goal.MeleeAttackGoal;

public class ZombieAttackGoal extends MeleeAttackGoal {

  private final EasyNPCEntity easyNPCEntity;
  private int raiseArmTicks;

  public ZombieAttackGoal(
      EasyNPCEntity easyNPCEntity, double speedModifier, boolean followingTargetEvenIfNotSeen) {
    super(easyNPCEntity, speedModifier, followingTargetEvenIfNotSeen);
    this.easyNPCEntity = easyNPCEntity;
  }

  @Override
  public void start() {
    super.start();
    this.raiseArmTicks = 0;
  }

  @Override
  public void stop() {
    super.stop();
    this.easyNPCEntity.setAggressive(false);
  }

  @Override
  public void tick() {
    super.tick();
    ++this.raiseArmTicks;
    this.easyNPCEntity.setAggressive(
        this.raiseArmTicks >= 5 && this.getTicksUntilNextAttack() < this.getAttackInterval() / 2);
  }
}
