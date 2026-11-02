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

import java.util.EnumSet;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;

public class OutOfCombatGoal extends Goal {

  public static final int COMBAT_COOLDOWN_TICKS = 60;

  private final Goal goal;
  private final Mob mob;
  private int lastTargetTick = -COMBAT_COOLDOWN_TICKS;

  public OutOfCombatGoal(Goal goal, Mob mob) {
    this.goal = goal;
    this.mob = mob;
  }

  @Override
  public boolean canUse() {
    return !this.isInCombat() && this.goal.canUse();
  }

  @Override
  public boolean canContinueToUse() {
    return !this.isInCombat() && this.goal.canContinueToUse();
  }

  @Override
  public boolean isInterruptable() {
    return this.goal.isInterruptable();
  }

  @Override
  public void start() {
    this.goal.start();
  }

  @Override
  public void stop() {
    this.goal.stop();
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return this.goal.requiresUpdateEveryTick();
  }

  @Override
  public void tick() {
    this.goal.tick();
  }

  @Override
  public EnumSet<Goal.Flag> getFlags() {
    return this.goal.getFlags();
  }

  @Override
  public String toString() {
    return this.goal.toString();
  }

  private boolean isInCombat() {
    LivingEntity target = this.mob.getTarget();
    if (target != null && target.isAlive()) {
      this.lastTargetTick = this.mob.tickCount;
      return true;
    }

    return this.isWithinCombatCooldown(this.lastTargetTick)
        || (this.mob.getLastHurtByMob() != null
            && this.isWithinCombatCooldown(this.mob.getLastHurtByMobTimestamp()))
        || (this.mob.getLastHurtMob() != null
            && this.isWithinCombatCooldown(this.mob.getLastHurtMobTimestamp()));
  }

  private boolean isWithinCombatCooldown(int combatTick) {
    return this.mob.tickCount - combatTick < COMBAT_COOLDOWN_TICKS;
  }
}
