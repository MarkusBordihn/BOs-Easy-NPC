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

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.SmartAttackHandler;
import net.minecraft.world.entity.ai.attributes.AttributeInstance;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.goal.MeleeAttackGoal;

public class CustomMeleeAttackGoal<T extends EasyNPC<?>> extends MeleeAttackGoal {

  private static final int DEFAULT_ATTACK_INTERVAL_TICKS = 20;
  private static final double TICKS_PER_SECOND = 20.0D;

  private final T easyNPC;

  // Vanilla MeleeAttackGoal keeps its cooldown in a private field that is always reset to 20 ticks.
  private int ticksUntilNextAttack;

  public CustomMeleeAttackGoal(T easyNPC, double speedModifier, boolean mustSeeTarget) {
    super(easyNPC.getPathfinderMob(), speedModifier, mustSeeTarget);
    this.easyNPC = easyNPC;
  }

  @Override
  public boolean canUse() {
    return this.isMeleeAttackSelected() && super.canUse();
  }

  @Override
  public boolean canContinueToUse() {
    return this.isMeleeAttackSelected() && super.canContinueToUse();
  }

  private boolean isMeleeAttackSelected() {
    return !this.isHoldingWeaponOfRangedAttack()
        && SmartAttackHandler.isAttackTypeAllowed(this.easyNPC, ObjectiveType.MELEE_ATTACK);
  }

  private boolean isHoldingWeaponOfRangedAttack() {
    ObjectiveDataCapable<?> objectiveData = this.easyNPC.getEasyNPCObjectiveData();
    return objectiveData != null
        && SmartAttackHandler.isUsableRangedWeapon(objectiveData, this.mob.getMainHandItem());
  }

  @Override
  public void stop() {
    super.stop();
    this.mob.setAggressive(false);
  }

  @Override
  public void start() {
    super.start();
    this.mob.setAggressive(true);
    this.ticksUntilNextAttack = 0;
  }

  @Override
  public void tick() {
    this.ticksUntilNextAttack = Math.max(this.ticksUntilNextAttack - 1, 0);
    super.tick();
  }

  @Override
  protected void resetAttackCooldown() {
    this.ticksUntilNextAttack = this.getAttackInterval();
  }

  @Override
  protected boolean isTimeToAttack() {
    return this.ticksUntilNextAttack <= 0;
  }

  @Override
  protected int getTicksUntilNextAttack() {
    return this.ticksUntilNextAttack;
  }

  @Override
  protected int getAttackInterval() {
    AttributeInstance attackSpeed = this.mob.getAttribute(Attributes.ATTACK_SPEED);
    if (attackSpeed == null || attackSpeed.getValue() <= 0.0D) {
      return this.adjustedTickDelay(DEFAULT_ATTACK_INTERVAL_TICKS);
    }

    return this.adjustedTickDelay(
        Math.max(1, (int) Math.round(TICKS_PER_SECOND / attackSpeed.getValue())));
  }
}
