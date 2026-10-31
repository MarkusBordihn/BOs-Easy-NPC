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
import java.util.EnumSet;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;

public abstract class StrafingRangedAttackGoal<T extends EasyNPC<?>> extends Goal {
  protected final PathfinderMob pathfinderMob;
  private final double speedModifier;
  private final float attackRadiusSqr;
  private final int attackInterval;
  private int attackTime = -1;
  private int seeTime;
  private boolean strafingBackwards;
  private boolean strafingClockwise;
  private int strafingTime = -1;

  protected StrafingRangedAttackGoal(
      T easyNPC, double speedModifier, int attackInterval, float attackRadius) {
    this.pathfinderMob = easyNPC.getPathfinderMob();
    this.speedModifier = speedModifier;
    this.attackInterval = attackInterval;
    this.attackRadiusSqr = attackRadius * attackRadius;
    this.setFlags(EnumSet.of(Flag.MOVE, Flag.LOOK));
  }

  protected abstract boolean canAttack();

  protected abstract void performAttack(LivingEntity targetEntity);

  @Override
  public boolean canUse() {
    return this.pathfinderMob.getTarget() != null && this.canAttack();
  }

  @Override
  public boolean canContinueToUse() {
    return (this.canUse() || !this.pathfinderMob.getNavigation().isDone()) && this.canAttack();
  }

  @Override
  public void start() {
    super.start();
    this.pathfinderMob.setAggressive(true);
  }

  @Override
  public void stop() {
    super.stop();
    this.pathfinderMob.setAggressive(false);
    this.seeTime = 0;
    this.attackTime = -1;
    this.pathfinderMob.stopUsingItem();
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return true;
  }

  @Override
  public void tick() {
    LivingEntity livingEntity = this.pathfinderMob.getTarget();
    if (livingEntity != null) {
      double targetDistance =
          this.pathfinderMob.distanceToSqr(
              livingEntity.getX(), livingEntity.getY(), livingEntity.getZ());
      boolean hasLineOfSight = this.pathfinderMob.getSensing().hasLineOfSight(livingEntity);
      boolean hasSeen = this.seeTime > 0;
      if (hasLineOfSight != hasSeen) {
        this.seeTime = 0;
      }

      if (hasLineOfSight) {
        ++this.seeTime;
      } else {
        --this.seeTime;
      }

      if (!(targetDistance > this.attackRadiusSqr) && this.seeTime >= 20) {
        this.pathfinderMob.getNavigation().stop();
        ++this.strafingTime;
      } else {
        this.pathfinderMob.getNavigation().moveTo(livingEntity, this.speedModifier);
        this.strafingTime = -1;
      }

      if (this.strafingTime >= 20) {
        if (this.pathfinderMob.getRandom().nextFloat() < 0.3) {
          this.strafingClockwise = !this.strafingClockwise;
        }

        if (this.pathfinderMob.getRandom().nextFloat() < 0.3) {
          this.strafingBackwards = !this.strafingBackwards;
        }

        this.strafingTime = 0;
      }

      if (this.strafingTime > -1) {
        if (targetDistance > (this.attackRadiusSqr * 0.75F)) {
          this.strafingBackwards = false;
        } else if (targetDistance < (this.attackRadiusSqr * 0.25F)) {
          this.strafingBackwards = true;
        }

        this.pathfinderMob
            .getMoveControl()
            .strafe(this.strafingBackwards ? -0.5F : 0.5F, this.strafingClockwise ? 0.5F : -0.5F);
        this.pathfinderMob.lookAt(livingEntity, 30.0F, 30.0F);
      } else {
        this.pathfinderMob.getLookControl().setLookAt(livingEntity, 30.0F, 30.0F);
      }

      if (--this.attackTime <= 0) {
        if (!hasLineOfSight || targetDistance > this.attackRadiusSqr) {
          this.attackTime = this.attackInterval;
          return;
        }

        this.performAttack(livingEntity);
        this.attackTime = this.attackInterval;
      }
    }
  }
}
