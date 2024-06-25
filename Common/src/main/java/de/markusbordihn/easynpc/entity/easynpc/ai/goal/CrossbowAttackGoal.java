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
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttackHandler;
import java.util.EnumSet;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;

public class CrossbowAttackGoal<T extends EasyNPC<?>> extends Goal {
  public static final UniformInt PATHFINDING_DELAY_RANGE = TimeUtil.rangeOfSeconds(1, 2);
  private final double speedModifier;
  private final float attackRadiusSqr;
  private final PathfinderMob pathfinderMob;
  private final CrossbowAttackMob crossbowAttackMob;
  private int attackDelay;
  private CrossbowState crossbowState = CrossbowState.UNCHARGED;
  private int seeTime;
  private int updatePathDelay;

  public CrossbowAttackGoal(T easyNPC, double speedModifier, float attackRange) {
    this.pathfinderMob = easyNPC.getPathfinderMob();
    this.crossbowAttackMob = easyNPC.getCrossbowAttackMob();
    this.speedModifier = speedModifier;
    this.attackRadiusSqr = attackRange * attackRange;
    this.setFlags(EnumSet.of(Goal.Flag.MOVE, Goal.Flag.LOOK));
  }

  public boolean canUse() {
    return this.crossbowAttackMob != null
        && this.isValidTarget()
        && AttackHandler.isHoldingCrossbowWeapon(this.pathfinderMob);
  }

  @Override
  public boolean canContinueToUse() {
    return this.isValidTarget()
        && (this.canUse() || !this.pathfinderMob.getNavigation().isDone())
        && AttackHandler.isHoldingCrossbowWeapon(this.pathfinderMob);
  }

  private boolean isValidTarget() {
    return this.pathfinderMob.getTarget() != null && this.pathfinderMob.getTarget().isAlive();
  }

  @Override
  public void stop() {
    super.stop();
    this.pathfinderMob.setAggressive(false);
    this.pathfinderMob.setTarget(null);
    this.seeTime = 0;
    if (this.pathfinderMob.isUsingItem()) {
      this.pathfinderMob.stopUsingItem();
      this.crossbowAttackMob.setChargingCrossbow(false);
      CrossbowItem.setCharged(this.pathfinderMob.getUseItem(), false);
    }
  }

  @Override
  public void start() {
    super.start();
    this.pathfinderMob.setAggressive(true);
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return true;
  }

  @Override
  public void tick() {
    LivingEntity livingentity = this.pathfinderMob.getTarget();
    if (livingentity != null) {
      boolean hasLineOfSight = this.pathfinderMob.getSensing().hasLineOfSight(livingentity);
      boolean hasSeen = this.seeTime > 0;
      if (hasLineOfSight != hasSeen) {
        this.seeTime = 0;
      }

      if (hasLineOfSight) {
        ++this.seeTime;
      } else {
        --this.seeTime;
      }

      double distanceToTarget = this.pathfinderMob.distanceToSqr(livingentity);
      boolean flag2 =
          (distanceToTarget > this.attackRadiusSqr || this.seeTime < 5) && this.attackDelay == 0;
      if (flag2) {
        --this.updatePathDelay;
        if (this.updatePathDelay <= 0) {
          this.pathfinderMob
              .getNavigation()
              .moveTo(livingentity, this.canRun() ? this.speedModifier : this.speedModifier * 0.5D);
          this.updatePathDelay = PATHFINDING_DELAY_RANGE.sample(this.pathfinderMob.getRandom());
        }
      } else {
        this.updatePathDelay = 0;
        this.pathfinderMob.getNavigation().stop();
      }

      this.pathfinderMob.getLookControl().setLookAt(livingentity, 30.0F, 30.0F);
      if (this.crossbowState == CrossbowState.UNCHARGED) {
        if (!flag2) {
          this.pathfinderMob.startUsingItem(
              AttackHandler.getCrossbowHoldingHand(this.pathfinderMob));
          this.crossbowState = CrossbowState.CHARGING;
          this.crossbowAttackMob.setChargingCrossbow(true);
        }
      } else if (this.crossbowState == CrossbowState.CHARGING) {
        if (!this.pathfinderMob.isUsingItem()) {
          this.crossbowState = CrossbowState.UNCHARGED;
        }

        int i = this.pathfinderMob.getTicksUsingItem();
        ItemStack itemstack = this.pathfinderMob.getUseItem();
        if (i >= CrossbowItem.getChargeDuration(itemstack)) {
          this.pathfinderMob.releaseUsingItem();
          this.crossbowState = CrossbowState.CHARGED;
          this.attackDelay = 20 + this.pathfinderMob.getRandom().nextInt(20);
          this.crossbowAttackMob.setChargingCrossbow(false);
        }
      } else if (this.crossbowState == CrossbowState.CHARGED) {
        --this.attackDelay;
        if (this.attackDelay == 0) {
          this.crossbowState = CrossbowState.READY_TO_ATTACK;
        }
      } else if (this.crossbowState == CrossbowState.READY_TO_ATTACK && hasLineOfSight) {
        this.crossbowAttackMob.performRangedAttack(livingentity, 1.0F);
        ItemStack itemStack1 =
            this.pathfinderMob.getItemInHand(
                AttackHandler.getCrossbowHoldingHand(this.pathfinderMob));
        CrossbowItem.setCharged(itemStack1, false);
        this.crossbowState = CrossbowState.UNCHARGED;
      }
    }
  }

  private boolean canRun() {
    return this.crossbowState == CrossbowState.UNCHARGED;
  }

  enum CrossbowState {
    UNCHARGED,
    CHARGING,
    CHARGED,
    READY_TO_ATTACK
  }
}
