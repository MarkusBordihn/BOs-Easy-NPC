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
import java.util.EnumSet;
import net.minecraft.util.TimeUtil;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;

public class CrossbowAttackGoal extends Goal {
  public static final UniformInt PATHFINDING_DELAY_RANGE = TimeUtil.rangeOfSeconds(1, 2);
  private final EasyNPCEntity easyNPCEntity;
  private final double speedModifier;
  private final float attackRadiusSqr;
  private CrossbowState crossbowState = CrossbowState.UNCHARGED;
  private int seeTime;
  private int attackDelay;
  private int updatePathDelay;

  public CrossbowAttackGoal(EasyNPCEntity easyNPCEntity, double speedModifier, float attackRange) {
    this.easyNPCEntity = easyNPCEntity;
    this.speedModifier = speedModifier;
    this.attackRadiusSqr = attackRange * attackRange;
    this.setFlags(EnumSet.of(Goal.Flag.MOVE, Goal.Flag.LOOK));
  }

  public boolean canUse() {
    return this.isValidTarget() && this.isHoldingCrossbow();
  }

  private boolean isHoldingCrossbow() {
    return this.easyNPCEntity.isHolding(is -> is.getItem() instanceof CrossbowItem);
  }

  @Override
  public boolean canContinueToUse() {
    return this.isValidTarget()
        && (this.canUse() || !this.easyNPCEntity.getNavigation().isDone())
        && this.isHoldingCrossbow();
  }

  private boolean isValidTarget() {
    return this.easyNPCEntity.getTarget() != null && this.easyNPCEntity.getTarget().isAlive();
  }

  @Override
  public void stop() {
    super.stop();
    this.easyNPCEntity.setAggressive(false);
    this.easyNPCEntity.setTarget(null);
    this.seeTime = 0;
    if (this.easyNPCEntity.isUsingItem()) {
      this.easyNPCEntity.stopUsingItem();
      this.easyNPCEntity.setChargingCrossbow(false);
      CrossbowItem.setCharged(this.easyNPCEntity.getUseItem(), false);
    }
  }

  @Override
  public void start() {
    super.start();
    this.easyNPCEntity.setAggressive(true);
  }

  @Override
  public boolean requiresUpdateEveryTick() {
    return true;
  }

  @Override
  public void tick() {
    LivingEntity livingentity = this.easyNPCEntity.getTarget();
    if (livingentity != null) {
      boolean flag = this.easyNPCEntity.getSensing().hasLineOfSight(livingentity);
      boolean flag1 = this.seeTime > 0;
      if (flag != flag1) {
        this.seeTime = 0;
      }

      if (flag) {
        ++this.seeTime;
      } else {
        --this.seeTime;
      }

      double d0 = this.easyNPCEntity.distanceToSqr(livingentity);
      boolean flag2 = (d0 > this.attackRadiusSqr || this.seeTime < 5) && this.attackDelay == 0;
      if (flag2) {
        --this.updatePathDelay;
        if (this.updatePathDelay <= 0) {
          this.easyNPCEntity
              .getNavigation()
              .moveTo(livingentity, this.canRun() ? this.speedModifier : this.speedModifier * 0.5D);
          this.updatePathDelay = PATHFINDING_DELAY_RANGE.sample(this.easyNPCEntity.getRandom());
        }
      } else {
        this.updatePathDelay = 0;
        this.easyNPCEntity.getNavigation().stop();
      }

      this.easyNPCEntity.getLookControl().setLookAt(livingentity, 30.0F, 30.0F);
      if (this.crossbowState == CrossbowState.UNCHARGED) {
        if (!flag2) {
          this.easyNPCEntity.startUsingItem(
              ProjectileUtil.getWeaponHoldingHand(
                  this.easyNPCEntity, CrossbowItem.class::isInstance));
          this.crossbowState = CrossbowState.CHARGING;
          this.easyNPCEntity.setChargingCrossbow(true);
        }
      } else if (this.crossbowState == CrossbowState.CHARGING) {
        if (!this.easyNPCEntity.isUsingItem()) {
          this.crossbowState = CrossbowState.UNCHARGED;
        }

        int i = this.easyNPCEntity.getTicksUsingItem();
        ItemStack itemstack = this.easyNPCEntity.getUseItem();
        if (i >= CrossbowItem.getChargeDuration(itemstack)) {
          this.easyNPCEntity.releaseUsingItem();
          this.crossbowState = CrossbowState.CHARGED;
          this.attackDelay = 20 + this.easyNPCEntity.getRandom().nextInt(20);
          this.easyNPCEntity.setChargingCrossbow(false);
        }
      } else if (this.crossbowState == CrossbowState.CHARGED) {
        --this.attackDelay;
        if (this.attackDelay == 0) {
          this.crossbowState = CrossbowState.READY_TO_ATTACK;
        }
      } else if (this.crossbowState == CrossbowState.READY_TO_ATTACK && flag) {
        this.easyNPCEntity.performRangedAttack(livingentity, 1.0F);
        ItemStack itemstack1 =
            this.easyNPCEntity.getItemInHand(
                ProjectileUtil.getWeaponHoldingHand(
                    this.easyNPCEntity, CrossbowItem.class::isInstance));
        CrossbowItem.setCharged(itemstack1, false);
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
