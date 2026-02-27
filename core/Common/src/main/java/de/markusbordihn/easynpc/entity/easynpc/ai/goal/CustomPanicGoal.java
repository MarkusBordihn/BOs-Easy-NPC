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
import de.markusbordihn.easynpc.entity.easynpc.ai.control.JumpEasyNPCMoveControl;
import java.util.EnumSet;
import net.minecraft.util.Mth;
import net.minecraft.util.RandomSource;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.control.MoveControl;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.PanicGoal;

public class CustomPanicGoal<T extends EasyNPC<?>> extends Goal {

  private static final int PANIC_DURATION_TICKS = 60;
  private static final float PANIC_RANGE = 16.0F;

  private final Mob mob;
  private final PathfinderMob pathfinderMob;
  private final double speedModifier;
  private final PanicGoal vanillaPanicGoal;
  private int panicTicks;

  public CustomPanicGoal(T easyNPCEntity, double speedModifier) {
    this.mob = easyNPCEntity.getMob();
    this.pathfinderMob = easyNPCEntity.getPathfinderMob();
    this.speedModifier = speedModifier;

    if (this.pathfinderMob != null) {
      this.vanillaPanicGoal = new PanicGoal(this.pathfinderMob, speedModifier);
    } else {
      this.vanillaPanicGoal = null;
    }

    this.setFlags(EnumSet.of(Goal.Flag.MOVE));
  }

  @Override
  public boolean canUse() {
    if (this.vanillaPanicGoal != null) {
      return this.vanillaPanicGoal.canUse();
    }
    return this.mob.getLastHurtByMob() != null
        || this.mob.isOnFire()
        || this.mob.isInWaterOrBubble();
  }

  @Override
  public boolean canContinueToUse() {
    if (this.vanillaPanicGoal != null) {
      return this.vanillaPanicGoal.canContinueToUse();
    }
    return this.panicTicks > 0;
  }

  @Override
  public void start() {
    if (this.vanillaPanicGoal != null) {
      this.vanillaPanicGoal.start();
      return;
    }

    this.panicTicks = PANIC_DURATION_TICKS;
    moveToRandomPosition();
  }

  @Override
  public void tick() {
    if (this.vanillaPanicGoal != null) {
      this.vanillaPanicGoal.tick();
      return;
    }

    this.panicTicks--;
    if (this.panicTicks % 20 == 0) {
      moveToRandomPosition();
    }
  }

  @Override
  public void stop() {
    if (this.vanillaPanicGoal != null) {
      this.vanillaPanicGoal.stop();
      return;
    }

    this.panicTicks = 0;
  }

  @Override
  public boolean isInterruptable() {
    return false;
  }

  private void moveToRandomPosition() {
    RandomSource random = this.mob.getRandom();
    double x = this.mob.getX() + ((random.nextFloat() * 2.0F - 1.0F) * PANIC_RANGE);
    double y = this.mob.getY() + ((random.nextFloat() * 2.0F - 1.0F) * PANIC_RANGE);
    double z = this.mob.getZ() + ((random.nextFloat() * 2.0F - 1.0F) * PANIC_RANGE);

    MoveControl moveControl = this.mob.getMoveControl();
    if (moveControl instanceof JumpEasyNPCMoveControl jumpMoveControl) {
      jumpMoveControl.setDirection(
          (float) (Mth.atan2(z - this.mob.getZ(), x - this.mob.getX()) * (180.0 / Math.PI)) - 90.0F,
          true);
      jumpMoveControl.setWantedMovement(this.speedModifier);
    } else {
      moveControl.setWantedPosition(x, y, z, this.speedModifier);
    }
  }
}
