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
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.LookAtPlayerGoal;
import net.minecraft.world.entity.player.Player;

public class CustomLookAtPlayerGoal<T extends EasyNPC<?>> extends LookAtPlayerGoal {
  private static final float MAX_HEAD_ROTATION = 60.0F;
  private static final float LOOK_SPEED = 0.15F;
  private static final int FAILED_SEARCH_COOLDOWN_TICKS = 4;
  private final ModelDataCapable<?> modelData;
  private final LivingEntity livingEntity;
  private final boolean looksAtPlayers;
  private int nextSearchTick;

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
    this.livingEntity = easyNPC.getLivingEntity();
    this.looksAtPlayers = Player.class.isAssignableFrom(lookAtType);
  }

  private boolean hasLockedBodyPose() {
    return this.modelData != null && this.modelData.hasLockedBodyPose();
  }

  @Override
  public boolean canUse() {
    if (this.modelData != null
        && this.modelData.getModelRootData().isRotationLocked()
        && !this.hasLockedBodyPose()) {
      return false;
    }

    if (this.mob.tickCount < this.nextSearchTick) {
      return false;
    }

    if (this.looksAtPlayers && !hasPlayerInRange(this.mob, this.lookDistance)) {
      this.startSearchCooldown();
      return false;
    }

    try {
      if (super.canUse()) {
        return true;
      }
    } catch (IllegalStateException e) {
      return false;
    }

    if (this.failedCanUseMeansNoTarget()) {
      this.startSearchCooldown();
    }
    return false;
  }

  public static boolean hasPlayerInRange(Entity entity, double range) {
    double maximumDistanceSquared = range * range;
    for (Player player : entity.level().players()) {
      if (!player.isSpectator() && player.distanceToSqr(entity) <= maximumDistanceSquared) {
        return true;
      }
    }
    return false;
  }

  private boolean failedCanUseMeansNoTarget() {
    return this.probability >= 1.0F;
  }

  private void startSearchCooldown() {
    this.nextSearchTick = this.mob.tickCount + FAILED_SEARCH_COOLDOWN_TICKS;
  }

  @Override
  public boolean canContinueToUse() {
    if (this.modelData != null
        && this.modelData.getModelRootData().isRotationLocked()
        && !this.hasLockedBodyPose()) {
      return false;
    }

    return super.canContinueToUse();
  }

  @Override
  public void tick() {
    if (this.modelData != null
        && this.modelData.getModelRootData().isRotationLocked()
        && !this.hasLockedBodyPose()) {
      return;
    }

    if (this.livingEntity != null && this.hasLockedBodyPose()) {
      if (this.lookAt != null && this.lookAt.isAlive()) {
        float delta = this.getDelta();
        this.livingEntity.yHeadRot += delta * LOOK_SPEED;
      }
    } else {
      super.tick();
    }
  }

  private float getDelta() {
    double dx = this.lookAt.getX() - this.livingEntity.getX();
    double dz = this.lookAt.getZ() - this.livingEntity.getZ();
    float targetAngle = (float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F;
    float bodyRot = this.livingEntity.yBodyRot;
    float clampedTarget =
        bodyRot
            + Mth.clamp(
                Mth.wrapDegrees(targetAngle - bodyRot), -MAX_HEAD_ROTATION, MAX_HEAD_ROTATION);

    return Mth.wrapDegrees(clampedTarget - this.livingEntity.yHeadRot);
  }
}
