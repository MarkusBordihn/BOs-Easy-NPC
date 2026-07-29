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

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.EnumSet;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;

public abstract class LookAtTargetGoal<T extends EasyNPC<?>> extends Goal {

  protected static final int TARGET_SEARCH_INTERVAL = 40;
  private static final float MAX_HEAD_ROTATION = 60.0F;
  private static final float LOOK_SPEED = 0.15F;

  protected final Mob mob;
  protected final float lookDistance;
  private final ModelDataCapable<?> modelData;
  private final LivingEntity livingEntity;
  private Entity targetEntity;
  private int searchCooldown;

  protected LookAtTargetGoal(T easyNPC, float lookDistance) {
    this.mob = easyNPC.getMob();
    this.modelData = easyNPC.getEasyNPCModelData();
    this.livingEntity = easyNPC.getLivingEntity();
    this.lookDistance = lookDistance;
    this.setFlags(EnumSet.of(Goal.Flag.LOOK));
  }

  protected abstract Entity findTargetEntity();

  protected Entity getTargetEntity() {
    return this.targetEntity;
  }

  @Override
  public boolean canUse() {
    if (this.targetEntity == null || !this.targetEntity.isAlive()) {
      this.targetEntity = this.findTargetEntity();
    }

    return this.isTargetInRange();
  }

  @Override
  public boolean canContinueToUse() {
    return this.isTargetInRange();
  }

  @Override
  public void start() {
    this.searchCooldown = 0;
  }

  @Override
  public void stop() {
    this.targetEntity = null;
  }

  @Override
  public void tick() {
    if (this.targetEntity != null && this.targetEntity.isAlive()) {
      if (this.hasLockedBodyPose()) {
        this.applyLimitedHeadRotationToTarget(this.targetEntity);
      } else {
        this.mob
            .getLookControl()
            .setLookAt(
                this.targetEntity.getX(), this.targetEntity.getEyeY(), this.targetEntity.getZ());
      }
    }

    if (--this.searchCooldown <= 0) {
      this.searchCooldown = TARGET_SEARCH_INTERVAL;
      this.targetEntity = this.findTargetEntity();
    }
  }

  private boolean isTargetInRange() {
    return this.targetEntity != null
        && this.targetEntity.isAlive()
        && this.mob.distanceToSqr(this.targetEntity) <= this.lookDistance * this.lookDistance;
  }

  private boolean hasLockedBodyPose() {
    if (this.modelData == null) {
      return false;
    }

    if (this.modelData.getModelPartRotation(ModelPartType.HEAD).hasChangedRotation()) {
      return false;
    }

    return this.modelData.getModelPose() == ModelPose.DEFAULT
        || this.modelData.getModelRootData().isRotationLocked();
  }

  private void applyLimitedHeadRotationToTarget(Entity target) {
    double dx = target.getX() - this.livingEntity.getX();
    double dz = target.getZ() - this.livingEntity.getZ();
    float targetAngle = (float) (Mth.atan2(dz, dx) * (180.0 / Math.PI)) - 90.0F;
    float bodyRot = this.livingEntity.yBodyRot;
    float clamped =
        bodyRot
            + Mth.clamp(
                Mth.wrapDegrees(targetAngle - bodyRot), -MAX_HEAD_ROTATION, MAX_HEAD_ROTATION);
    float delta = Mth.wrapDegrees(clamped - this.livingEntity.yHeadRot);
    this.livingEntity.yHeadRot += delta * LOOK_SPEED;
  }
}
