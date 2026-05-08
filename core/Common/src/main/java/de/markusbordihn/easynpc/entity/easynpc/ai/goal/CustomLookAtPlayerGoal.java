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

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.LookAtPlayerGoal;

public class CustomLookAtPlayerGoal<T extends EasyNPC<?>> extends LookAtPlayerGoal {
  private static final float MAX_HEAD_ROTATION = 60.0F;
  private static final float LOOK_SPEED = 0.15F;
  private final ModelDataCapable<?> modelData;
  private final LivingEntity livingEntity;

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

  @Override
  public boolean canUse() {
    if (this.modelData != null
        && this.modelData.getModelRootData().isRotationLocked()
        && !hasLockedBodyPose()) {
      return false;
    }
    return super.canUse();
  }

  @Override
  public boolean canContinueToUse() {
    if (this.modelData != null
        && this.modelData.getModelRootData().isRotationLocked()
        && !hasLockedBodyPose()) {
      return false;
    }

    return super.canContinueToUse();
  }

  @Override
  public void tick() {
    if (this.modelData != null
        && this.modelData.getModelRootData().isRotationLocked()
        && !hasLockedBodyPose()) {
      return;
    }

    if (this.livingEntity != null && hasLockedBodyPose()) {
      if (this.lookAt != null && this.lookAt.isAlive()) {
        float delta = getDelta();
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
