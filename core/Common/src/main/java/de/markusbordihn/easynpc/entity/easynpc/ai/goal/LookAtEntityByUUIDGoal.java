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
import java.util.EnumSet;
import java.util.UUID;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.Goal;

public class LookAtEntityByUUIDGoal<T extends EasyNPC<?>> extends Goal {

  private static final int ENTITY_SEARCH_INTERVAL = 40;
  private static final float MAX_HEAD_ROTATION = 60.0F;
  private static final float LOOK_SPEED = 0.15F;

  private final Mob mob;
  private final ModelDataCapable<?> modelData;
  private final LivingEntity livingEntity;
  private final UUID targetEntityUUID;
  private final float lookDistance;
  private LivingEntity targetEntity;
  private int searchCooldown;

  public LookAtEntityByUUIDGoal(T easyNPC, UUID targetEntityUUID, float lookDistance) {
    this.mob = easyNPC.getMob();
    this.modelData = easyNPC.getEasyNPCModelData();
    this.livingEntity = easyNPC.getLivingEntity();
    this.targetEntityUUID = targetEntityUUID;
    this.lookDistance = lookDistance;
    this.setFlags(EnumSet.of(Goal.Flag.LOOK));
  }

  @Override
  public boolean canUse() {
    if (this.targetEntity == null || !this.targetEntity.isAlive()) {
      resolveTargetEntity();
    }
    return isTargetInRange();
  }

  @Override
  public boolean canContinueToUse() {
    return isTargetInRange();
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
      if (hasLockedBodyPose()) {
        applyLimitedHeadRotationToTarget(this.targetEntity);
      } else {
        this.mob
            .getLookControl()
            .setLookAt(
                this.targetEntity.getX(), this.targetEntity.getEyeY(), this.targetEntity.getZ());
      }
    }

    if (--this.searchCooldown <= 0) {
      this.searchCooldown = ENTITY_SEARCH_INTERVAL;
      resolveTargetEntity();
    }
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

  private void applyLimitedHeadRotationToTarget(LivingEntity target) {
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

  private boolean isTargetInRange() {
    return this.targetEntity != null
        && this.targetEntity.isAlive()
        && this.mob.distanceToSqr(this.targetEntity) <= this.lookDistance * this.lookDistance;
  }

  private void resolveTargetEntity() {
    if (!(this.mob.level() instanceof ServerLevel serverLevel)) {
      return;
    }

    Entity entity = serverLevel.getEntity(this.targetEntityUUID);
    this.targetEntity =
        entity instanceof LivingEntity targetLivingEntity ? targetLivingEntity : null;
  }
}
