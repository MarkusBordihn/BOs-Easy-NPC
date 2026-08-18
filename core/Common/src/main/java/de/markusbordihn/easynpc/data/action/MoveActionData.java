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

package de.markusbordihn.easynpc.data.action;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

public record MoveActionData(
    MoveTargetType targetType,
    double speedModifier,
    float arrivalRadius,
    int timeoutTicks,
    boolean teleportOnTimeout) {

  public static final String DATA_TARGET_TYPE_TAG = "Target";
  public static final String DATA_SPEED_TAG = "Speed";
  public static final String DATA_RADIUS_TAG = "Radius";
  public static final String DATA_TIMEOUT_TAG = "Timeout";
  public static final String DATA_TELEPORT_TAG = "Teleport";
  public static final double DEFAULT_SPEED_MODIFIER = 1.0;
  public static final float DEFAULT_ARRIVAL_RADIUS = 2.0F;
  public static final int DEFAULT_TIMEOUT_TICKS = 200;
  public static final double MIN_SPEED_MODIFIER = 0.1;
  public static final double MAX_SPEED_MODIFIER = 4.0;
  public static final float MIN_ARRIVAL_RADIUS = 0.5F;
  public static final float MAX_ARRIVAL_RADIUS = 16.0F;
  public static final int MIN_TIMEOUT_TICKS = 20;
  public static final int MAX_TIMEOUT_TICKS = 6000;
  public static final MoveActionData DEFAULT =
      new MoveActionData(
          MoveTargetType.POSITION,
          DEFAULT_SPEED_MODIFIER,
          DEFAULT_ARRIVAL_RADIUS,
          DEFAULT_TIMEOUT_TICKS,
          false);

  public MoveActionData {
    targetType = targetType == null ? MoveTargetType.POSITION : targetType;
    speedModifier = Math.min(MAX_SPEED_MODIFIER, Math.max(MIN_SPEED_MODIFIER, speedModifier));
    arrivalRadius = Math.min(MAX_ARRIVAL_RADIUS, Math.max(MIN_ARRIVAL_RADIUS, arrivalRadius));
    timeoutTicks = Math.min(MAX_TIMEOUT_TICKS, Math.max(MIN_TIMEOUT_TICKS, timeoutTicks));
  }

  public MoveActionData(MoveTargetType targetType) {
    this(targetType, DEFAULT_SPEED_MODIFIER, DEFAULT_ARRIVAL_RADIUS, DEFAULT_TIMEOUT_TICKS, false);
  }

  public static MoveActionData fromTag(CompoundTag compoundTag) {
    if (compoundTag == null || compoundTag.isEmpty()) {
      return DEFAULT;
    }

    return new MoveActionData(
        MoveTargetType.get(compoundTag.getString(DATA_TARGET_TYPE_TAG)),
        compoundTag.contains(DATA_SPEED_TAG)
            ? compoundTag.getDouble(DATA_SPEED_TAG)
            : DEFAULT_SPEED_MODIFIER,
        compoundTag.contains(DATA_RADIUS_TAG)
            ? compoundTag.getFloat(DATA_RADIUS_TAG)
            : DEFAULT_ARRIVAL_RADIUS,
        compoundTag.contains(DATA_TIMEOUT_TAG)
            ? compoundTag.getInt(DATA_TIMEOUT_TAG)
            : DEFAULT_TIMEOUT_TICKS,
        compoundTag.getBoolean(DATA_TELEPORT_TAG));
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    if (this.targetType != MoveTargetType.POSITION) {
      compoundTag.putString(DATA_TARGET_TYPE_TAG, this.targetType.name());
    }
    if (this.speedModifier != DEFAULT_SPEED_MODIFIER) {
      compoundTag.putDouble(DATA_SPEED_TAG, this.speedModifier);
    }
    if (this.arrivalRadius != DEFAULT_ARRIVAL_RADIUS) {
      compoundTag.putFloat(DATA_RADIUS_TAG, this.arrivalRadius);
    }
    if (this.timeoutTicks != DEFAULT_TIMEOUT_TICKS) {
      compoundTag.putInt(DATA_TIMEOUT_TAG, this.timeoutTicks);
    }
    if (this.teleportOnTimeout) {
      compoundTag.putBoolean(DATA_TELEPORT_TAG, true);
    }
    return compoundTag;
  }

  public boolean hasResolvableTarget(BlockPos blockPos) {
    return !this.targetType.requiresPosition()
        || (blockPos != null && !BlockPos.ZERO.equals(blockPos));
  }

  public MoveActionData withTargetType(MoveTargetType targetType) {
    return new MoveActionData(
        targetType,
        this.speedModifier,
        this.arrivalRadius,
        this.timeoutTicks,
        this.teleportOnTimeout);
  }

  public MoveActionData withSpeedModifier(double speedModifier) {
    return new MoveActionData(
        this.targetType,
        speedModifier,
        this.arrivalRadius,
        this.timeoutTicks,
        this.teleportOnTimeout);
  }

  public MoveActionData withArrivalRadius(float arrivalRadius) {
    return new MoveActionData(
        this.targetType,
        this.speedModifier,
        arrivalRadius,
        this.timeoutTicks,
        this.teleportOnTimeout);
  }

  public MoveActionData withTimeoutTicks(int timeoutTicks) {
    return new MoveActionData(
        this.targetType,
        this.speedModifier,
        this.arrivalRadius,
        timeoutTicks,
        this.teleportOnTimeout);
  }

  public MoveActionData withTeleportOnTimeout(boolean teleportOnTimeout) {
    return new MoveActionData(
        this.targetType,
        this.speedModifier,
        this.arrivalRadius,
        this.timeoutTicks,
        teleportOnTimeout);
  }
}
