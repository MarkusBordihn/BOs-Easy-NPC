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

package de.markusbordihn.easynpc.data.attribute;

import net.minecraft.nbt.CompoundTag;

public record InteractionAttributes(
    boolean isPushable,
    boolean canBeHitByProjectile,
    boolean canBeLeashed,
    boolean pushEntities,
    boolean blockVehicleMounting)
    implements EntityAttributesInterface {

  public static final String IS_PUSHABLE_TAG = InteractionAttributeType.IS_PUSHABLE.getTagName();
  public static final String CAN_BE_HIT_BY_PROJECTILE_TAG =
      InteractionAttributeType.CAN_BE_HIT_BY_PROJECTILE.getTagName();
  public static final String CAN_BE_LEASHED_TAG =
      InteractionAttributeType.CAN_BE_LEASHED.getTagName();
  public static final String PUSH_ENTITIES_TAG =
      InteractionAttributeType.PUSH_ENTITIES.getTagName();
  public static final String BLOCK_VEHICLE_MOUNTING_TAG =
      InteractionAttributeType.BLOCK_VEHICLE_MOUNTING.getTagName();

  public InteractionAttributes() {
    this(false, false, false, false, false);
  }

  public static InteractionAttributes decode(CompoundTag compoundTag) {
    return new InteractionAttributes(
        compoundTag.getBoolean(IS_PUSHABLE_TAG),
        compoundTag.getBoolean(CAN_BE_HIT_BY_PROJECTILE_TAG),
        compoundTag.getBoolean(CAN_BE_LEASHED_TAG),
        compoundTag.getBoolean(PUSH_ENTITIES_TAG),
        compoundTag.getBoolean(BLOCK_VEHICLE_MOUNTING_TAG));
  }

  public InteractionAttributes withIsPushable(boolean isPushable) {
    return new InteractionAttributes(
        isPushable,
        this.canBeHitByProjectile,
        this.canBeLeashed,
        this.pushEntities,
        this.blockVehicleMounting);
  }

  public InteractionAttributes withCanBeHitByProjectile(boolean canBeHitByProjectile) {
    return new InteractionAttributes(
        this.isPushable,
        canBeHitByProjectile,
        this.canBeLeashed,
        this.pushEntities,
        this.blockVehicleMounting);
  }

  public InteractionAttributes withCanBeLeashed(boolean canBeLeashed) {
    return new InteractionAttributes(
        this.isPushable,
        this.canBeHitByProjectile,
        canBeLeashed,
        this.pushEntities,
        this.blockVehicleMounting);
  }

  public InteractionAttributes withPushEntities(boolean pushEntities) {
    return new InteractionAttributes(
        this.isPushable,
        this.canBeHitByProjectile,
        this.canBeLeashed,
        pushEntities,
        this.blockVehicleMounting);
  }

  public InteractionAttributes withBlockVehicleMounting(boolean blockVehicleMounting) {
    return new InteractionAttributes(
        this.isPushable,
        this.canBeHitByProjectile,
        this.canBeLeashed,
        this.pushEntities,
        blockVehicleMounting);
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    AttributeTagUtils.putIfTrue(compoundTag, IS_PUSHABLE_TAG, this.isPushable);
    AttributeTagUtils.putIfTrue(
        compoundTag, CAN_BE_HIT_BY_PROJECTILE_TAG, this.canBeHitByProjectile);
    AttributeTagUtils.putIfTrue(compoundTag, CAN_BE_LEASHED_TAG, this.canBeLeashed);
    AttributeTagUtils.putIfTrue(compoundTag, PUSH_ENTITIES_TAG, this.pushEntities);
    AttributeTagUtils.putIfTrue(compoundTag, BLOCK_VEHICLE_MOUNTING_TAG, this.blockVehicleMounting);
    return compoundTag;
  }
}
