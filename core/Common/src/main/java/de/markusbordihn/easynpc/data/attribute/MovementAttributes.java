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
import net.minecraft.util.Mth;

public record MovementAttributes(
    boolean canOpenDoor,
    boolean canCloseDoor,
    boolean canPassDoor,
    boolean canUseNetherPortal,
    NavigationType navigationType,
    double hoverHeight,
    double swimDepthBelowSurface,
    double swimHeightAboveFloor,
    boolean isImmovable)
    implements EntityAttributesInterface {

  public static final String CAN_OPEN_DOOR_TAG = MovementAttributeType.CAN_OPEN_DOOR.getTagName();
  public static final String CAN_PASS_DOOR_TAG = MovementAttributeType.CAN_PASS_DOOR.getTagName();
  public static final String CAN_CLOSE_DOOR_TAG = MovementAttributeType.CAN_CLOSE_DOOR.getTagName();
  public static final String CAN_USE_NETHER_PORTAL_TAG =
      MovementAttributeType.CAN_USE_NETHER_PORTAL.getTagName();
  public static final String NAVIGATION_TYPE_TAG =
      MovementAttributeType.NAVIGATION_TYPE.getTagName();
  public static final String HOVER_HEIGHT_TAG = MovementAttributeType.HOVER_HEIGHT.getTagName();
  public static final String SWIM_DEPTH_BELOW_SURFACE_TAG =
      MovementAttributeType.SWIM_DEPTH_BELOW_SURFACE.getTagName();
  public static final String SWIM_HEIGHT_ABOVE_FLOOR_TAG =
      MovementAttributeType.SWIM_HEIGHT_ABOVE_FLOOR.getTagName();
  public static final String IS_IMMOVABLE_TAG = MovementAttributeType.IS_IMMOVABLE.getTagName();
  public static final double MAX_HOVER_HEIGHT = 16.0D;
  public static final double MAX_SWIM_DEPTH_BELOW_SURFACE = 32.0D;
  public static final double MAX_SWIM_HEIGHT_ABOVE_FLOOR = 16.0D;

  public MovementAttributes {
    if (navigationType == null) {
      navigationType = NavigationType.DEFAULT;
    }

    // A hover height reaches the movement control unchecked, where anything outside this range
    // would move the NPC to an invalid position.
    hoverHeight = clampDistance(hoverHeight, MAX_HOVER_HEIGHT);
    swimDepthBelowSurface = clampDistance(swimDepthBelowSurface, MAX_SWIM_DEPTH_BELOW_SURFACE);
    swimHeightAboveFloor = clampDistance(swimHeightAboveFloor, MAX_SWIM_HEIGHT_ABOVE_FLOOR);
  }

  public MovementAttributes() {
    this(false, false, false, false, NavigationType.DEFAULT, 0.0D, 0.0D, 0.0D, false);
  }

  public MovementAttributes(
      boolean canOpenDoor, boolean canCloseDoor, boolean canPassDoor, boolean canUseNetherPortal) {
    this(
        canOpenDoor,
        canCloseDoor,
        canPassDoor,
        canUseNetherPortal,
        NavigationType.DEFAULT,
        0.0D,
        0.0D,
        0.0D,
        false);
  }

  private static double clampDistance(double distance, double maxDistance) {
    return Double.isNaN(distance) ? 0.0D : Mth.clamp(distance, 0.0D, maxDistance);
  }

  public static MovementAttributes decode(CompoundTag compoundTag) {
    return new MovementAttributes(
        compoundTag.getBoolean(CAN_OPEN_DOOR_TAG),
        compoundTag.getBoolean(CAN_CLOSE_DOOR_TAG),
        compoundTag.getBoolean(CAN_PASS_DOOR_TAG),
        compoundTag.getBoolean(CAN_USE_NETHER_PORTAL_TAG),
        NavigationType.fromName(compoundTag.getString(NAVIGATION_TYPE_TAG)),
        compoundTag.getDouble(HOVER_HEIGHT_TAG),
        compoundTag.getDouble(SWIM_DEPTH_BELOW_SURFACE_TAG),
        compoundTag.getDouble(SWIM_HEIGHT_ABOVE_FLOOR_TAG),
        compoundTag.getBoolean(IS_IMMOVABLE_TAG));
  }

  public MovementAttributes withCanOpenDoor(boolean canOpenDoor) {
    return new MovementAttributes(
        canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withCanCloseDoor(boolean canCloseDoor) {
    return new MovementAttributes(
        this.canOpenDoor,
        canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withCanPassDoor(boolean canPassDoor) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withCanUseNetherPortal(boolean canUseNetherPortal) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withNavigationType(NavigationType navigationType) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withHoverHeight(double hoverHeight) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withSwimDepthBelowSurface(double swimDepthBelowSurface) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withSwimHeightAboveFloor(double swimHeightAboveFloor) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        swimHeightAboveFloor,
        this.isImmovable);
  }

  public MovementAttributes withIsImmovable(boolean isImmovable) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight,
        this.swimDepthBelowSurface,
        this.swimHeightAboveFloor,
        isImmovable);
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    AttributeTagUtils.putIfTrue(compoundTag, CAN_OPEN_DOOR_TAG, this.canOpenDoor);
    AttributeTagUtils.putIfTrue(compoundTag, CAN_CLOSE_DOOR_TAG, this.canCloseDoor);
    AttributeTagUtils.putIfTrue(compoundTag, CAN_PASS_DOOR_TAG, this.canPassDoor);
    AttributeTagUtils.putIfTrue(compoundTag, CAN_USE_NETHER_PORTAL_TAG, this.canUseNetherPortal);
    if (this.navigationType != NavigationType.DEFAULT) {
      compoundTag.putString(NAVIGATION_TYPE_TAG, this.navigationType.name());
    }
    AttributeTagUtils.putIfNotZero(compoundTag, HOVER_HEIGHT_TAG, this.hoverHeight);
    AttributeTagUtils.putIfNotZero(
        compoundTag, SWIM_DEPTH_BELOW_SURFACE_TAG, this.swimDepthBelowSurface);
    AttributeTagUtils.putIfNotZero(
        compoundTag, SWIM_HEIGHT_ABOVE_FLOOR_TAG, this.swimHeightAboveFloor);
    AttributeTagUtils.putIfTrue(compoundTag, IS_IMMOVABLE_TAG, this.isImmovable);
    return compoundTag;
  }
}
