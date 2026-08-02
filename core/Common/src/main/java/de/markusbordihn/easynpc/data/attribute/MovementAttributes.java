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
    double hoverHeight)
    implements EntityAttributesInterface {

  public static final String CAN_OPEN_DOOR_TAG = MovementAttributeType.CAN_OPEN_DOOR.getTagName();
  public static final String CAN_PASS_DOOR_TAG = MovementAttributeType.CAN_PASS_DOOR.getTagName();
  public static final String CAN_CLOSE_DOOR_TAG = MovementAttributeType.CAN_CLOSE_DOOR.getTagName();
  public static final String CAN_USE_NETHER_PORTAL_TAG =
      MovementAttributeType.CAN_USE_NETHER_PORTAL.getTagName();
  public static final String NAVIGATION_TYPE_TAG =
      MovementAttributeType.NAVIGATION_TYPE.getTagName();
  public static final String HOVER_HEIGHT_TAG = MovementAttributeType.HOVER_HEIGHT.getTagName();
  public static final double MAX_HOVER_HEIGHT = 16.0D;

  public MovementAttributes {
    if (navigationType == null) {
      navigationType = NavigationType.DEFAULT;
    }

    // A hover height reaches the movement control unchecked, where anything outside this range
    // would move the NPC to an invalid position.
    hoverHeight = Double.isNaN(hoverHeight) ? 0.0D : Mth.clamp(hoverHeight, 0.0D, MAX_HOVER_HEIGHT);
  }

  public MovementAttributes() {
    this(false, false, false, false, NavigationType.DEFAULT, 0.0D);
  }

  public MovementAttributes(
      boolean canOpenDoor, boolean canCloseDoor, boolean canPassDoor, boolean canUseNetherPortal) {
    this(canOpenDoor, canCloseDoor, canPassDoor, canUseNetherPortal, NavigationType.DEFAULT, 0.0D);
  }

  public static MovementAttributes decode(CompoundTag compoundTag) {
    return new MovementAttributes(
        compoundTag.getBoolean(CAN_OPEN_DOOR_TAG).orElse(false),
        compoundTag.getBoolean(CAN_CLOSE_DOOR_TAG).orElse(false),
        compoundTag.getBoolean(CAN_PASS_DOOR_TAG).orElse(false),
        compoundTag.getBoolean(CAN_USE_NETHER_PORTAL_TAG).orElse(false),
        NavigationType.fromName(compoundTag.getString(NAVIGATION_TYPE_TAG).orElse("")),
        compoundTag.getDouble(HOVER_HEIGHT_TAG).orElse(0.0D));
  }

  public MovementAttributes withCanOpenDoor(boolean canOpenDoor) {
    return new MovementAttributes(
        canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight);
  }

  public MovementAttributes withCanCloseDoor(boolean canCloseDoor) {
    return new MovementAttributes(
        this.canOpenDoor,
        canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight);
  }

  public MovementAttributes withCanPassDoor(boolean canPassDoor) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        this.hoverHeight);
  }

  public MovementAttributes withCanUseNetherPortal(boolean canUseNetherPortal) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        canUseNetherPortal,
        this.navigationType,
        this.hoverHeight);
  }

  public MovementAttributes withNavigationType(NavigationType navigationType) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        navigationType,
        this.hoverHeight);
  }

  public MovementAttributes withHoverHeight(double hoverHeight) {
    return new MovementAttributes(
        this.canOpenDoor,
        this.canCloseDoor,
        this.canPassDoor,
        this.canUseNetherPortal,
        this.navigationType,
        hoverHeight);
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    compoundTag.putBoolean(CAN_OPEN_DOOR_TAG, this.canOpenDoor);
    compoundTag.putBoolean(CAN_CLOSE_DOOR_TAG, this.canCloseDoor);
    compoundTag.putBoolean(CAN_PASS_DOOR_TAG, this.canPassDoor);
    compoundTag.putBoolean(CAN_USE_NETHER_PORTAL_TAG, this.canUseNetherPortal);
    compoundTag.putString(NAVIGATION_TYPE_TAG, this.navigationType.name());
    compoundTag.putDouble(HOVER_HEIGHT_TAG, this.hoverHeight);
    return compoundTag;
  }
}
