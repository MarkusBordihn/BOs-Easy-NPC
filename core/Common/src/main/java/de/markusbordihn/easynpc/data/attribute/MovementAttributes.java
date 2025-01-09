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

public record MovementAttributes(
    boolean canOpenDoor, boolean canCloseDoor, boolean canPassDoor, boolean canUseNetherPortal)
    implements EntityAttributesInterface {

  public static final String CAN_OPEN_DOOR_TAG = MovementAttributeType.CAN_OPEN_DOOR.getTagName();
  public static final String CAN_PASS_DOOR_TAG = MovementAttributeType.CAN_PASS_DOOR.getTagName();
  public static final String CAN_CLOSE_DOOR_TAG = MovementAttributeType.CAN_CLOSE_DOOR.getTagName();
  public static final String CAN_USE_NETHER_PORTAL_TAG =
      MovementAttributeType.CAN_USE_NETHER_PORTAL.getTagName();

  public MovementAttributes() {
    this(false, false, false, false);
  }

  public static MovementAttributes decode(CompoundTag compoundTag) {
    return new MovementAttributes(
        compoundTag.getBoolean(CAN_OPEN_DOOR_TAG),
        compoundTag.getBoolean(CAN_CLOSE_DOOR_TAG),
        compoundTag.getBoolean(CAN_PASS_DOOR_TAG),
        compoundTag.getBoolean(CAN_USE_NETHER_PORTAL_TAG));
  }

  public MovementAttributes withCanOpenDoor(boolean canOpenDoor) {
    return new MovementAttributes(
        canOpenDoor, this.canCloseDoor, this.canPassDoor, this.canUseNetherPortal);
  }

  public MovementAttributes withCanCloseDoor(boolean canCloseDoor) {
    return new MovementAttributes(
        this.canOpenDoor, canCloseDoor, this.canPassDoor, this.canUseNetherPortal);
  }

  public MovementAttributes withCanPassDoor(boolean canPassDoor) {
    return new MovementAttributes(
        this.canOpenDoor, this.canCloseDoor, canPassDoor, this.canUseNetherPortal);
  }

  public MovementAttributes withCanUseNetherPortal(boolean canUseNetherPortal) {
    return new MovementAttributes(
        this.canOpenDoor, this.canCloseDoor, this.canPassDoor, canUseNetherPortal);
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    compoundTag.putBoolean(CAN_OPEN_DOOR_TAG, this.canOpenDoor);
    compoundTag.putBoolean(CAN_CLOSE_DOOR_TAG, this.canCloseDoor);
    compoundTag.putBoolean(CAN_PASS_DOOR_TAG, this.canPassDoor);
    compoundTag.putBoolean(CAN_USE_NETHER_PORTAL_TAG, this.canUseNetherPortal);
    return compoundTag;
  }
}
