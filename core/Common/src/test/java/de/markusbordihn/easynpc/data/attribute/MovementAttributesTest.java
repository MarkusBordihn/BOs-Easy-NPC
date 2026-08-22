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

package de.markusbordihn.easynpc.data.attribute;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class MovementAttributesTest {

  @Test
  @DisplayName("All movement attributes survive an encode and decode round trip")
  void testEncodeDecodeRoundTrip() {
    MovementAttributes movementAttributes =
        new MovementAttributes(
            true, false, true, false, NavigationType.AQUATIC, 2.5D, 4.0D, 1.5D, true);

    MovementAttributes decoded =
        MovementAttributes.decode(movementAttributes.encode(new CompoundTag()));

    assertEquals(movementAttributes, decoded);
    assertTrue(decoded.isImmovable());
  }

  @Test
  @DisplayName("Data saved before the immovable attribute existed keeps the NPC movable")
  void testLegacyDataDecodesToMovable() {
    CompoundTag legacyTag = new CompoundTag();
    legacyTag.putBoolean(MovementAttributes.CAN_OPEN_DOOR_TAG, true);

    assertFalse(MovementAttributes.decode(legacyTag).isImmovable());
    assertFalse(
        new MovementAttributes()
            .encode(new CompoundTag())
            .contains(MovementAttributes.IS_IMMOVABLE_TAG));
  }

  @Test
  @DisplayName("Data saved before the navigation type existed keeps the entity default")
  void testLegacyDataDecodesToDefault() {
    CompoundTag legacyTag = new CompoundTag();
    legacyTag.putBoolean(MovementAttributes.CAN_OPEN_DOOR_TAG, true);

    MovementAttributes decoded = MovementAttributes.decode(legacyTag);

    assertTrue(decoded.canOpenDoor());
    assertEquals(NavigationType.DEFAULT, decoded.navigationType());
    assertEquals(0.0D, decoded.hoverHeight());
  }

  @Test
  @DisplayName("An unknown navigation type falls back to the entity default")
  void testUnknownNavigationTypeDecodesToDefault() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(MovementAttributes.NAVIGATION_TYPE_TAG, "TELEPORTING");

    assertEquals(NavigationType.DEFAULT, MovementAttributes.decode(compoundTag).navigationType());
  }

  @Test
  @DisplayName("An explicitly chosen ground navigation is kept")
  void testExplicitGroundNavigationIsKept() {
    MovementAttributes movementAttributes =
        new MovementAttributes().withNavigationType(NavigationType.GROUND);

    MovementAttributes decoded =
        MovementAttributes.decode(movementAttributes.encode(new CompoundTag()));

    assertEquals(NavigationType.GROUND, decoded.navigationType());
  }

  @Test
  @DisplayName("The door copy methods keep the navigation type and the hover height")
  void testDoorCopyMethodsKeepNavigationValues() {
    MovementAttributes movementAttributes =
        new MovementAttributes()
            .withNavigationType(NavigationType.FLYING)
            .withHoverHeight(3.0D)
            .withCanOpenDoor(true)
            .withCanCloseDoor(true)
            .withCanPassDoor(true)
            .withCanUseNetherPortal(true);

    assertEquals(NavigationType.FLYING, movementAttributes.navigationType());
    assertEquals(3.0D, movementAttributes.hoverHeight());
  }

  @Test
  @DisplayName("A hover height outside the allowed range is capped instead of moving the NPC away")
  void testHoverHeightIsLimited() {
    assertEquals(
        MovementAttributes.MAX_HOVER_HEIGHT,
        new MovementAttributes().withHoverHeight(1024.0D).hoverHeight());
    assertEquals(0.0D, new MovementAttributes().withHoverHeight(-5.0D).hoverHeight());
    assertEquals(0.0D, new MovementAttributes().withHoverHeight(Double.NaN).hoverHeight());
    assertEquals(
        MovementAttributes.MAX_HOVER_HEIGHT,
        new MovementAttributes().withHoverHeight(Double.POSITIVE_INFINITY).hoverHeight());
  }

  @Test
  @DisplayName("A hover height outside the allowed range is also capped while loading")
  void testHoverHeightIsLimitedOnDecode() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putDouble(MovementAttributes.HOVER_HEIGHT_TAG, Double.NaN);

    assertEquals(0.0D, MovementAttributes.decode(compoundTag).hoverHeight());
  }

  @Test
  @DisplayName("A swim distance outside the allowed range is capped")
  void testSwimDistancesAreLimited() {
    assertEquals(
        MovementAttributes.MAX_SWIM_DEPTH_BELOW_SURFACE,
        new MovementAttributes().withSwimDepthBelowSurface(1024.0D).swimDepthBelowSurface());
    assertEquals(
        0.0D, new MovementAttributes().withSwimDepthBelowSurface(-5.0D).swimDepthBelowSurface());
    assertEquals(
        0.0D,
        new MovementAttributes().withSwimDepthBelowSurface(Double.NaN).swimDepthBelowSurface());
    assertEquals(
        MovementAttributes.MAX_SWIM_HEIGHT_ABOVE_FLOOR,
        new MovementAttributes().withSwimHeightAboveFloor(1024.0D).swimHeightAboveFloor());
    assertEquals(
        0.0D, new MovementAttributes().withSwimHeightAboveFloor(-5.0D).swimHeightAboveFloor());
    assertEquals(
        MovementAttributes.MAX_SWIM_HEIGHT_ABOVE_FLOOR,
        new MovementAttributes()
            .withSwimHeightAboveFloor(Double.POSITIVE_INFINITY)
            .swimHeightAboveFloor());
  }

  @Test
  @DisplayName("A swim distance outside the allowed range is also capped while loading")
  void testSwimDistancesAreLimitedOnDecode() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putDouble(MovementAttributes.SWIM_DEPTH_BELOW_SURFACE_TAG, Double.NaN);
    compoundTag.putDouble(MovementAttributes.SWIM_HEIGHT_ABOVE_FLOOR_TAG, 1024.0D);

    MovementAttributes decoded = MovementAttributes.decode(compoundTag);

    assertEquals(0.0D, decoded.swimDepthBelowSurface());
    assertEquals(MovementAttributes.MAX_SWIM_HEIGHT_ABOVE_FLOOR, decoded.swimHeightAboveFloor());
  }

  @Test
  @DisplayName("Data saved before the swim distances existed keeps them unset")
  void testLegacyDataDecodesWithoutSwimDistances() {
    CompoundTag legacyTag = new CompoundTag();
    legacyTag.putBoolean(MovementAttributes.CAN_OPEN_DOOR_TAG, true);

    MovementAttributes decoded = MovementAttributes.decode(legacyTag);

    assertEquals(0.0D, decoded.swimDepthBelowSurface());
    assertEquals(0.0D, decoded.swimHeightAboveFloor());
  }

  @Test
  @DisplayName("A null navigation type is normalized to the entity default")
  void testNullNavigationTypeIsNormalized() {
    assertEquals(
        NavigationType.DEFAULT, new MovementAttributes().withNavigationType(null).navigationType());
  }
}
