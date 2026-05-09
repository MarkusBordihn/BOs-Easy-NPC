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

package de.markusbordihn.easynpc.data.rotation;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CustomRotationTest {

  @Test
  @DisplayName("hasChanged() should be false for (0, 0, 0)")
  void testHasChangedFalseForZero() {
    assertFalse(new CustomRotation(0f, 0f, 0f).hasChanged());
  }

  @Test
  void testHasChangedTrueForNonZeroX() {
    assertTrue(new CustomRotation(1f, 0f, 0f).hasChanged());
    assertTrue(new CustomRotation(-0.5f, 0f, 0f).hasChanged());
  }

  @Test
  void testHasChangedTrueForNonZeroY() {
    assertTrue(new CustomRotation(0f, 1f, 0f).hasChanged());
  }

  @Test
  void testHasChangedTrueForNonZeroZ() {
    assertTrue(new CustomRotation(0f, 0f, 1f).hasChanged());
  }

  @Test
  @DisplayName("hasChangedYaw() should be true only for (x=0, y!=0, z=0) with locked=true")
  void testHasChangedYawTrueForLockedYawOnly() {
    assertTrue(new CustomRotation(0f, 45f, 0f, true).hasChangedYaw());
    assertTrue(new CustomRotation(0f, 180f, 0f, true).hasChangedYaw());
  }

  @Test
  void testHasChangedYawFalseWhenNotLocked() {
    assertFalse(new CustomRotation(0f, 45f, 0f, false).hasChangedYaw());
  }

  @Test
  void testHasChangedYawFalseWhenXOrZNonZero() {
    assertFalse(new CustomRotation(1f, 45f, 0f, true).hasChangedYaw());
    assertFalse(new CustomRotation(0f, 45f, 1f, true).hasChangedYaw());
    assertFalse(new CustomRotation(1f, 45f, 1f, true).hasChangedYaw());
  }

  @Test
  void testHasChangedYawFalseWhenYIsZero() {
    assertFalse(new CustomRotation(0f, 0f, 0f, true).hasChangedYaw());
  }

  @Test
  @DisplayName("withLocked(true) should preserve x, y, z and set locked=true")
  void testWithLockedTrue() {
    CustomRotation original = new CustomRotation(1f, 2f, 3f, false);
    CustomRotation locked = original.withLocked(true);
    assertEquals(1f, locked.x());
    assertEquals(2f, locked.y());
    assertEquals(3f, locked.z());
    assertTrue(locked.locked());
  }

  @Test
  @DisplayName("withLocked(false) should preserve x, y, z and set locked=false")
  void testWithLockedFalse() {
    CustomRotation original = new CustomRotation(1f, 2f, 3f, true);
    CustomRotation unlocked = original.withLocked(false);
    assertEquals(1f, unlocked.x());
    assertEquals(2f, unlocked.y());
    assertEquals(3f, unlocked.z());
    assertFalse(unlocked.locked());
  }

  @Test
  @DisplayName("withLocked() should produce a new instance, not mutate the original (immutability)")
  void testWithLockedIsImmutable() {
    CustomRotation original = new CustomRotation(5f, 10f, 15f, false);
    CustomRotation locked = original.withLocked(true);
    assertNotSame(original, locked);
    assertFalse(original.locked(), "original must remain unlocked");
    assertTrue(locked.locked());
  }

  @Test
  void testThreeArgConstructorSetsLockedFalse() {
    CustomRotation r = new CustomRotation(5f, 10f, 15f);
    assertFalse(r.locked());
    assertEquals(5f, r.x());
    assertEquals(10f, r.y());
    assertEquals(15f, r.z());
  }

  @Test
  @DisplayName("DEFAULT constant should be (0, 0, 0, false) and report hasChanged()=false")
  void testDefaultConstant() {
    assertEquals(0f, CustomRotation.DEFAULT.x());
    assertEquals(0f, CustomRotation.DEFAULT.y());
    assertEquals(0f, CustomRotation.DEFAULT.z());
    assertFalse(CustomRotation.DEFAULT.locked());
    assertFalse(CustomRotation.DEFAULT.hasChanged());
  }

  @Test
  @DisplayName("hasChanged(x,y,z) should be false when rotation matches the reference")
  void testHasChangedOverloadFalseWhenMatches() {
    assertFalse(new CustomRotation(1f, 2f, 3f).hasChanged(1f, 2f, 3f));
  }

  @Test
  @DisplayName("hasChanged(x,y,z) should be true when any component differs from the reference")
  void testHasChangedOverloadTrueWhenDiffers() {
    assertTrue(new CustomRotation(1f, 2f, 3f).hasChanged(0f, 2f, 3f));
    assertTrue(new CustomRotation(1f, 2f, 3f).hasChanged(1f, 0f, 3f));
    assertTrue(new CustomRotation(1f, 2f, 3f).hasChanged(1f, 2f, 0f));
  }

  @Test
  @DisplayName("hasChanged(x,y,z,locked) should be false when all four components match")
  void testHasChangedLockedOverloadFalseWhenAllMatch() {
    assertFalse(new CustomRotation(1f, 2f, 3f, true).hasChanged(1f, 2f, 3f, true));
    assertFalse(new CustomRotation(1f, 2f, 3f, false).hasChanged(1f, 2f, 3f, false));
  }

  @Test
  void testHasChangedLockedOverloadTrueWhenOnlyLockedDiffers() {
    assertTrue(new CustomRotation(1f, 2f, 3f, true).hasChanged(1f, 2f, 3f, false));
    assertTrue(new CustomRotation(1f, 2f, 3f, false).hasChanged(1f, 2f, 3f, true));
  }

  @Test
  void testHasChangedLockedOverloadTrueWhenFloatDiffers() {
    assertTrue(new CustomRotation(1f, 2f, 3f, true).hasChanged(0f, 2f, 3f, true));
    assertTrue(new CustomRotation(1f, 2f, 3f, true).hasChanged(1f, 0f, 3f, true));
    assertTrue(new CustomRotation(1f, 2f, 3f, true).hasChanged(1f, 2f, 0f, true));
  }

  @Test
  @DisplayName("Two CustomRotations with the same components should be equal (record semantics)")
  void testEqualityBySameComponents() {
    CustomRotation r1 = new CustomRotation(1.5f, 0f, 0f, false);
    CustomRotation r2 = new CustomRotation(1.5f, 0f, 0f, false);
    assertEquals(r1, r2);
  }

  @Test
  void testInequalityByLockedFlag() {
    CustomRotation unlocked = new CustomRotation(1.5f, 0f, 0f, false);
    CustomRotation locked = new CustomRotation(1.5f, 0f, 0f, true);
    assertNotEquals(unlocked, locked);
  }
}
