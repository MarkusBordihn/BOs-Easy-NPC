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

package de.markusbordihn.easynpc.data.position;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class CustomPositionTest {

  @Test
  @DisplayName("hasChanged() should be false for (0, 0, 0)")
  void testHasChangedFalseForZero() {
    assertFalse(new CustomPosition(0f, 0f, 0f).hasChanged());
  }

  @Test
  void testHasChangedTrueForNonZeroX() {
    assertTrue(new CustomPosition(1f, 0f, 0f).hasChanged());
    assertTrue(new CustomPosition(-0.5f, 0f, 0f).hasChanged());
  }

  @Test
  void testHasChangedTrueForNonZeroY() {
    assertTrue(new CustomPosition(0f, 1f, 0f).hasChanged());
  }

  @Test
  void testHasChangedTrueForNonZeroZ() {
    assertTrue(new CustomPosition(0f, 0f, 1f).hasChanged());
  }

  @Test
  @DisplayName("hasChanged(x,y,z) should be false when position matches the given reference")
  void testHasChangedOverloadFalseWhenMatches() {
    assertFalse(new CustomPosition(1f, 2f, 3f).hasChanged(1f, 2f, 3f));
  }

  @Test
  @DisplayName("hasChanged(x,y,z) should be true when any component differs from reference")
  void testHasChangedOverloadTrueWhenDiffers() {
    assertTrue(new CustomPosition(1f, 2f, 3f).hasChanged(0f, 2f, 3f));
    assertTrue(new CustomPosition(1f, 2f, 3f).hasChanged(1f, 0f, 3f));
    assertTrue(new CustomPosition(1f, 2f, 3f).hasChanged(1f, 2f, 0f));
  }

  @Test
  @DisplayName("DEFAULT constant should be (0, 0, 0) and report hasChanged()=false")
  void testDefaultConstant() {
    assertEquals(0f, CustomPosition.DEFAULT.x());
    assertEquals(0f, CustomPosition.DEFAULT.y());
    assertEquals(0f, CustomPosition.DEFAULT.z());
    assertFalse(CustomPosition.DEFAULT.hasChanged());
  }

  @Test
  @DisplayName("Two CustomPositions with the same components should be equal (record semantics)")
  void testEqualityBySameComponents() {
    CustomPosition p1 = new CustomPosition(1f, 2f, 3f);
    CustomPosition p2 = new CustomPosition(1f, 2f, 3f);
    assertEquals(p1, p2);
  }

  @Test
  void testInequalityByDifferentComponents() {
    assertNotEquals(new CustomPosition(1f, 0f, 0f), new CustomPosition(0f, 0f, 0f));
    assertNotEquals(new CustomPosition(0f, 1f, 0f), new CustomPosition(0f, 0f, 0f));
    assertNotEquals(new CustomPosition(0f, 0f, 1f), new CustomPosition(0f, 0f, 0f));
  }
}
