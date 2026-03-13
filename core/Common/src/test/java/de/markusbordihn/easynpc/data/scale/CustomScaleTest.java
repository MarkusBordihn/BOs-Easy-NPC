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

package de.markusbordihn.easynpc.data.scale;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("CustomScale Tests")
class CustomScaleTest {

  @Test
  @DisplayName("hasChanged() should be false for (1, 1, 1)")
  void testHasChangedFalseForUniform1() {
    assertFalse(new CustomScale(1f, 1f, 1f).hasChanged());
  }

  @Test
  @DisplayName("hasChanged() should be true for (0, 0, 0)")
  void testHasChangedTrueForZeroScale() {
    assertTrue(new CustomScale(0f, 0f, 0f).hasChanged());
  }

  @Test
  @DisplayName("hasChanged() should be true for any scale other than (1,1,1)")
  void testHasChangedTrueForNonDefault() {
    assertTrue(new CustomScale(2f, 1f, 1f).hasChanged());
    assertTrue(new CustomScale(1f, 2f, 1f).hasChanged());
    assertTrue(new CustomScale(1f, 1f, 2f).hasChanged());
    assertTrue(new CustomScale(0.5f, 0.5f, 0.5f).hasChanged());
  }

  @Test
  @DisplayName("Single-float constructor should set x=y=z to the given value")
  void testSingleFloatConstructorUniform() {
    CustomScale scale = new CustomScale(2f);
    assertEquals(2f, scale.x());
    assertEquals(2f, scale.y());
    assertEquals(2f, scale.z());
  }

  @Test
  @DisplayName("Single-float constructor with 1.0 should report hasChanged()=false")
  void testSingleFloatConstructorOneIsNotChanged() {
    assertFalse(new CustomScale(1f).hasChanged());
  }

  @Test
  @DisplayName("Single-float constructor with non-1 value should report hasChanged()=true")
  void testSingleFloatConstructorNonOneIsChanged() {
    assertTrue(new CustomScale(2f).hasChanged());
    assertTrue(new CustomScale(0f).hasChanged());
  }

  @Test
  @DisplayName("hasChanged(x,y,z) should be false when scale matches the given reference")
  void testHasChangedOverloadFalseWhenMatches() {
    assertFalse(new CustomScale(1.5f, 2f, 0.5f).hasChanged(1.5f, 2f, 0.5f));
  }

  @Test
  @DisplayName("hasChanged(x,y,z) should be true when any component differs from reference")
  void testHasChangedOverloadTrueWhenDiffers() {
    assertTrue(new CustomScale(1.5f, 2f, 0.5f).hasChanged(1f, 2f, 0.5f));
    assertTrue(new CustomScale(1.5f, 2f, 0.5f).hasChanged(1.5f, 1f, 0.5f));
    assertTrue(new CustomScale(1.5f, 2f, 0.5f).hasChanged(1.5f, 2f, 1f));
  }

  @Test
  @DisplayName("DEFAULT constant should be (1, 1, 1) and report hasChanged()=false")
  void testDefaultConstant() {
    assertEquals(1f, CustomScale.DEFAULT.x());
    assertEquals(1f, CustomScale.DEFAULT.y());
    assertEquals(1f, CustomScale.DEFAULT.z());
    assertFalse(CustomScale.DEFAULT.hasChanged());
  }

  @Test
  @DisplayName("Two CustomScales with the same components should be equal (record semantics)")
  void testEqualityBySameComponents() {
    CustomScale s1 = new CustomScale(1.5f, 2f, 0.5f);
    CustomScale s2 = new CustomScale(1.5f, 2f, 0.5f);
    assertEquals(s1, s2);
  }

  @Test
  @DisplayName("Two CustomScales with different components should not be equal")
  void testInequalityByDifferentComponents() {
    assertNotEquals(new CustomScale(2f, 1f, 1f), new CustomScale(1f, 1f, 1f));
  }
}
