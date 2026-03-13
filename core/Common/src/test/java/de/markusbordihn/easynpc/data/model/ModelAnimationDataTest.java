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

package de.markusbordihn.easynpc.data.model;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("ModelAnimationData Tests")
class ModelAnimationDataTest {

  @Test
  @DisplayName("Default constructor should use SMART behavior")
  void testDefaultConstructorIsSmart() {
    ModelAnimationData data = new ModelAnimationData();
    assertEquals(ModelAnimationBehavior.SMART, data.behavior());
  }

  @Test
  @DisplayName("hasChanged() should be false for SMART")
  void testHasChangedFalseForSmart() {
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.SMART);
    assertFalse(data.hasChanged());
  }

  @Test
  @DisplayName("hasChanged() should be true for DEFAULT behavior")
  void testHasChangedTrueForDefault() {
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.DEFAULT);
    assertTrue(data.hasChanged());
  }

  @Test
  @DisplayName("hasChanged() should be true for NONE behavior")
  void testHasChangedTrueForNone() {
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.NONE);
    assertTrue(data.hasChanged());
  }

  @Test
  @DisplayName("Two ModelAnimationData with the same behavior should be equal")
  void testEqualityBySameBehavior() {
    ModelAnimationData d1 = new ModelAnimationData(ModelAnimationBehavior.NONE);
    ModelAnimationData d2 = new ModelAnimationData(ModelAnimationBehavior.NONE);
    assertEquals(d1, d2);
  }

  @Test
  @DisplayName("Two ModelAnimationData with different behaviors should not be equal")
  void testInequalityByDifferentBehavior() {
    ModelAnimationData smart = new ModelAnimationData(ModelAnimationBehavior.SMART);
    ModelAnimationData none = new ModelAnimationData(ModelAnimationBehavior.NONE);
    assertNotEquals(smart, none);
  }

  @Test
  @DisplayName("DEFAULT constant should use SMART behavior and be unchanged")
  void testDefaultConstantIsSmart() {
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationData.DEFAULT.behavior());
    assertFalse(ModelAnimationData.DEFAULT.hasChanged());
  }
}
