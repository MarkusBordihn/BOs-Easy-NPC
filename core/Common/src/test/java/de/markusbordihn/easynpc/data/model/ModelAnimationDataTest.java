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

import org.junit.jupiter.api.Test;

class ModelAnimationDataTest {

  @Test
  void testDefaultConstructorIsSmart() {
    ModelAnimationData data = new ModelAnimationData();
    assertEquals(ModelAnimationBehavior.SMART, data.behavior());
  }

  @Test
  void testHasChangedFalseForSmart() {
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.SMART);
    assertFalse(data.hasChanged());
  }

  @Test
  void testHasChangedTrueForDefault() {
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.DEFAULT);
    assertTrue(data.hasChanged());
  }

  @Test
  void testHasChangedTrueForNone() {
    ModelAnimationData data = new ModelAnimationData(ModelAnimationBehavior.NONE);
    assertTrue(data.hasChanged());
  }

  @Test
  void testEqualityBySameBehavior() {
    ModelAnimationData d1 = new ModelAnimationData(ModelAnimationBehavior.NONE);
    ModelAnimationData d2 = new ModelAnimationData(ModelAnimationBehavior.NONE);
    assertEquals(d1, d2);
  }

  @Test
  void testInequalityByDifferentBehavior() {
    ModelAnimationData smart = new ModelAnimationData(ModelAnimationBehavior.SMART);
    ModelAnimationData none = new ModelAnimationData(ModelAnimationBehavior.NONE);
    assertNotEquals(smart, none);
  }

  @Test
  void testDefaultConstantIsSmart() {
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationData.DEFAULT.behavior());
    assertFalse(ModelAnimationData.DEFAULT.hasChanged());
  }
}
