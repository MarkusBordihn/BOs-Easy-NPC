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

package de.markusbordihn.easynpc.api.animation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class ModelAnimationAPITest {

  @Test
  void keepsStandardAnimationsUnprefixed() {
    assertTrue(ModelAnimationAPI.standardAnimations().contains("sit"));
    assertEquals("sit", ModelAnimationAPI.normalizeAnimationName("Sit"));
    assertEquals("idle", ModelAnimationAPI.normalizeAnimationName(" idle "));
  }

  @Test
  void prefixesCustomAnimations() {
    assertEquals("named:wave", ModelAnimationAPI.normalizeAnimationName("wave"));
    assertEquals("named:wave", ModelAnimationAPI.normalizeAnimationName("named:Wave"));
  }

  @Test
  void rejectsEmptyAndOversizedAnimationNames() {
    assertEquals("", ModelAnimationAPI.normalizeAnimationName(null));
    assertEquals("", ModelAnimationAPI.normalizeAnimationName("  "));
    assertEquals("", ModelAnimationAPI.normalizeAnimationName("named:"));
    assertEquals(
        "",
        ModelAnimationAPI.normalizeAnimationName(
            "a".repeat(ModelAnimationAPI.MAX_ANIMATION_NAME_LENGTH + 1)));
  }

  @Test
  void animationInfoDerivesVariantFromBaseName() {
    ModelAnimationInfo variant = new ModelAnimationInfo("wave_2", "wave", 20.0F, false, 20, 4, 3);
    ModelAnimationInfo base = new ModelAnimationInfo("wave", null, 20.0F, false, 20, 4, 3);

    assertTrue(variant.isVariant());
    assertFalse(base.isVariant());
    assertEquals("wave", base.baseName());
  }
}
