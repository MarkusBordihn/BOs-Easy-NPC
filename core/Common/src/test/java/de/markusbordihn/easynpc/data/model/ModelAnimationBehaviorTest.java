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

@DisplayName("ModelAnimationBehavior Tests")
class ModelAnimationBehaviorTest {

  @Test
  @DisplayName("get() should return the correct enum value for valid names")
  void testGetValidNames() {
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get("SMART"));
    assertEquals(ModelAnimationBehavior.DEFAULT, ModelAnimationBehavior.get("DEFAULT"));
    assertEquals(ModelAnimationBehavior.NONE, ModelAnimationBehavior.get("NONE"));
  }

  @Test
  @DisplayName("get() should return SMART for null input")
  void testGetNull() {
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get(null));
  }

  @Test
  @DisplayName("get() should return SMART for empty string")
  void testGetEmpty() {
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get(""));
  }

  @Test
  @DisplayName("get() should return SMART for unrecognized names")
  void testGetUnknown() {
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get("UNKNOWN"));
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get("none"));
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get("smart"));
    assertEquals(ModelAnimationBehavior.SMART, ModelAnimationBehavior.get("default"));
  }

  @Test
  @DisplayName("Each enum variant should round-trip through get(name())")
  void testRoundTrip() {
    for (ModelAnimationBehavior behavior : ModelAnimationBehavior.values()) {
      assertEquals(
          behavior,
          ModelAnimationBehavior.get(behavior.name()),
          "Round-trip failed for " + behavior);
    }
  }
}
