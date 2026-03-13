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

@DisplayName("ModelPose Tests")
class ModelPoseTest {

  @Test
  @DisplayName("get() should return the correct enum value for valid names")
  void testGetValidNames() {
    assertEquals(ModelPose.VANILLA, ModelPose.get("VANILLA"));
    assertEquals(ModelPose.DEFAULT, ModelPose.get("DEFAULT"));
    assertEquals(ModelPose.CUSTOM, ModelPose.get("CUSTOM"));
  }

  @Test
  @DisplayName("get() should return VANILLA for null input")
  void testGetNull() {
    assertEquals(ModelPose.VANILLA, ModelPose.get(null));
  }

  @Test
  @DisplayName("get() should return VANILLA for empty string")
  void testGetEmpty() {
    assertEquals(ModelPose.VANILLA, ModelPose.get(""));
  }

  @Test
  @DisplayName("get() should return VANILLA for unrecognized names")
  void testGetUnknown() {
    assertEquals(ModelPose.VANILLA, ModelPose.get("UNKNOWN_POSE"));
    assertEquals(ModelPose.VANILLA, ModelPose.get("custom"));
    assertEquals(ModelPose.VANILLA, ModelPose.get("vanilla"));
    assertEquals(ModelPose.VANILLA, ModelPose.get("default"));
  }

  @Test
  @DisplayName("Each enum variant should round-trip through get(name())")
  void testRoundTrip() {
    for (ModelPose pose : ModelPose.values()) {
      assertEquals(pose, ModelPose.get(pose.name()), "Round-trip failed for " + pose);
    }
  }
}
