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

package de.markusbordihn.easynpc.data.progression;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ProgressionDataTest {

  @Test
  @DisplayName("An unchanged progression is not stored at all")
  void testDefaultProgressionIsNotStored() {
    CompoundTag compoundTag = new ProgressionData().encode(new CompoundTag());

    assertFalse(compoundTag.contains(ProgressionData.DATA_PROGRESSION_TAG));
  }

  @Test
  @DisplayName("A missing progression falls back to experience level one")
  void testMissingProgressionKeepsDefaults() {
    ProgressionData progressionData = ProgressionData.decode(new CompoundTag());

    assertEquals(ProgressionData.DEFAULT_EXPERIENCE, progressionData.experience());
    assertEquals(ProgressionData.DEFAULT_EXPERIENCE_LEVEL, progressionData.experienceLevel());
    assertFalse(progressionData.attributeScalingEnabled());
  }

  @Test
  @DisplayName("An empty progression falls back to experience level one")
  void testEmptyProgressionKeepsDefaults() {
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(ProgressionData.DATA_PROGRESSION_TAG, new CompoundTag());

    ProgressionData progressionData = ProgressionData.decode(compoundTag);

    assertEquals(ProgressionData.DEFAULT_EXPERIENCE, progressionData.experience());
    assertEquals(ProgressionData.DEFAULT_EXPERIENCE_LEVEL, progressionData.experienceLevel());
  }

  @Test
  @DisplayName("A changed progression survives a round trip")
  void testChangedProgressionSurvivesRoundTrip() {
    ProgressionData progressionData =
        new ProgressionData()
            .withExperience(120)
            .withExperienceLevel(7)
            .withAttributeScalingEnabled(true);

    ProgressionData restored = ProgressionData.decode(progressionData.encode(new CompoundTag()));

    assertEquals(120, restored.experience());
    assertEquals(7, restored.experienceLevel());
    assertTrue(restored.attributeScalingEnabled());
  }

  @Test
  @DisplayName("A progression back on its default is stored no more")
  void testProgressionBackOnDefaultIsNotStored() {
    ProgressionData progressionData = new ProgressionData().withExperience(120).withExperience(1);

    CompoundTag compoundTag = progressionData.encode(new CompoundTag());

    assertFalse(compoundTag.contains(ProgressionData.DATA_PROGRESSION_TAG));
  }

  @Test
  @DisplayName("Zero experience is kept, so it is not read back as the default")
  void testZeroExperienceIsStored() {
    CompoundTag compoundTag = new ProgressionData().withExperience(0).encode(new CompoundTag());

    assertEquals(0, ProgressionData.decode(compoundTag).experience());
  }
}
