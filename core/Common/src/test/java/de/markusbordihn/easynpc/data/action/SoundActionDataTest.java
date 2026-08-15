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

package de.markusbordihn.easynpc.data.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.sounds.SoundSource;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SoundActionDataTest {

  @Test
  @DisplayName("Every sound source survives a tag round trip")
  void testSoundSourceRoundTrip() {
    for (SoundSource soundSource : SoundSource.values()) {
      SoundActionData soundActionData =
          new SoundActionData("minecraft:entity.villager.yes", soundSource, 0.75F, 1.5F);

      assertEquals(
          soundActionData,
          SoundActionData.fromTag(soundActionData.createTag()),
          soundSource.name());
    }
  }

  @Test
  @DisplayName("A missing or empty tag falls back to the default")
  void testFromEmptyTag() {
    assertEquals(SoundActionData.DEFAULT, SoundActionData.fromTag(null));
    assertEquals(SoundActionData.DEFAULT, SoundActionData.fromTag(new CompoundTag()));
  }

  @Test
  @DisplayName("The default is not stored")
  void testDefaultIsNotStored() {
    assertTrue(SoundActionData.DEFAULT.createTag().isEmpty());
  }

  @Test
  @DisplayName("Volume and pitch are clamped to their supported range")
  void testVolumeAndPitchClamping() {
    SoundActionData tooLoud = new SoundActionData("minecraft:ui.button.click", null, 25.0F, 9.0F);
    assertEquals(SoundActionData.MAX_VOLUME, tooLoud.volume());
    assertEquals(SoundActionData.MAX_PITCH, tooLoud.pitch());
    assertEquals(SoundSource.NEUTRAL, tooLoud.soundSource());

    SoundActionData tooQuiet = new SoundActionData("minecraft:ui.button.click", null, -5.0F, 0.0F);
    assertEquals(SoundActionData.MIN_VOLUME, tooQuiet.volume());
    assertEquals(SoundActionData.MIN_PITCH, tooQuiet.pitch());
  }

  @Test
  @DisplayName("Only a parsable sound id is accepted")
  void testSoundIdValidation() {
    assertTrue(new SoundActionData("minecraft:entity.villager.yes").hasSoundId());
    assertTrue(new SoundActionData("  Entity.Villager.Yes  ").hasSoundId());
    assertFalse(new SoundActionData("").hasSoundId());
    assertFalse(new SoundActionData("not a sound id").hasSoundId());
  }
}
