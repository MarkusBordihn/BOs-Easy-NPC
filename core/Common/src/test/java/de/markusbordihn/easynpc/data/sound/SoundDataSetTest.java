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

package de.markusbordihn.easynpc.data.sound;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.sounds.SoundEvents;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SoundDataSetTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  @DisplayName("An empty sound set is not stored")
  void testEmptySoundSetIsNotStored() {
    CompoundTag compoundTag = new SoundDataSet().createTag();

    assertFalse(compoundTag.contains(SoundDataSet.DATA_SOUND_DATA_SET_TAG));
  }

  @Test
  @DisplayName("The sounds of the variant are not stored as own sounds")
  void testDefaultSoundsAreNotStored() {
    SoundDataSet soundDataSet = new SoundDataSet();
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.VILLAGER_HURT);

    CompoundTag compoundTag = soundDataSet.createTag();

    assertFalse(compoundTag.contains(SoundDataSet.DATA_SOUND_DATA_SET_TAG));
    assertTrue(soundDataSet.hasSound(SoundType.AMBIENT));
  }

  @Test
  @DisplayName("An own sound survives a round trip")
  void testOwnSoundSurvivesRoundTrip() {
    SoundDataSet soundDataSet = new SoundDataSet();
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);
    soundDataSet.addDefaultSound(SoundType.HURT, SoundEvents.VILLAGER_HURT);
    soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.CAT_AMBIENT);

    SoundDataSet restored = new SoundDataSet();
    restored.addDefaultSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);
    restored.addDefaultSound(SoundType.HURT, SoundEvents.VILLAGER_HURT);
    restored.load(soundDataSet.createTag());

    assertEquals(
        SoundEvents.CAT_AMBIENT.location(),
        restored.getSound(SoundType.AMBIENT).getSoundEvent().location());
    assertEquals(
        SoundEvents.VILLAGER_HURT.location(),
        restored.getSound(SoundType.HURT).getSoundEvent().location());
  }

  @Test
  @DisplayName("A sound set back on the sounds of its variant is stored no more")
  void testSoundBackOnDefaultIsNotStored() {
    SoundDataSet soundDataSet = new SoundDataSet();
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);
    soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.CAT_AMBIENT);
    soundDataSet.addSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);

    assertFalse(soundDataSet.createTag().contains(SoundDataSet.DATA_SOUND_DATA_SET_TAG));
  }

  @Test
  @DisplayName("An NPC without stored sounds keeps the sounds of its variant")
  void testMissingSoundTagKeepsDefaultSounds() {
    SoundDataSet soundDataSet = new SoundDataSet();
    soundDataSet.addDefaultSound(SoundType.AMBIENT, SoundEvents.VILLAGER_AMBIENT);

    soundDataSet.load(new CompoundTag());

    assertEquals(
        SoundEvents.VILLAGER_AMBIENT.location(),
        soundDataSet.getSound(SoundType.AMBIENT).getSoundEvent().location());
  }
}
