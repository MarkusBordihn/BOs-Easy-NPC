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

import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Locale;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.sounds.SoundSource;

public record SoundActionData(String soundId, SoundSource soundSource, float volume, float pitch) {

  public static final String DATA_SOUND_TAG = "Sound";
  public static final String DATA_SOURCE_TAG = "Src";
  public static final String DATA_VOLUME_TAG = "Vol";
  public static final String DATA_PITCH_TAG = "Pitch";
  public static final float DEFAULT_VOLUME = 1.0F;
  public static final float DEFAULT_PITCH = 1.0F;
  public static final float MIN_VOLUME = 0.0F;
  public static final float MAX_VOLUME = 10.0F;
  public static final float MIN_PITCH = 0.5F;
  public static final float MAX_PITCH = 2.0F;
  public static final SoundActionData DEFAULT =
      new SoundActionData("", SoundSource.NEUTRAL, DEFAULT_VOLUME, DEFAULT_PITCH);

  public SoundActionData {
    soundId = soundId == null ? "" : soundId.trim().toLowerCase(Locale.ROOT);
    soundSource = soundSource == null ? SoundSource.NEUTRAL : soundSource;
    volume = Math.min(MAX_VOLUME, Math.max(MIN_VOLUME, volume));
    pitch = Math.min(MAX_PITCH, Math.max(MIN_PITCH, pitch));
  }

  public SoundActionData(String soundId) {
    this(soundId, SoundSource.NEUTRAL, DEFAULT_VOLUME, DEFAULT_PITCH);
  }

  public static SoundActionData fromTag(CompoundTag compoundTag) {
    if (compoundTag == null || compoundTag.isEmpty()) {
      return DEFAULT;
    }

    return new SoundActionData(
        compoundTag.getStringOr(DATA_SOUND_TAG, ""),
        EnumUtils.getIgnoreCase(
            SoundSource.class, compoundTag.getStringOr(DATA_SOURCE_TAG, ""), SoundSource.NEUTRAL),
        compoundTag.getFloatOr(DATA_VOLUME_TAG, DEFAULT_VOLUME),
        compoundTag.getFloatOr(DATA_PITCH_TAG, DEFAULT_PITCH));
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    if (!this.soundId.isEmpty()) {
      compoundTag.putString(DATA_SOUND_TAG, this.soundId);
    }
    if (this.soundSource != SoundSource.NEUTRAL) {
      compoundTag.putString(DATA_SOURCE_TAG, this.soundSource.name());
    }
    if (this.volume != DEFAULT_VOLUME) {
      compoundTag.putFloat(DATA_VOLUME_TAG, this.volume);
    }
    if (this.pitch != DEFAULT_PITCH) {
      compoundTag.putFloat(DATA_PITCH_TAG, this.pitch);
    }
    return compoundTag;
  }

  public boolean hasSoundId() {
    return !this.soundId.isEmpty() && Identifier.tryParse(this.soundId) != null;
  }

  public Identifier getSoundLocation() {
    return this.soundId.isEmpty() ? null : Identifier.tryParse(this.soundId);
  }

  public SoundActionData withSoundId(String soundId) {
    return new SoundActionData(soundId, this.soundSource, this.volume, this.pitch);
  }

  public SoundActionData withSoundSource(SoundSource soundSource) {
    return new SoundActionData(this.soundId, soundSource, this.volume, this.pitch);
  }

  public SoundActionData withVolume(float volume) {
    return new SoundActionData(this.soundId, this.soundSource, volume, this.pitch);
  }

  public SoundActionData withPitch(float pitch) {
    return new SoundActionData(this.soundId, this.soundSource, this.volume, pitch);
  }
}
