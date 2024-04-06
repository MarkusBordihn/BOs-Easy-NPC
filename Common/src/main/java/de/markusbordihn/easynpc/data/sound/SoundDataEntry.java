/*
 * Copyright 2023 Markus Bordihn
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

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;

public class SoundDataEntry {

  public static final String DATA_SOUND_TYPE = "Type";
  public static final String DATA_SOUND_NAME_TAG = "Name";
  public static final String DATA_SOUND_VOLUME_TAG = "Volume";
  public static final String DATA_SOUND_PITCH_TAG = "Pitch";
  public static final String DATA_SOUND_ENABLED_TAG = "Enabled";

  public static final float DEFAULT_VOLUME = 0.75F;
  public static final float DEFAULT_PITCH = 1.0F;
  public static final boolean DEFAULT_ENABLED = true;

  private SoundType type;
  private float volume = DEFAULT_VOLUME;
  private float pitch = DEFAULT_PITCH;
  private boolean enabled = DEFAULT_ENABLED;
  private SoundEvent soundEvent;

  public SoundDataEntry(SoundType type, ResourceLocation location) {
    this(type, location, DEFAULT_VOLUME, DEFAULT_PITCH, DEFAULT_ENABLED);
  }

  public SoundDataEntry(
      SoundType type, ResourceLocation location, float volume, float pitch, boolean enabled) {
    this.type = type;
    this.volume = volume;
    this.pitch = pitch;
    this.enabled = enabled;
    this.soundEvent =
        BuiltInRegistries.SOUND_EVENT
            .getOptional(location)
            .orElseGet(() -> SoundEvent.createVariableRangeEvent(location));
  }

  public SoundDataEntry(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public SoundType getType() {
    return type;
  }

  public float getVolume() {
    return volume;
  }

  public float getPitch() {
    return pitch;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public SoundEvent getSoundEvent() {
    return soundEvent;
  }

  public void load(CompoundTag compoundTag) {
    this.type = SoundType.valueOf(compoundTag.getString(DATA_SOUND_TYPE));
    this.soundEvent =
        SoundEvent.createVariableRangeEvent(
            new ResourceLocation(compoundTag.getString(DATA_SOUND_NAME_TAG)));
    if (compoundTag.contains(DATA_SOUND_VOLUME_TAG)) {
      this.volume = compoundTag.getFloat(DATA_SOUND_VOLUME_TAG);
    }
    if (compoundTag.contains(DATA_SOUND_PITCH_TAG)) {
      this.pitch = compoundTag.getFloat(DATA_SOUND_PITCH_TAG);
    }
    if (compoundTag.contains(DATA_SOUND_ENABLED_TAG)) {
      this.enabled = compoundTag.getBoolean(DATA_SOUND_ENABLED_TAG);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_SOUND_TYPE, this.type.name());
    compoundTag.putString(DATA_SOUND_NAME_TAG, this.soundEvent.getLocation().toString());
    if (this.volume != DEFAULT_VOLUME) {
      compoundTag.putFloat(DATA_SOUND_VOLUME_TAG, this.volume);
    }
    if (this.pitch != DEFAULT_PITCH) {
      compoundTag.putFloat(DATA_SOUND_PITCH_TAG, this.pitch);
    }
    if (this.enabled != DEFAULT_ENABLED) {
      compoundTag.putBoolean(DATA_SOUND_ENABLED_TAG, this.enabled);
    }
    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }
}
