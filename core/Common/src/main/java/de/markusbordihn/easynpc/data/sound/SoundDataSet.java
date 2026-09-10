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

import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.resources.Identifier;
import net.minecraft.sounds.SoundEvent;

public class SoundDataSet {

  public static final String DATA_SOUND_DATA_SET_TAG = "SoundDataSet";
  public static final String DATA_DEFAULT_SOUND_DATA_SET_TAG = "DefaultSoundDataSet";

  public static final StreamCodec<RegistryFriendlyByteBuf, SoundDataSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public SoundDataSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new SoundDataSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, SoundDataSet soundDataSet) {
          registryFriendlyByteBuf.writeNbt(
              EntityDataSerializersManager.validateAndGetNbt(
                  soundDataSet.createTag(), "SoundDataSet"));
        }
      };

  private final Map<SoundType, SoundDataEntry> defaultSounds = new EnumMap<>(SoundType.class);
  private final Map<SoundType, SoundDataEntry> overrideSounds = new EnumMap<>(SoundType.class);

  public SoundDataSet() {}

  public SoundDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public SoundDataSet(SoundDataSet soundDataSet) {
    this.defaultSounds.putAll(soundDataSet.defaultSounds);
    this.overrideSounds.putAll(soundDataSet.overrideSounds);
  }

  private static boolean isSameSound(SoundDataEntry soundDataEntry, SoundDataEntry otherEntry) {
    return soundDataEntry.getSoundEvent().equals(otherEntry.getSoundEvent())
        && soundDataEntry.getVolume() == otherEntry.getVolume()
        && soundDataEntry.getPitch() == otherEntry.getPitch()
        && soundDataEntry.isEnabled() == otherEntry.isEnabled();
  }

  public boolean hasSound(SoundType type) {
    return defaultSounds.containsKey(type) || overrideSounds.containsKey(type);
  }

  public void addSound(SoundType type, SoundEvent soundEvent) {
    this.addSound(type, soundEvent.location());
  }

  public void addSound(SoundType type, Identifier resourceLocation) {
    this.addSound(
        type,
        resourceLocation,
        SoundDataEntry.DEFAULT_VOLUME,
        SoundDataEntry.DEFAULT_PITCH,
        SoundDataEntry.DEFAULT_ENABLED);
  }

  public void addSound(
      SoundType type, Identifier resourceLocation, float volume, float pitch, boolean enabled) {
    if (resourceLocation == null || resourceLocation.toString().isEmpty()) {
      return;
    }

    SoundDataEntry soundDataEntry =
        new SoundDataEntry(type, resourceLocation, volume, pitch, enabled);
    SoundDataEntry defaultSoundDataEntry = this.defaultSounds.get(type);
    if (defaultSoundDataEntry != null && isSameSound(defaultSoundDataEntry, soundDataEntry)) {
      this.overrideSounds.remove(type);
    } else {
      this.overrideSounds.put(type, soundDataEntry);
    }
  }

  public void removeSound(SoundType type) {
    this.overrideSounds.remove(type);
  }

  public void addDefaultSound(SoundType type, SoundEvent soundEvent) {
    if (soundEvent == null || soundEvent.location().toString().isEmpty()) {
      return;
    }
    defaultSounds.put(type, new SoundDataEntry(type, soundEvent.location()));
  }

  public boolean isEmpty() {
    return defaultSounds.isEmpty() && overrideSounds.isEmpty();
  }

  public SoundDataEntry getSound(SoundType type) {
    return overrideSounds.containsKey(type) ? overrideSounds.get(type) : defaultSounds.get(type);
  }

  private static List<SoundDataEntry> readSoundEntries(CompoundTag compoundTag, String tagName) {
    List<SoundDataEntry> soundDataEntries = new ArrayList<>();
    ListTag soundListTag = compoundTag.getListOrEmpty(tagName);
    for (int i = 0; i < soundListTag.size(); i++) {
      SoundDataEntry soundDataEntry = new SoundDataEntry(soundListTag.getCompoundOrEmpty(i));
      if (soundDataEntry.getType() != null && soundDataEntry.getSoundEvent() != null) {
        soundDataEntries.add(soundDataEntry);
      }
    }
    return soundDataEntries;
  }

  private static void writeSoundEntries(
      CompoundTag compoundTag, String tagName, Map<SoundType, SoundDataEntry> soundEntries) {
    ListTag soundListTag = new ListTag();
    for (SoundDataEntry soundDataEntry : soundEntries.values()) {
      soundListTag.add(soundDataEntry.createTag());
    }
    CompoundTagUtils.putIfNotEmpty(compoundTag, tagName, soundListTag);
  }

  public void load(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_SOUND_DATA_SET_TAG)) {
      return;
    }

    this.overrideSounds.clear();
    for (SoundDataEntry soundDataEntry : readSoundEntries(compoundTag, DATA_SOUND_DATA_SET_TAG)) {
      SoundDataEntry defaultSoundDataEntry = this.defaultSounds.get(soundDataEntry.getType());
      if (defaultSoundDataEntry == null || !isSameSound(defaultSoundDataEntry, soundDataEntry)) {
        this.overrideSounds.put(soundDataEntry.getType(), soundDataEntry);
      }
    }
  }

  public void loadComplete(CompoundTag compoundTag) {
    this.defaultSounds.clear();
    for (SoundDataEntry soundDataEntry :
        readSoundEntries(compoundTag, DATA_DEFAULT_SOUND_DATA_SET_TAG)) {
      this.defaultSounds.put(soundDataEntry.getType(), soundDataEntry);
    }

    this.load(compoundTag);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    writeSoundEntries(compoundTag, DATA_SOUND_DATA_SET_TAG, this.overrideSounds);

    return compoundTag;
  }

  public CompoundTag saveComplete(CompoundTag compoundTag) {
    writeSoundEntries(compoundTag, DATA_DEFAULT_SOUND_DATA_SET_TAG, this.defaultSounds);

    return this.save(compoundTag);
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  public CompoundTag createCompleteTag() {
    return this.saveComplete(new CompoundTag());
  }
}
