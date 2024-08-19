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

import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;

public class SoundDataSet {

  public static final String DATA_SOUND_DATA_SET_TAG = "SoundDataSet";

  private final Map<SoundType, SoundDataEntry> soundDataEntryMap = new EnumMap<>(SoundType.class);

  public SoundDataSet() {}

  public SoundDataSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public boolean hasSound(SoundType type) {
    return soundDataEntryMap.containsKey(type);
  }

  public void addSound(SoundType type, SoundEvent soundEvent) {
    this.addSound(type, soundEvent.getLocation());
  }

  public void addSound(SoundType type, ResourceLocation resourceLocation) {
    soundDataEntryMap.put(type, new SoundDataEntry(type, resourceLocation));
  }

  public boolean isEmpty() {
    return soundDataEntryMap.isEmpty();
  }

  public SoundDataEntry getSound(SoundType type) {
    return soundDataEntryMap.get(type);
  }

  public void load(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_SOUND_DATA_SET_TAG)) {
      return;
    }

    // Load sound data entries
    soundDataEntryMap.clear();
    ListTag soundListTag = compoundTag.getList(DATA_SOUND_DATA_SET_TAG, 10);
    for (int i = 0; i < soundListTag.size(); i++) {
      CompoundTag soundDataTag = soundListTag.getCompound(i);
      SoundDataEntry soundDataEntry = new SoundDataEntry(soundDataTag);
      soundDataEntryMap.put(soundDataEntry.getType(), soundDataEntry);
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag soundListTag = new ListTag();
    for (Map.Entry<SoundType, SoundDataEntry> entry : soundDataEntryMap.entrySet()) {
      SoundDataEntry soundDataEntry = entry.getValue();
      soundListTag.add(soundDataEntry.createTag());
    }
    compoundTag.put(DATA_SOUND_DATA_SET_TAG, soundListTag);

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }
}
