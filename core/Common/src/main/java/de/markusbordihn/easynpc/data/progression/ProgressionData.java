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

package de.markusbordihn.easynpc.data.progression;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;

public record ProgressionData(
    int experience, int experienceLevel, boolean attributeScalingEnabled) {

  public static final String DATA_PROGRESSION_TAG = "Progression";
  public static final String ENTITY_EXPERIENCE_TAG = "EntityExperience";
  public static final String ENTITY_EXPERIENCE_LEVEL_TAG = "EntityExperienceLevel";
  public static final String ATTRIBUTE_SCALING_ENABLED_TAG = "AttributeScalingEnabled";

  public static final StreamCodec<RegistryFriendlyByteBuf, ProgressionData> STREAM_CODEC =
      StreamCodec.composite(
          ByteBufCodecs.INT,
          ProgressionData::experience,
          ByteBufCodecs.INT,
          ProgressionData::experienceLevel,
          ByteBufCodecs.BOOL,
          ProgressionData::attributeScalingEnabled,
          ProgressionData::new);

  public ProgressionData() {
    this(1, 1, false);
  }

  public static ProgressionData decode(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_PROGRESSION_TAG)) {
      return new ProgressionData();
    }
    CompoundTag progressionTag = compoundTag.getCompound(DATA_PROGRESSION_TAG);
    return new ProgressionData(
        progressionTag.getInt(ENTITY_EXPERIENCE_TAG),
        progressionTag.getInt(ENTITY_EXPERIENCE_LEVEL_TAG),
        progressionTag.getBoolean(ATTRIBUTE_SCALING_ENABLED_TAG));
  }

  public CompoundTag encode(CompoundTag compoundTag) {
    CompoundTag progressionTag = new CompoundTag();
    progressionTag.putInt(ENTITY_EXPERIENCE_TAG, experience());
    progressionTag.putInt(ENTITY_EXPERIENCE_LEVEL_TAG, experienceLevel());
    progressionTag.putBoolean(ATTRIBUTE_SCALING_ENABLED_TAG, attributeScalingEnabled());
    compoundTag.put(DATA_PROGRESSION_TAG, progressionTag);
    return compoundTag;
  }

  public ProgressionData withExperience(int experience) {
    return new ProgressionData(experience, experienceLevel, attributeScalingEnabled);
  }

  public ProgressionData withExperienceLevel(int experienceLevel) {
    return new ProgressionData(experience, experienceLevel, attributeScalingEnabled);
  }

  public ProgressionData withAttributeScalingEnabled(boolean attributeScalingEnabled) {
    return new ProgressionData(experience, experienceLevel, attributeScalingEnabled);
  }
}
