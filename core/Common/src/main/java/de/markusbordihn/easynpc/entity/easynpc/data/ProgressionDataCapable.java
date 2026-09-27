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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.progression.ProgressionData;
import de.markusbordihn.easynpc.data.progression.ProgressionLevelMap;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.ProgressionAttributeHandler;
import java.util.Optional;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface ProgressionDataCapable<E extends Mob> extends EasyNPC<E> {

  default ProgressionData getProgressionData() {
    return this.getSynchedEntityData(SynchedDataIndex.PROGRESSION);
  }

  default void setProgressionData(ProgressionData progressionData) {
    this.setSynchedEntityData(SynchedDataIndex.PROGRESSION, progressionData);
  }

  default int getExperience() {
    ProgressionData data = this.getProgressionData();
    return data != null ? data.experience() : 1;
  }

  default void setExperience(int experience) {
    ProgressionData oldData = this.getProgressionData();
    if (oldData == null) {
      return;
    }

    int clampedExperience =
        Math.max(
            1,
            Math.min(
                experience,
                ProgressionLevelMap.getExperienceForLevel(ProgressionLevelMap.MAX_LEVEL)));
    int newLevel = ProgressionLevelMap.getLevelForExperience(clampedExperience);
    ProgressionData newData =
        new ProgressionData(clampedExperience, newLevel, oldData.attributeScalingEnabled());
    this.setProgressionData(newData);
    if (oldData.experienceLevel() != newLevel) {
      this.onProgressLevelChange(oldData, newData);
    }
  }

  default int getExperienceLevel() {
    ProgressionData data = this.getProgressionData();
    return data != null ? data.experienceLevel() : 1;
  }

  default void setExperienceLevel(int level) {
    ProgressionData oldData = this.getProgressionData();
    if (oldData == null) {
      return;
    }

    int clampedLevel =
        Math.max(ProgressionLevelMap.MIN_LEVEL, Math.min(level, ProgressionLevelMap.MAX_LEVEL));
    int newExperience = ProgressionLevelMap.getExperienceForLevel(clampedLevel);
    ProgressionData newData =
        new ProgressionData(newExperience, clampedLevel, oldData.attributeScalingEnabled());
    this.setProgressionData(newData);
    if (oldData.experienceLevel() != clampedLevel) {
      this.onProgressLevelChange(oldData, newData);
    }
  }

  default boolean isAttributeScalingEnabled() {
    ProgressionData data = this.getProgressionData();
    return data != null && data.attributeScalingEnabled();
  }

  default void setAttributeScalingEnabled(boolean enabled) {
    ProgressionData oldData = this.getProgressionData();
    if (oldData == null) {
      return;
    }

    ProgressionData newData =
        new ProgressionData(oldData.experience(), oldData.experienceLevel(), enabled);
    this.setProgressionData(newData);
    ProgressionAttributeHandler.applyLevelScaling(this);
  }

  default void addExperience(int amount) {
    this.setExperience(this.getExperience() + amount);
  }

  default void increaseExperience(int experience) {
    this.addExperience(experience);
  }

  default void decreaseExperience(int experience) {
    this.addExperience(-experience);
  }

  default void increaseExperienceLevel(int levels) {
    this.setExperienceLevel(this.getExperienceLevel() + levels);
  }

  default void decreaseExperienceLevel(int levels) {
    this.setExperienceLevel(this.getExperienceLevel() - levels);
  }

  default void decreaseExperienceAndExperienceLevel() {
    int currentLevel = this.getExperienceLevel();
    this.decreaseExperience(ProgressionLevelMap.getExperienceDifferenceForLevel(currentLevel));
  }

  default boolean isMaxExperienceLevel() {
    return this.getExperienceLevel() >= this.getMaxExperienceLevel();
  }

  default boolean isMinExperienceLevel() {
    return this.getExperienceLevel() == this.getMinExperienceLevel();
  }

  default int getMaxExperienceLevel() {
    return ProgressionLevelMap.MAX_LEVEL;
  }

  default int getMinExperienceLevel() {
    return ProgressionLevelMap.MIN_LEVEL;
  }

  default int getExperienceForNextLevel() {
    return ProgressionLevelMap.getExperienceForNextLevel(this.getExperienceLevel());
  }

  default int getExperienceForLevel() {
    return ProgressionLevelMap.getExperienceForLevel(this.getExperienceLevel());
  }

  default int getExperienceProgressToNextLevel() {
    ProgressionData data = this.getProgressionData();
    return data != null
        ? ProgressionLevelMap.getExperienceProgressToNextLevel(
            data.experience(), data.experienceLevel())
        : 0;
  }

  default float getProgressPercentageToNextLevel() {
    ProgressionData data = this.getProgressionData();
    return data != null
        ? ProgressionLevelMap.getProgressPercentageToNextLevel(
            data.experience(), data.experienceLevel())
        : 0.0f;
  }

  default int getAttributeAdjustment(int baseValue, int maxValue) {
    int level = this.getExperienceLevel();
    if (level == 1 || maxValue == 0 || baseValue >= maxValue) {
      return 0;
    }

    double factor = (double) (maxValue - baseValue) / this.getMaxExperienceLevel();
    return (int) Math.floor(level * factor + 0.5);
  }

  default void onProgressLevelChange(ProgressionData oldData, ProgressionData newData) {
    int oldLevel = oldData.experienceLevel();
    int newLevel = newData.experienceLevel();
    if (newLevel > oldLevel) {
      this.onProgressLevelUp(oldData, newData);
    } else if (newLevel < oldLevel) {
      this.onProgressLevelDown(oldData, newData);
    }
    ProgressionAttributeHandler.applyLevelScaling(this);
  }

  default void onProgressLevelUp(ProgressionData oldData, ProgressionData newData) {
    if (this.getEntity().level() instanceof ServerLevel serverLevel) {
      serverLevel.sendParticles(
          ParticleTypes.ENCHANT,
          this.getEntity().getX(),
          this.getEntity().getY() + this.getEntity().getBbHeight() / 2.0,
          this.getEntity().getZ(),
          50,
          0.5,
          0.5,
          0.5,
          0.5);
    }
    log.debug(
        "{} leveled up from {} to {}!",
        this.getEntity(),
        oldData.experienceLevel(),
        newData.experienceLevel());
  }

  default void onProgressLevelDown(ProgressionData oldData, ProgressionData newData) {
    if (this.getEntity().level() instanceof ServerLevel serverLevel) {
      serverLevel.sendParticles(
          ParticleTypes.SMOKE,
          this.getEntity().getX(),
          this.getEntity().getY() + this.getEntity().getBbHeight() / 2.0,
          this.getEntity().getZ(),
          50,
          0.5,
          0.5,
          0.5,
          0.5);
    }
    log.debug(
        "{} leveled down from {} to {}!",
        this.getEntity(),
        oldData.experienceLevel(),
        newData.experienceLevel());
  }

  default void defineSynchedProgressionData(SynchedEntityData.Builder builder) {
    this.defineSynchedEntityData(builder, SynchedDataIndex.PROGRESSION, new ProgressionData());
  }

  default void addAdditionalProgressionData(ValueOutput valueOutput) {
    ProgressionData progressionData = this.getProgressionData();
    if (progressionData == null) {
      return;
    }

    CompoundTag progressionTag =
        progressionData
            .encode(new CompoundTag())
            .getCompoundOrEmpty(ProgressionData.DATA_PROGRESSION_TAG);
    if (!progressionTag.isEmpty()) {
      valueOutput.store(ProgressionData.DATA_PROGRESSION_TAG, CompoundTag.CODEC, progressionTag);
    }
  }

  default void readAdditionalProgressionData(ValueInput valueInput) {
    Optional<CompoundTag> compoundTagData =
        valueInput.read(ProgressionData.DATA_PROGRESSION_TAG, CompoundTag.CODEC);
    if (compoundTagData.isEmpty()) {
      return;
    }
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(ProgressionData.DATA_PROGRESSION_TAG, compoundTagData.get());
    this.setProgressionData(ProgressionData.decode(compoundTag));
  }
}
