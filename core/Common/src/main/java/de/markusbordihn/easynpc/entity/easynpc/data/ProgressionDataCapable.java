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
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;

public interface ProgressionDataCapable<E extends Mob> extends EasyNPC<E> {

  default ProgressionData getProgressionData() {
    return getSynchedEntityData(SynchedDataIndex.PROGRESSION);
  }

  default void setProgressionData(ProgressionData progressionData) {
    setSynchedEntityData(SynchedDataIndex.PROGRESSION, progressionData);
  }

  default int getExperience() {
    ProgressionData data = getProgressionData();
    return data != null ? data.experience() : 1;
  }

  default void setExperience(int experience) {
    ProgressionData oldData = getProgressionData();
    if (oldData == null) {
      return;
    }
    int clampedXP =
        Math.max(
            1,
            Math.min(
                experience,
                ProgressionLevelMap.getExperienceForLevel(ProgressionLevelMap.MAX_LEVEL)));
    int newLevel = ProgressionLevelMap.getLevelForExperience(clampedXP);
    ProgressionData newData =
        new ProgressionData(clampedXP, newLevel, oldData.attributeScalingEnabled());
    setProgressionData(newData);
    if (oldData.experienceLevel() != newLevel) {
      onProgressLevelChange(oldData, newData);
    }
  }

  default int getExperienceLevel() {
    ProgressionData data = getProgressionData();
    return data != null ? data.experienceLevel() : 1;
  }

  default void setExperienceLevel(int level) {
    ProgressionData oldData = getProgressionData();
    if (oldData == null) return;
    int clampedLevel =
        Math.max(ProgressionLevelMap.MIN_LEVEL, Math.min(level, ProgressionLevelMap.MAX_LEVEL));
    int newXP = ProgressionLevelMap.getExperienceForLevel(clampedLevel);
    ProgressionData newData =
        new ProgressionData(newXP, clampedLevel, oldData.attributeScalingEnabled());
    setProgressionData(newData);
    if (oldData.experienceLevel() != clampedLevel) {
      onProgressLevelChange(oldData, newData);
    }
  }

  default boolean isAttributeScalingEnabled() {
    ProgressionData data = getProgressionData();
    return data != null && data.attributeScalingEnabled();
  }

  default void setAttributeScalingEnabled(boolean enabled) {
    ProgressionData oldData = getProgressionData();
    if (oldData == null) return;
    ProgressionData newData =
        new ProgressionData(oldData.experience(), oldData.experienceLevel(), enabled);
    setProgressionData(newData);
    ProgressionAttributeHandler.applyLevelScaling(this);
  }

  default void addExperience(int amount) {
    setExperience(getExperience() + amount);
  }

  default void increaseExperience(int experience) {
    addExperience(experience);
  }

  default void decreaseExperience(int experience) {
    addExperience(-experience);
  }

  default void increaseExperienceLevel(int levels) {
    setExperienceLevel(getExperienceLevel() + levels);
  }

  default void decreaseExperienceLevel(int levels) {
    setExperienceLevel(getExperienceLevel() - levels);
  }

  default void decreaseExperienceAndExperienceLevel() {
    int currentLevel = getExperienceLevel();
    decreaseExperience(ProgressionLevelMap.getExperienceDifferenceForLevel(currentLevel));
  }

  default boolean isMaxExperienceLevel() {
    return getExperienceLevel() >= getMaxExperienceLevel();
  }

  default boolean isMinExperienceLevel() {
    return getExperienceLevel() == getMinExperienceLevel();
  }

  default int getMaxExperienceLevel() {
    return ProgressionLevelMap.MAX_LEVEL;
  }

  default int getMinExperienceLevel() {
    return ProgressionLevelMap.MIN_LEVEL;
  }

  default int getExperienceForNextLevel() {
    return ProgressionLevelMap.getExperienceForNextLevel(getExperienceLevel());
  }

  default int getExperienceForLevel() {
    return ProgressionLevelMap.getExperienceForLevel(getExperienceLevel());
  }

  default int getExperienceProgressToNextLevel() {
    ProgressionData data = getProgressionData();
    return data != null
        ? ProgressionLevelMap.getExperienceProgressToNextLevel(
            data.experience(), data.experienceLevel())
        : 0;
  }

  default float getProgressPercentageToNextLevel() {
    ProgressionData data = getProgressionData();
    return data != null
        ? ProgressionLevelMap.getProgressPercentageToNextLevel(
            data.experience(), data.experienceLevel())
        : 0.0f;
  }

  default int getAttributeAdjustment(int baseValue, int maxValue) {
    int level = getExperienceLevel();
    if (level == 1 || maxValue == 0 || baseValue >= maxValue) return 0;
    double factor = (double) (maxValue - baseValue) / getMaxExperienceLevel();
    return (int) Math.floor(level * factor + 0.5);
  }

  default void onProgressLevelChange(ProgressionData oldData, ProgressionData newData) {
    int oldLevel = oldData.experienceLevel();
    int newLevel = newData.experienceLevel();
    if (newLevel > oldLevel) {
      onProgressLevelUp(oldData, newData);
    } else if (newLevel < oldLevel) {
      onProgressLevelDown(oldData, newData);
    }
    ProgressionAttributeHandler.applyLevelScaling(this);
  }

  default void onProgressLevelUp(ProgressionData oldData, ProgressionData newData) {
    if (getEntity().level() instanceof ServerLevel serverLevel) {
      serverLevel.sendParticles(
          ParticleTypes.ENCHANT,
          getEntity().getX(),
          getEntity().getY() + getEntity().getBbHeight() / 2.0,
          getEntity().getZ(),
          50,
          0.5,
          0.5,
          0.5,
          0.5);
    }
    log.debug(
        "{} leveled up from {} to {}!",
        getEntity(),
        oldData.experienceLevel(),
        newData.experienceLevel());
  }

  default void onProgressLevelDown(ProgressionData oldData, ProgressionData newData) {
    if (getEntity().level() instanceof ServerLevel serverLevel) {
      serverLevel.sendParticles(
          ParticleTypes.SMOKE,
          getEntity().getX(),
          getEntity().getY() + getEntity().getBbHeight() / 2.0,
          getEntity().getZ(),
          50,
          0.5,
          0.5,
          0.5,
          0.5);
    }
    log.debug(
        "{} leveled down from {} to {}!",
        getEntity(),
        oldData.experienceLevel(),
        newData.experienceLevel());
  }

  default void defineSynchedProgressionData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.PROGRESSION, new ProgressionData());
  }

  default void addAdditionalProgressionData(CompoundTag compoundTag) {
    ProgressionData progressionData = getProgressionData();
    if (progressionData != null) {
      progressionData.encode(compoundTag);
    }
  }

  default void readAdditionalProgressionData(CompoundTag compoundTag) {
    this.setProgressionData(ProgressionData.decode(compoundTag));
  }
}
