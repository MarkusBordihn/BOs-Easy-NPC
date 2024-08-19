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

import de.markusbordihn.easynpc.data.display.DisplayAttributeSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface DisplayAttributeData<E extends PathfinderMob> extends EasyNPC<E> {

  static void registerSyncedDisplayAttributeData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Display Attribute Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.DISPLAY_ATTRIBUTE_SET,
        SynchedEntityData.defineId(
            entityClass, EntityDataSerializersManager.DISPLAY_ATTRIBUTE_SET));
  }

  default DisplayAttributeSet getDisplayAttributeSet() {
    return getSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET);
  }

  default void setDisplayAttributeSet(DisplayAttributeSet displayAttributeSet) {
    setSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, displayAttributeSet);
  }

  default void clearDisplayAttributeSet() {
    setSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, new DisplayAttributeSet());
  }

  default void updateDisplayAttributeSet() {
    DisplayAttributeSet displayAttributeSet = getDisplayAttributeSet();
    if (displayAttributeSet != null) {
      this.clearDisplayAttributeSet();
      this.setDisplayAttributeSet(displayAttributeSet);
    }
  }

  default boolean hasDisplayAttribute(DisplayAttributeType displayAttributeType) {
    DisplayAttributeSet displayAttributeSet = getDisplayAttributeSet();
    if (displayAttributeSet != null) {
      return displayAttributeSet.hasDisplayAttribute(displayAttributeType);
    }
    return false;
  }

  default boolean getDisplayBooleanAttribute(DisplayAttributeType displayAttributeType) {
    DisplayAttributeSet displayAttributeSet = getDisplayAttributeSet();
    if (displayAttributeSet != null) {
      return displayAttributeSet.getBooleanValue(displayAttributeType);
    }
    return false;
  }

  default int getDisplayIntAttribute(DisplayAttributeType displayAttributeType) {
    DisplayAttributeSet displayAttributeSet = getDisplayAttributeSet();
    if (displayAttributeSet != null) {
      return displayAttributeSet.getIntValue(displayAttributeType);
    }
    return 0;
  }

  default void defineSynchedDisplayAttributeData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, new DisplayAttributeSet());
  }

  default void readAdditionalDisplayAttributeData(CompoundTag compoundTag) {
    if (compoundTag.contains(DisplayAttributeSet.DATA_DISPLAY_ATTRIBUTE_SET_TAG)) {
      DisplayAttributeSet displayAttributeSet = new DisplayAttributeSet(compoundTag);
      this.setDisplayAttributeSet(displayAttributeSet);
    }
  }

  default void addAdditionalDisplayAttributeData(CompoundTag compoundTag) {
    DisplayAttributeSet displayAttributeSet = getDisplayAttributeSet();
    if (displayAttributeSet != null) {
      displayAttributeSet.save(compoundTag);
    }
  }
}
