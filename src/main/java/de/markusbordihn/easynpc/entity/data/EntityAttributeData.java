/**
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

package de.markusbordihn.easynpc.entity.data;


import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;

import de.markusbordihn.easynpc.entity.EasyNPCEntityData;

public interface EntityAttributeData extends EntityDataInterface {

  // Skin Details
  public enum EntityAttribute {
    FREEFALL
  }

  // Synced entity data
  public static final EntityDataAccessor<Boolean> DATA_ATTRIBUTE_FREEFALL =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // CompoundTags
  public static final String DATA_ATTRIBUTE_TAG = "EntityAttribute";
  public static final String DATA_ATTRIBUTE_FREEFALL_TAG = "Freefall";

  default boolean getAttributeFreefall() {
    return getEntityData(DATA_ATTRIBUTE_FREEFALL);
  }

  default void setAttributeFreefall(boolean freefall) {
    setEntityData(DATA_ATTRIBUTE_FREEFALL, freefall);
  }

  default void defineSynchedAttributeData() {
    defineEntityData(DATA_ATTRIBUTE_FREEFALL, false);
  }

  default void addAdditionalAttributeData(CompoundTag compoundTag) {
    CompoundTag attributeTag = new CompoundTag();

    attributeTag.putBoolean(DATA_ATTRIBUTE_FREEFALL_TAG, getAttributeFreefall());

    compoundTag.put(DATA_ATTRIBUTE_TAG, attributeTag);
  }

  default void readAdditionalAttributeData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_ATTRIBUTE_TAG)) {
      return;
    }
    CompoundTag attributeTag = compoundTag.getCompound(DATA_ATTRIBUTE_TAG);

    setAttributeFreefall(attributeTag.getBoolean(DATA_ATTRIBUTE_FREEFALL_TAG));
  }

}
