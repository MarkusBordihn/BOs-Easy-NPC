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

public interface EntityAttackData extends EntityDataInterface {

  // Synced entity data
  public static final EntityDataAccessor<Boolean> DATA_AGGRESSIVE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // CompoundTags
  public static final String DATA_AGGRESSIVE_TAG = "Aggressive";

  default boolean getDefaultAggression() {
    return false;
  }

  default boolean isAggressive() {
    return getEntityData(DATA_AGGRESSIVE);
  }

  default void setAggressive(Boolean aggressive) {
    setEntityData(DATA_AGGRESSIVE, aggressive);
  }

  default void defineSynchedAttackData() {
    defineEntityData(DATA_AGGRESSIVE, getDefaultAggression());
  }

  default void addAdditionalAttackData(CompoundTag compoundTag) {
    compoundTag.putBoolean(DATA_AGGRESSIVE_TAG, this.isAggressive());
  }

  default void readAdditionalAttackData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_AGGRESSIVE_TAG)) {
      this.setAggressive(compoundTag.getBoolean(DATA_AGGRESSIVE_TAG));
    }
  }
}
