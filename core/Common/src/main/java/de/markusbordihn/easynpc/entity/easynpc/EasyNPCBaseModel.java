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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.ScaleData;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface EasyNPCBaseModel<E extends PathfinderMob>
    extends EasyNPCBase<E>, ModelData<E>, ScaleData<E> {

  static void registerEasyNPCSyncedData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    ModelData.registerSyncedModelData(map, entityClass);
    ScaleData.registerSyncedScaleData(map, entityClass);
  }

  default void defineEasyNPCBaseModelSyncedData() {
    ModelData<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.defineSynchedModelData();
    }
    ScaleData<E> scaleData = getEasyNPCScaleData();
    if (scaleData != null) {
      scaleData.defineSynchedScaleData();
    }
  }

  default void addEasyNPCBaseModelAdditionalSaveData(CompoundTag compoundTag) {
    ModelData<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.addAdditionalModelData(compoundTag);
    }
    ScaleData<E> scaleData = getEasyNPCScaleData();
    if (scaleData != null) {
      scaleData.addAdditionalScaleData(compoundTag);
    }
  }

  default void readEasyNPCBaseModelAdditionalSaveData(CompoundTag compoundTag) {
    ModelData<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.readAdditionalModelData(compoundTag);
    }
    ScaleData<E> scaleData = getEasyNPCScaleData();
    if (scaleData != null) {
      scaleData.readAdditionalScaleData(compoundTag);
    }
  }
}
