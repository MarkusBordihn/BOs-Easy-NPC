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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Objects;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.LivingEntity;

public interface ScaleData<T extends LivingEntity> extends EasyNPC<T> {

  // Constants values
  float DEFAULT_SCALE_X = 1.0f;
  float DEFAULT_SCALE_Y = 1.0f;
  float DEFAULT_SCALE_Z = 1.0f;

  // Synced entity data
  EntityDataAccessor<Float> DATA_SCALE_X =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.FLOAT);
  EntityDataAccessor<Float> DATA_SCALE_Y =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.FLOAT);
  EntityDataAccessor<Float> DATA_SCALE_Z =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.FLOAT);

  // CompoundTags
  String DATA_SCALE_X_TAG = "ScaleX";
  String DATA_SCALE_Y_TAG = "ScaleY";
  String DATA_SCALE_Z_TAG = "ScaleZ";

  default Float getDefaultScaleX() {
    return DEFAULT_SCALE_X;
  }

  default Float getDefaultScaleY() {
    return DEFAULT_SCALE_Y;
  }

  default Float getDefaultScaleZ() {
    return DEFAULT_SCALE_Z;
  }

  default Float getScaleX() {
    return getEasyNPCData(DATA_SCALE_X);
  }

  default void setScaleX(Float scale) {
    if (!Objects.equals(getScaleX(), scale)) {
      setEasyNPCData(DATA_SCALE_X, scale);
      getEasyNPCEntity().refreshDimensions();
    }
  }

  default Float getScaleY() {
    return getEasyNPCData(DATA_SCALE_Y);
  }

  default void setScaleY(Float scale) {
    if (!Objects.equals(getScaleY(), scale)) {
      setEasyNPCData(DATA_SCALE_Y, scale);
      getEasyNPCEntity().refreshDimensions();
    }
  }

  default Float getScaleZ() {
    return getEasyNPCData(DATA_SCALE_Z);
  }

  default void setScaleZ(Float scale) {
    if (!Objects.equals(getScaleZ(), scale)) {
      setEasyNPCData(DATA_SCALE_Z, scale);
      getEasyNPCEntity().refreshDimensions();
    }
  }

  default void defineSynchedScaleData() {
    defineEasyNPCData(DATA_SCALE_X, this.getDefaultScaleX());
    defineEasyNPCData(DATA_SCALE_Y, this.getDefaultScaleY());
    defineEasyNPCData(DATA_SCALE_Z, this.getDefaultScaleZ());
  }

  default void addAdditionalScaleData(CompoundTag compoundTag) {
    if (this.getScaleX() != null && this.getScaleX() > 0.0f) {
      compoundTag.putFloat(DATA_SCALE_X_TAG, this.getScaleX());
    }
    if (this.getScaleY() != null && this.getScaleY() > 0.0f) {
      compoundTag.putFloat(DATA_SCALE_Y_TAG, this.getScaleY());
    }
    if (this.getScaleZ() != null && this.getScaleZ() > 0.0f) {
      compoundTag.putFloat(DATA_SCALE_Z_TAG, this.getScaleZ());
    }
  }

  default void readAdditionalScaleData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_SCALE_X_TAG)) {
      float scale = compoundTag.getFloat(DATA_SCALE_X_TAG);
      if (scale > 0.0f) {
        this.setScaleX(scale);
      }
    }
    if (compoundTag.contains(DATA_SCALE_Y_TAG)) {
      float scale = compoundTag.getFloat(DATA_SCALE_Y_TAG);
      if (scale > 0.0f) {
        this.setScaleY(scale);
      }
    }
    if (compoundTag.contains(DATA_SCALE_Z_TAG)) {
      float scale = compoundTag.getFloat(DATA_SCALE_Z_TAG);
      if (scale > 0.0f) {
        this.setScaleZ(scale);
      }
    }
  }
}
