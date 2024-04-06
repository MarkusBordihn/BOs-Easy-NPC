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

import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Objects;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;

public interface ScaleData<T extends PathfinderMob> extends EasyNPC<T> {

  // Constants values
  float DEFAULT_SCALE_X = 1.0f;
  float DEFAULT_SCALE_Y = 1.0f;
  float DEFAULT_SCALE_Z = 1.0f;

  // Synced entity data
  EntityDataAccessor<Float> EASY_NPC_DATA_SCALE_X =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.FLOAT);
  EntityDataAccessor<Float> EASY_NPC_DATA_SCALE_Y =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.FLOAT);
  EntityDataAccessor<Float> EASY_NPC_DATA_SCALE_Z =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.FLOAT);

  // CompoundTags
  String EASY_NPC_DATA_SCALE_DATA_TAG = "ScaleData";

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
    return getEasyNPCData(EASY_NPC_DATA_SCALE_X);
  }

  default void setScaleX(Float scale) {
    if (!Objects.equals(getScaleX(), scale)) {
      setEasyNPCData(EASY_NPC_DATA_SCALE_X, scale);
      getEasyNPCEntity().refreshDimensions();
    }
  }

  default Float getScaleY() {
    return getEasyNPCData(EASY_NPC_DATA_SCALE_Y);
  }

  default void setScaleY(Float scale) {
    if (!Objects.equals(getScaleY(), scale)) {
      setEasyNPCData(EASY_NPC_DATA_SCALE_Y, scale);
      getEasyNPCEntity().refreshDimensions();
    }
  }

  default Float getScaleZ() {
    return getEasyNPCData(EASY_NPC_DATA_SCALE_Z);
  }

  default void setScaleZ(Float scale) {
    if (!Objects.equals(getScaleZ(), scale)) {
      setEasyNPCData(EASY_NPC_DATA_SCALE_Z, scale);
      getEasyNPCEntity().refreshDimensions();
    }
  }

  default void defineSynchedScaleData() {
    defineEasyNPCData(EASY_NPC_DATA_SCALE_X, this.getDefaultScaleX());
    defineEasyNPCData(EASY_NPC_DATA_SCALE_Y, this.getDefaultScaleY());
    defineEasyNPCData(EASY_NPC_DATA_SCALE_Z, this.getDefaultScaleZ());
  }

  default void addAdditionalScaleData(CompoundTag compoundTag) {
    if (Objects.equals(this.getScaleX(), this.getDefaultScaleX())
        && Objects.equals(this.getScaleY(), this.getDefaultScaleY())
        && Objects.equals(this.getScaleZ(), this.getDefaultScaleZ())) {
      return;
    }
    compoundTag.put(
        EASY_NPC_DATA_SCALE_DATA_TAG,
        CompoundTagUtils.writeScale(this.getScaleX(), this.getScaleY(), this.getScaleZ()));
  }

  default void readAdditionalScaleData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_SCALE_DATA_TAG)) {
      return;
    }
    CustomScale customScale =
        CompoundTagUtils.readCustomScale(compoundTag.getCompound(EASY_NPC_DATA_SCALE_DATA_TAG));
    if (customScale.x() > 0.0f) {
      this.setScaleX(customScale.x());
    }
    if (customScale.y() > 0.0f) {
      this.setScaleY(customScale.y());
    }
    if (customScale.z() > 0.0f) {
      this.setScaleZ(customScale.z());
    }
  }
}
