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

import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Mob;

public interface ModelRootDataCapable<T extends Mob> extends EasyNPC<T> {

  String EASY_NPC_DATA_MODEL_ROOT_TAG = "Root";

  default RootModelData getModelRootData() {
    RootModelData data = getSynchedEntityData(SynchedDataIndex.MODEL_ROOT_DATA);

    return data != null ? data : RootModelData.DEFAULT;
  }

  default void setModelRootData(RootModelData data) {
    if (data != null) {
      setSynchedEntityData(SynchedDataIndex.MODEL_ROOT_DATA, data, true);
    }
  }

  default void setModelRootRotation(CustomRotation rotation) {
    RootModelData current = getModelRootData();
    setModelRootData(new RootModelData(rotation, current.scale()));
  }

  default void setModelRootScale(CustomScale scale) {
    RootModelData current = getModelRootData();
    setModelRootData(new RootModelData(current.rotation(), scale));
    this.getEntity().refreshDimensions();
  }

  default boolean hasChangedModelRoot() {
    return getModelRootData().hasChanged();
  }

  default void defineSynchedModelRootData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.MODEL_ROOT_DATA, RootModelData.DEFAULT);
  }

  default void addAdditionalModelRootData(CompoundTag modelDataTag) {
    if (hasChangedModelRoot()) {
      modelDataTag.put(EASY_NPC_DATA_MODEL_ROOT_TAG, getModelRootData().save());
    }
  }

  default void readAdditionalModelRootData(CompoundTag modelDataTag) {
    if (modelDataTag.contains(EASY_NPC_DATA_MODEL_ROOT_TAG)) {
      setModelRootData(RootModelData.load(modelDataTag.getCompound(EASY_NPC_DATA_MODEL_ROOT_TAG)));
    }
  }
}
