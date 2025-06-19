/*
 * Copyright 2025 Markus Bordihn
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

import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.PathfinderMob;

public interface StatusDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  String DATA_STATUS_DATA_TAG = "Status";

  EnumMap<StatusDataType, Boolean> getStatusDataFlags();

  default boolean getStatusDataFlag(StatusDataType key) {
    return getStatusDataFlags().getOrDefault(key, false);
  }

  default void setStatusDataFlag(StatusDataType key, boolean value) {
    getStatusDataFlags().put(key, value);
  }

  default void addAdditionalStatusData(CompoundTag compoundTag) {
    CompoundTag statusTag = new CompoundTag();

    // Set status flags, if not already set.
    if (!getStatusDataFlag(StatusDataType.FINALIZED)) {
      setStatusDataFlag(StatusDataType.FINALIZED, true);
    }

    // Add status flags to the status tag.
    for (Map.Entry<StatusDataType, Boolean> entry : getStatusDataFlags().entrySet()) {
      statusTag.putBoolean(entry.getKey().getTagName(), entry.getValue());
    }

    compoundTag.put(DATA_STATUS_DATA_TAG, statusTag);
  }

  default void readAdditionalStatusData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_STATUS_DATA_TAG)) {
      return;
    }

    // Set status flags from the status tag.
    CompoundTag statusTag = compoundTag.getCompound(DATA_STATUS_DATA_TAG);
    for (String key : statusTag.getAllKeys()) {
      StatusDataType statusDataType = StatusDataType.get(key);
      if (statusDataType == null) {
        continue;
      }
      setStatusDataFlag(StatusDataType.get(key), statusTag.getBoolean(key));
    }
  }
}
