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
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Mob;

public interface StatusDataCapable<T extends Mob> extends EasyNPC<T> {

  String DATA_STATUS_DATA_TAG = "Status";

  private static long currentTimeAfter(long earlierTimestamp) {
    return Math.max(System.currentTimeMillis(), earlierTimestamp + 1);
  }

  EnumMap<StatusDataType, Boolean> getStatusDataFlags();

  EnumMap<StatusDataType, Long> getStatusDataTimestamps();

  default boolean getStatusDataFlag(StatusDataType key) {
    return this.getStatusDataFlags().getOrDefault(key, false);
  }

  default void setStatusDataFlag(StatusDataType key, boolean value) {
    this.getStatusDataFlags().put(key, value);
  }

  default long getStatusDataTimestamp(StatusDataType key) {
    return this.getStatusDataTimestamps().getOrDefault(key, 0L);
  }

  default void setStatusDataTimestamp(StatusDataType key, long timestamp) {
    this.getStatusDataTimestamps().put(key, timestamp);
  }

  default boolean hasUnsavedNPCData() {
    return this.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE)
        > getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED);
  }

  default void markNPCDataUpdated() {
    this.setStatusDataTimestamp(
        StatusDataType.NPC_DATA_LAST_UPDATE,
        currentTimeAfter(this.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED)));
  }

  default void markNPCDataSaved() {
    long lastUpdate = this.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_UPDATE);
    if (lastUpdate <= this.getStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED)) {
      return;
    }

    this.setStatusDataTimestamp(StatusDataType.NPC_DATA_LAST_SAVED, lastUpdate);
  }

  default void addAdditionalStatusData(CompoundTag compoundTag) {
    CompoundTag statusTag = new CompoundTag();

    if (!this.getStatusDataFlag(StatusDataType.FINALIZED)) {
      this.setStatusDataFlag(StatusDataType.FINALIZED, true);
    }

    for (StatusDataType statusDataType : StatusDataType.values()) {
      if (statusDataType.isBoolean()) {
        Boolean value = this.getStatusDataFlags().get(statusDataType);
        if (value != null) {
          statusTag.putBoolean(statusDataType.getTagName(), value);
        }
      } else if (statusDataType.isTimestamp()) {
        Long value = this.getStatusDataTimestamps().get(statusDataType);
        if (value != null && value > 0) {
          statusTag.putLong(statusDataType.getTagName(), value);
        }
      }
    }

    compoundTag.put(DATA_STATUS_DATA_TAG, statusTag);
  }

  default void readAdditionalStatusData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_STATUS_DATA_TAG)) {
      return;
    }

    CompoundTag statusTag = compoundTag.getCompound(DATA_STATUS_DATA_TAG);
    for (String key : statusTag.getAllKeys()) {
      StatusDataType statusDataType = StatusDataType.get(key);
      if (statusDataType == null) {
        continue;
      }

      if (statusDataType.isBoolean()) {
        this.setStatusDataFlag(statusDataType, statusTag.getBoolean(key));
      } else if (statusDataType.isTimestamp()) {
        this.setStatusDataTimestamp(statusDataType, statusTag.getLong(key));
      }
    }
  }
}
