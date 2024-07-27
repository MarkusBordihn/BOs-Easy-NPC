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

import de.markusbordihn.easynpc.data.server.ServerDataAccessor;
import de.markusbordihn.easynpc.data.server.ServerDataIndex;
import de.markusbordihn.easynpc.data.server.ServerEntityData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.PathfinderMob;

public interface SpawnerData<T extends PathfinderMob> extends EasyNPC<T> {

  ServerDataAccessor<UUID> CUSTOM_DATA_SPAWNER_UUID =
      ServerEntityData.defineId(
          ServerDataIndex.SPAWNER_UUID, EntityDataSerializersManager.SPAWNER_UUID);
  String DATA_SPAWNER_UUID_TAG = "SpawnerUUID";

  default boolean hasSpawnerUUID() {
    return this.getSpawnerUUID() != null;
  }

  default UUID getSpawnerUUID() {
    return this.getServerEntityData(CUSTOM_DATA_SPAWNER_UUID);
  }

  default void setSpawnerUUID(UUID uuid) {
    this.setServerEntityData(CUSTOM_DATA_SPAWNER_UUID, uuid);
  }

  default void defineCustomSpawnerData() {
    defineServerEntityData(CUSTOM_DATA_SPAWNER_UUID, null);
  }

  default void addAdditionalSpawnerData(CompoundTag compoundTag) {
    if (this.isServerSide() && this.getSpawnerUUID() != null) {
      compoundTag.putUUID(DATA_SPAWNER_UUID_TAG, this.getSpawnerUUID());
    }
  }

  default void readAdditionalSpawnerData(CompoundTag compoundTag) {
    if (compoundTag.hasUUID(DATA_SPAWNER_UUID_TAG)) {
      this.setSpawnerUUID(compoundTag.getUUID(DATA_SPAWNER_UUID_TAG));
    }
  }
}
