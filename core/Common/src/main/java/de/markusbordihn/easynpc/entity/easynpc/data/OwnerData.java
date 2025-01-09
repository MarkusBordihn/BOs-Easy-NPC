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

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.OwnableEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;

public interface OwnerData<T extends PathfinderMob> extends EasyNPC<T>, OwnableEntity {

  String DATA_OWNER_TAG = "Owner";

  static void registerSyncedOwnerData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Owner Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.OWNER_UUID,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.OPTIONAL_UUID));
  }

  @Override
  default UUID getOwnerUUID() {
    Optional<UUID> ownerUUID = getSynchedEntityData(SynchedDataIndex.OWNER_UUID);
    return ownerUUID.orElse(null);
  }

  default void setOwnerUUID(UUID uuid) {
    setSynchedEntityData(SynchedDataIndex.OWNER_UUID, Optional.ofNullable(uuid));
  }

  default boolean hasOwner() {
    return this.getOwnerUUID() != null;
  }

  default boolean isOwnedBy(LivingEntity livingEntity) {
    return livingEntity != null
        && this.hasOwner()
        && livingEntity.getUUID().equals(this.getOwnerUUID());
  }

  @Override
  default LivingEntity getOwner() {
    Level level = getLevel();
    if (level == null) {
      return null;
    }
    try {
      UUID uuid = this.getOwnerUUID();
      return uuid == null ? null : level.getPlayerByUUID(uuid);
    } catch (IllegalArgumentException illegalArgumentException) {
      return null;
    }
  }

  default void setOwner(LivingEntity owner) {
    if (owner != null) {
      this.setOwnerUUID(owner.getUUID());
    } else {
      this.setOwnerUUID(null);
    }
  }

  default String getOwnerName() {
    LivingEntity owner = this.getOwner();
    return owner == null ? "" : owner.getName().getString();
  }

  default boolean isOwner(ServerPlayer serverPlayer) {
    return serverPlayer != null && isOwner(serverPlayer.getUUID());
  }

  default boolean isOwner(UUID uuid) {
    return uuid != null && this.hasOwner() && uuid.equals(this.getOwnerUUID());
  }

  default void defineSynchedOwnerData() {
    defineSynchedEntityData(SynchedDataIndex.OWNER_UUID, Optional.empty());
  }

  default void addAdditionalOwnerData(CompoundTag compoundTag) {
    if (this.getOwnerUUID() != null) {
      compoundTag.putUUID(DATA_OWNER_TAG, this.getOwnerUUID());
    }
  }

  default void readAdditionalOwnerData(CompoundTag compoundTag) {
    if (compoundTag.hasUUID(DATA_OWNER_TAG)) {
      UUID uuid = compoundTag.getUUID(DATA_OWNER_TAG);
      this.setOwnerUUID(uuid);
    }
  }
}
