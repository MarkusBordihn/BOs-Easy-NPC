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
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.UUIDUtil;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityReference;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.OwnableEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface OwnerDataCapable<T extends PathfinderMob> extends EasyNPC<T>, OwnableEntity {

  String DATA_OWNER_TAG = "Owner";

  default void setNPCOwnerUUID(UUID uuid) {
    if (uuid == null) {
      setSynchedEntityData(SynchedDataIndex.OWNER_UUID, Optional.empty());
    } else {
      EntityReference<LivingEntity> entityReference = EntityReference.of(uuid);
      setSynchedEntityData(SynchedDataIndex.OWNER_UUID, Optional.of(entityReference));
    }
  }

  default UUID getOwnerUUID() {
    EntityReference<LivingEntity> ownerReference = getOwnerReference();
    return ownerReference == null ? null : ownerReference.getUUID();
  }

  default EntityReference<LivingEntity> getOwnerReference() {
    Optional<EntityReference<LivingEntity>> ownerReference =
        getSynchedEntityData(SynchedDataIndex.OWNER_UUID);
    return ownerReference.orElse(null);
  }

  default boolean hasNPCOwner() {
    return this.getOwnerUUID() != null;
  }

  default boolean isNPCOwnedBy(LivingEntity livingEntity) {
    return livingEntity != null
        && this.hasNPCOwner()
        && livingEntity.getUUID().equals(this.getOwnerUUID());
  }

  default void setNPCOwner(LivingEntity owner) {
    if (owner != null) {
      this.setNPCOwnerUUID(owner.getUUID());
    } else {
      this.setNPCOwnerUUID(null);
    }
  }

  default String getNPCOwnerName() {
    LivingEntity owner = this.getOwner();
    return owner == null ? "" : owner.getName().getString();
  }

  default boolean isNPCOwner(ServerPlayer serverPlayer) {
    return serverPlayer != null && isNPCOwner(serverPlayer.getUUID());
  }

  default boolean isNPCOwner(UUID uuid) {
    return uuid != null && this.hasNPCOwner() && uuid.equals(this.getOwnerUUID());
  }

  @Override
  default LivingEntity getOwner() {
    Level level = getEntityLevel();
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

  default void defineSynchedOwnerData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.OWNER_UUID, Optional.empty());
  }

  default void addAdditionalOwnerData(ValueOutput valueOutput) {
    UUID ownerUUID = this.getOwnerUUID();
    if (ownerUUID != null) {
      valueOutput.store(DATA_OWNER_TAG, UUIDUtil.CODEC, ownerUUID);
    }
  }

  default void readAdditionalOwnerData(ValueInput valueInput) {
    Optional<UUID> ownerUUID = valueInput.read(DATA_OWNER_TAG, UUIDUtil.CODEC);
    ownerUUID.ifPresent(this::setNPCOwnerUUID);
  }
}
