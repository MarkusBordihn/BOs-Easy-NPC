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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface SkinData<T extends PathfinderMob> extends EasyNPC<T> {

  String EASY_NPC_DATA_SKIN_DATA_TAG = "SkinData";
  String EASY_NPC_DATA_SKIN_NAME_TAG = "SkinName";
  String EASY_NPC_DATA_SKIN_TAG = "Skin";
  String EASY_NPC_DATA_SKIN_TYPE_TAG = "SkinType";
  String EASY_NPC_DATA_SKIN_URL_TAG = "SkinURL";
  String EASY_NPC_DATA_SKIN_UUID_TAG = "SkinUUID";
  EntityDataSerializer<SkinType> SKIN_TYPE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, SkinType skinType) {
          buffer.writeEnum(skinType);
        }

        public SkinType read(FriendlyByteBuf buffer) {
          return buffer.readEnum(SkinType.class);
        }

        public SkinType copy(SkinType skinType) {
          return skinType;
        }
      };
  EntityDataSerializer<UUID> SKIN_UUID =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, UUID skinUUID) {
          buffer.writeUUID(skinUUID);
        }

        public UUID read(FriendlyByteBuf buffer) {
          return buffer.readUUID();
        }

        public UUID copy(UUID skinUUID) {
          return skinUUID;
        }
      };

  static void registerSyncedSkinData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Skin Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.SKIN_NAME,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.STRING));
    map.put(
        SynchedDataIndex.SKIN_URL,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.STRING));
    map.put(SynchedDataIndex.SKIN_UUID, SynchedEntityData.defineId(entityClass, SKIN_UUID));
    map.put(SynchedDataIndex.SKIN_TYPE, SynchedEntityData.defineId(entityClass, SKIN_TYPE));
  }

  static void registerSkinDataSerializer() {
    EntityDataSerializers.registerSerializer(SKIN_TYPE);
    EntityDataSerializers.registerSerializer(SKIN_UUID);
  }

  default int getEntitySkinScaling() {
    return 30;
  }

  default String getSkinName() {
    return getSynchedEntityData(SynchedDataIndex.SKIN_NAME);
  }

  default void setSkinName(String skin) {
    setSynchedEntityData(SynchedDataIndex.SKIN_NAME, skin != null ? skin : "");
  }

  default String getSkinURL() {
    return getSynchedEntityData(SynchedDataIndex.SKIN_URL);
  }

  default void setSkinURL(String skinURL) {
    setSynchedEntityData(SynchedDataIndex.SKIN_URL, skinURL != null ? skinURL : "");
  }

  default UUID getSkinUUID() {
    return getSynchedEntityData(SynchedDataIndex.SKIN_UUID);
  }

  default void setSkinUUID(UUID uuid) {
    setSynchedEntityData(SynchedDataIndex.SKIN_UUID, uuid != null ? uuid : Constants.BLANK_UUID);
  }

  default SkinType getSkinType() {
    return getSynchedEntityData(SynchedDataIndex.SKIN_TYPE);
  }

  default void setSkinType(SkinType skinType) {
    setSynchedEntityData(SynchedDataIndex.SKIN_TYPE, skinType);
  }

  default SkinType getSkinType(String name) {
    return SkinType.get(name);
  }

  default SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
  }

  default void defineSynchedSkinData() {
    defineSynchedEntityData(SynchedDataIndex.SKIN_NAME, "");
    defineSynchedEntityData(SynchedDataIndex.SKIN_URL, "");
    defineSynchedEntityData(SynchedDataIndex.SKIN_UUID, Constants.BLANK_UUID);
    defineSynchedEntityData(SynchedDataIndex.SKIN_TYPE, SkinType.DEFAULT);
  }

  default void addAdditionalSkinData(CompoundTag compoundTag) {
    CompoundTag skinTag = new CompoundTag();

    if (this.getSkinName() != null) {
      skinTag.putString(EASY_NPC_DATA_SKIN_TAG, this.getSkinName());
    }
    if (this.getSkinURL() != null) {
      skinTag.putString(EASY_NPC_DATA_SKIN_URL_TAG, this.getSkinURL());
    }
    UUID skinUUID = this.getSkinUUID();
    if (skinUUID != null && !skinUUID.equals(Constants.BLANK_UUID)) {
      skinTag.putUUID(EASY_NPC_DATA_SKIN_UUID_TAG, skinUUID);
    }
    if (this.getSkinType() != null) {
      skinTag.putString(EASY_NPC_DATA_SKIN_TYPE_TAG, this.getSkinType().name());
    }

    compoundTag.put(EASY_NPC_DATA_SKIN_DATA_TAG, skinTag);
  }

  default void readAdditionalSkinData(CompoundTag compoundTag) {

    // Early exit if no skin data is available.
    if (!compoundTag.contains(EASY_NPC_DATA_SKIN_DATA_TAG)) {
      return;
    }

    // Read skin data
    CompoundTag skinTag = compoundTag.getCompound(EASY_NPC_DATA_SKIN_DATA_TAG);
    if (skinTag.contains(EASY_NPC_DATA_SKIN_TYPE_TAG)) {
      String skinType = skinTag.getString(EASY_NPC_DATA_SKIN_TYPE_TAG);
      if (!skinType.isEmpty()) {
        this.setSkinType(this.getSkinType(skinType));
      }
    }
    if (skinTag.contains(EASY_NPC_DATA_SKIN_NAME_TAG)) {
      String skinName = skinTag.getString(EASY_NPC_DATA_SKIN_NAME_TAG);
      if (!skinName.isEmpty()) {
        this.setSkinName(skinName);
      }
    }
    if (skinTag.contains(EASY_NPC_DATA_SKIN_URL_TAG)) {
      String url = skinTag.getString(EASY_NPC_DATA_SKIN_URL_TAG);
      if (!url.isEmpty()) {
        this.setSkinURL(url);
      }
    }
    if (skinTag.contains(EASY_NPC_DATA_SKIN_UUID_TAG)) {
      UUID skinUUID = skinTag.getUUID(EASY_NPC_DATA_SKIN_UUID_TAG);
      this.setSkinUUID(skinUUID);
    }
  }
}
