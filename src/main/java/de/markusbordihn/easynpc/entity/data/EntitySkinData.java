/**
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

package de.markusbordihn.easynpc.entity.data;

import java.util.Optional;
import java.util.UUID;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;

import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import de.markusbordihn.easynpc.skin.SkinModel;
import de.markusbordihn.easynpc.skin.SkinType;

public interface EntitySkinData extends EntityDataInterface {

  // Synced entity data
  public static final EntityDataAccessor<String> DATA_SKIN =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  public static final EntityDataAccessor<String> DATA_SKIN_URL =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.STRING);
  public static final EntityDataAccessor<Optional<UUID>> DATA_SKIN_UUID =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.OPTIONAL_UUID);
  public static final EntityDataAccessor<SkinType> DATA_SKIN_TYPE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, CustomDataSerializers.SKIN_TYPE);

  // CompoundTags
  public static final String DATA_SKIN_TAG = "Skin";
  public static final String DATA_SKIN_TYPE_TAG = "SkinType";
  public static final String DATA_SKIN_URL_TAG = "SkinURL";
  public static final String DATA_SKIN_UUID_TAG = "SkinUUID";

  default String getSkin() {
    return getEntityData(DATA_SKIN);
  }

  default void setSkin(String skin) {
    setEntityData(DATA_SKIN, skin != null ? skin : "");
  }

  default String getSkinURL() {
    return getEntityData(DATA_SKIN_URL);
  }

  default void setSkinURL(String skinURL) {
    setEntityData(DATA_SKIN_URL, skinURL != null ? skinURL : "");
  }

  default Optional<UUID> getSkinUUID() {
    return getEntityData(DATA_SKIN_UUID);
  }

  default void setSkinUUID(UUID uuid) {
    setEntityData(DATA_SKIN_UUID, Optional.of(uuid));
  }

  default void setSkinUUID(Optional<UUID> uuid) {
    setEntityData(DATA_SKIN_UUID, uuid);
  }

  default SkinType getSkinType() {
    return getEntityData(DATA_SKIN_TYPE);
  }

  default SkinType getSkinType(String name) {
    return SkinType.get(name);
  }

  default void setSkinType(SkinType skinType) {
    setEntityData(DATA_SKIN_TYPE, skinType);
  }

  default void setSkinType(String name) {
    SkinType skinType = getSkinType(name);
    if (skinType != null) {
      setSkinType(skinType);
    } else {
      log.error("Unknown skin type {} for {}", name, this);
    }
  }

  default SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
  }

  default void defineSynchedSkinData() {
    defineEntityData(DATA_SKIN, "");
    defineEntityData(DATA_SKIN_URL, "");
    defineEntityData(DATA_SKIN_UUID, Optional.empty());
    defineEntityData(DATA_SKIN_TYPE, SkinType.DEFAULT);
  }

  default void addAdditionalSkinData(CompoundTag compoundTag) {
    if (this.getSkin() != null) {
      compoundTag.putString(DATA_SKIN_TAG, this.getSkin());
    }
    if (this.getSkinURL() != null) {
      compoundTag.putString(DATA_SKIN_URL_TAG, this.getSkinURL());
    }
    Optional<UUID> skinUUID = this.getSkinUUID();
    if (skinUUID.isPresent()) {
      compoundTag.putUUID(DATA_SKIN_UUID_TAG, skinUUID.get());
    }
    if (this.getSkinType() != null) {
      compoundTag.putString(DATA_SKIN_TYPE_TAG, this.getSkinType().name());
    }
  }

  default void readAdditionalSkinData(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_SKIN_TAG)) {
      String skin = compoundTag.getString(DATA_SKIN_TAG);
      if (skin != null && !skin.isEmpty()) {
        this.setSkin(skin);
      }
    }
    if (compoundTag.contains(DATA_SKIN_URL_TAG)) {
      String url = compoundTag.getString(DATA_SKIN_URL_TAG);
      if (url != null && !url.isEmpty()) {
        this.setSkinURL(url);
      }
    }
    if (compoundTag.contains(DATA_SKIN_UUID_TAG)) {
      UUID skinUUID = compoundTag.getUUID(DATA_SKIN_UUID_TAG);
      if (skinUUID != null) {
        this.setSkinUUID(skinUUID);
      }
    }
    if (compoundTag.contains(DATA_SKIN_TYPE_TAG)) {
      String skinType = compoundTag.getString(DATA_SKIN_TYPE_TAG);
      if (skinType != null && !skinType.isEmpty()) {
        this.setSkinType(this.getSkinType(skinType));
      }
    }
  }
}
