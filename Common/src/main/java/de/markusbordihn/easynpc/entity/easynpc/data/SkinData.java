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
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
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
  String EASY_NPC_DATA_SKIN_TYPE_TAG = "SkinType";
  String EASY_NPC_DATA_SKIN_URL_TAG = "SkinURL";
  String EASY_NPC_DATA_SKIN_UUID_TAG = "SkinUUID";

  EntityDataSerializer<SkinDataEntry> SKIN_DATA_ENTRY =
      new EntityDataSerializer<>() {
        @Override
        public void write(FriendlyByteBuf buffer, SkinDataEntry value) {
          buffer.writeNbt(value.createTag());
        }

        @Override
        public SkinDataEntry read(FriendlyByteBuf buffer) {
          return new SkinDataEntry(buffer.readNbt());
        }

        @Override
        public SkinDataEntry copy(SkinDataEntry value) {
          return value;
        }
      };

  static void registerSyncedSkinData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Skin Data for {}.", entityClass.getSimpleName());
    map.put(SynchedDataIndex.SKIN_DATA, SynchedEntityData.defineId(entityClass, SKIN_DATA_ENTRY));
  }

  static void registerSkinDataSerializer() {
    EntityDataSerializers.registerSerializer(SKIN_DATA_ENTRY);
  }

  default int getEntitySkinScaling() {
    return 30;
  }

  default String getSkinName() {
    return getSkinDataEntry().name();
  }

  default String getSkinURL() {
    return getSkinDataEntry().url();
  }

  default UUID getSkinUUID() {
    return getSkinDataEntry().uuid();
  }

  default SkinType getSkinType() {
    return getSkinDataEntry().type();
  }

  default SkinType getSkinType(String name) {
    return SkinType.get(name);
  }

  default SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
  }

  default SkinDataEntry getSkinDataEntry() {
    return getSynchedEntityData(SynchedDataIndex.SKIN_DATA);
  }

  default void setSkinDataEntry(SkinDataEntry skinDataEntry) {
    setSynchedEntityData(SynchedDataIndex.SKIN_DATA, skinDataEntry);
  }

  default void defineSynchedSkinData() {
    defineSynchedEntityData(SynchedDataIndex.SKIN_DATA, new SkinDataEntry());
  }

  default void addAdditionalSkinData(CompoundTag compoundTag) {
    CompoundTag skinTag = new CompoundTag();
    getSkinDataEntry().write(skinTag);
    compoundTag.put(EASY_NPC_DATA_SKIN_DATA_TAG, skinTag);
  }

  default void readAdditionalSkinData(CompoundTag compoundTag) {

    // Early exit if no skin data is available.
    if (!compoundTag.contains(EASY_NPC_DATA_SKIN_DATA_TAG)) {
      log.warn("No skin data available for {}.", this);
      return;
    }

    // Convert latency skin data to new format
    CompoundTag skinTag = compoundTag.getCompound(EASY_NPC_DATA_SKIN_DATA_TAG);
    if (skinTag.contains(EASY_NPC_DATA_SKIN_TYPE_TAG)) {
      log.info("Converting old skin data {} to new format ...", skinTag);
      SkinDataEntry skinDataEntry =
          new SkinDataEntry(
              skinTag.getString(EASY_NPC_DATA_SKIN_NAME_TAG),
              skinTag.getString(EASY_NPC_DATA_SKIN_URL_TAG),
              skinTag.contains(EASY_NPC_DATA_SKIN_UUID_TAG)
                  ? skinTag.getUUID(EASY_NPC_DATA_SKIN_UUID_TAG)
                  : Constants.BLANK_UUID,
              SkinType.get(skinTag.getString(EASY_NPC_DATA_SKIN_TYPE_TAG)));
      this.setSkinDataEntry(skinDataEntry);
      return;
    }

    // Load skin data from new format
    SkinDataEntry skinDataEntry = new SkinDataEntry(skinTag);
    this.setSkinDataEntry(skinDataEntry);
  }
}
