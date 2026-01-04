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

import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface SkinDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  String EASY_NPC_DATA_SKIN_DATA_TAG = "SkinData";

  default int getEntitySkinScaling() {
    return 30;
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

  default SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
  }

  default SkinDataEntry getSkinDataEntry() {
    return getSynchedEntityData(SynchedDataIndex.SKIN_DATA);
  }

  default void setSkinDataEntry(SkinDataEntry skinDataEntry) {
    setSynchedEntityData(SynchedDataIndex.SKIN_DATA, skinDataEntry);
  }

  default void defineSynchedSkinData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.SKIN_DATA, new SkinDataEntry());
  }

  default void addAdditionalSkinData(ValueOutput valueOutput) {
    CompoundTag skinTag = new CompoundTag();
    getSkinDataEntry().write(skinTag);
    valueOutput.store(EASY_NPC_DATA_SKIN_DATA_TAG, CompoundTag.CODEC, skinTag);
  }

  default void readAdditionalSkinData(ValueInput valueInput) {

    // Early exit if no skin data is available.
    Optional<CompoundTag> compoundTagData =
        valueInput.read(EASY_NPC_DATA_SKIN_DATA_TAG, CompoundTag.CODEC);
    if (compoundTagData.isEmpty()) {
      log.warn("No skin data available for {}.", this);
      return;
    }

    // Load skin data from new format
    SkinDataEntry skinDataEntry = new SkinDataEntry(compoundTagData.get());
    this.setSkinDataEntry(skinDataEntry);
  }
}
