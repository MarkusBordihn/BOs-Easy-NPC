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
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Mob;

public interface SkinDataCapable<T extends Mob> extends EasyNPC<T> {

  String EASY_NPC_DATA_SKIN_DATA_TAG = "SkinData";

  default int getEntitySkinScaling() {
    return 30;
  }

  default String getSkinURL() {
    return this.getSkinDataEntry().url();
  }

  default UUID getSkinUUID() {
    return this.getSkinDataEntry().uuid();
  }

  default SkinType getSkinType() {
    return this.getSkinDataEntry().type();
  }

  default SkinModel getSkinModel() {
    return SkinModel.HUMANOID;
  }

  default SkinDataEntry getSkinDataEntry() {
    return this.getSynchedEntityData(SynchedDataIndex.SKIN_DATA);
  }

  default void setSkinDataEntry(SkinDataEntry skinDataEntry) {
    this.setSynchedEntityData(SynchedDataIndex.SKIN_DATA, skinDataEntry);
  }

  default void defineSynchedSkinData() {
    this.defineSynchedEntityData(SynchedDataIndex.SKIN_DATA, new SkinDataEntry());
  }

  default void addAdditionalSkinData(CompoundTag compoundTag) {
    CompoundTag skinTag = new CompoundTag();
    this.getSkinDataEntry().write(skinTag);
    compoundTag.put(EASY_NPC_DATA_SKIN_DATA_TAG, skinTag);
  }

  default void readAdditionalSkinData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_SKIN_DATA_TAG)) {
      log.debug("No skin data available for {}.", this);
      return;
    }

    SkinDataEntry skinDataEntry =
        new SkinDataEntry(compoundTag.getCompound(EASY_NPC_DATA_SKIN_DATA_TAG));
    this.setSkinDataEntry(skinDataEntry);
  }
}
