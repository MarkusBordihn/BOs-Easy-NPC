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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.PathfinderMob;

public interface NPCData<T extends PathfinderMob> extends EasyNPC<T> {

  String DATA_EASY_NPC_DATA_VERSION_TAG = "EasyNPCVersion";

  default void addAdditionalNPCData(CompoundTag compoundTag) {
    compoundTag.putInt(DATA_EASY_NPC_DATA_VERSION_TAG, Constants.NPC_DATA_VERSION);
  }

  default void readAdditionalNPCData(CompoundTag compoundTag) {

    // Read Easy NPC Data Version to check for compatibility issues.
    if (compoundTag.contains(DATA_EASY_NPC_DATA_VERSION_TAG)) {
      int npcDataVersion = compoundTag.getInt(DATA_EASY_NPC_DATA_VERSION_TAG);
      if (npcDataVersion > Constants.NPC_DATA_VERSION) {
        log.warn(
            "Incompatible Easy NPC Data with version {} > {} for {}!",
            npcDataVersion,
            Constants.NPC_DATA_VERSION,
            this);
        log.warn("Will try to load data, but expect issues!");
      } else if (npcDataVersion < Constants.NPC_DATA_VERSION) {
        log.warn("Outdated Easy NPC Data with version {} for {}!", npcDataVersion, this);
        log.warn("Will try to convert data automatically to new format.");
      }
      this.setNPCDataVersion(npcDataVersion);
    } else {
      log.warn("Legacy Easy NPC Data for {}!", this);
      log.warn("It could be possible that the data is not compatible with the current version.");
      this.setNPCDataVersion(-1);
    }
  }
}
