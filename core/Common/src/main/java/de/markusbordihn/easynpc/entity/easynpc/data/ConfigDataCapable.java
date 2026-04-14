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
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface ConfigDataCapable<T extends Mob> extends EasyNPC<T> {

  String DATA_EASY_NPC_DATA_VERSION_TAG = "EasyNPCVersion";

  default void addAdditionalConfigData(ValueOutput valueOutput) {
    valueOutput.putInt(DATA_EASY_NPC_DATA_VERSION_TAG, Constants.NPC_DATA_VERSION);
  }

  default void readAdditionalConfigData(ValueInput valueInput) {
    // Read Easy NPC Data Version to check for compatibility issues.
    int npcDataVersion = valueInput.getInt(DATA_EASY_NPC_DATA_VERSION_TAG).orElse(-1);
    if (npcDataVersion > 0) {
      if (npcDataVersion > Constants.NPC_DATA_VERSION) {
        log.warn(
            "Incompatible Easy NPC Data with version {} > {} for {}! Will try to load data, but expect issues.",
            npcDataVersion,
            Constants.NPC_DATA_VERSION,
            this);
      } else if (npcDataVersion < Constants.NPC_DATA_VERSION) {
        log.warn(
            "Outdated Easy NPC Data with version {} for {}. Will try to convert data automatically.",
            npcDataVersion,
            this);
      }
      this.setNPCDataVersion(npcDataVersion);
    } else {
      log.warn(
          "Legacy Easy NPC Data for {}. Data may not be compatible with the current version.",
          this);
      this.setNPCDataVersion(-1);
    }
  }
}
