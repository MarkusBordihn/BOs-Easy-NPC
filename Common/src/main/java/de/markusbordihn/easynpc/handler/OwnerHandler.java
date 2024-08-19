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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class OwnerHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private OwnerHandler() {}

  public static boolean setOwner(EasyNPC<?> easyNPC, LivingEntity owner) {
    if (easyNPC == null || owner == null) {
      log.error("[{}] Error setting owner!", easyNPC);
      return false;
    }

    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null) {
      log.error("[{}] No owner data available for setting owner!", easyNPC);
      return false;
    }

    if (ownerData.getOwner() != null && ownerData.isOwnedBy(owner)) {
      log.debug("[{}] Owner is already set to {}!", easyNPC, owner);
      return true;
    }

    log.debug("[{}] Setting owner to {}", easyNPC, owner);
    ownerData.setOwnerUUID(owner.getUUID());

    // Update objective data if follow owner objective is active.
    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData != null && objectiveData.hasObjective(ObjectiveType.FOLLOW_OWNER)) {
      ObjectiveDataEntry objectiveDataEntry =
          objectiveData.getObjective(ObjectiveType.FOLLOW_OWNER);
      if (objectiveDataEntry.getTargetOwnerUUID() == null
          || objectiveDataEntry.getTargetOwnerUUID() != owner.getUUID()) {
        log.debug("[{}] Update follow owner objective to {}", easyNPC, owner);
        objectiveDataEntry.setTargetOwnerUUID(owner.getUUID());
        objectiveData.removeObjective(ObjectiveType.FOLLOW_OWNER);
        objectiveData.addObjective(objectiveDataEntry);
      }
    }

    return true;
  }

  public static boolean removeOwner(EasyNPC<?> easyNPC) {
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null) {
      log.error("[{}] No owner data available for setting owner!", easyNPC);
      return false;
    }

    if (ownerData.getOwnerUUID() == null) {
      log.debug("[{}] Owner is already removed!", easyNPC);
      return true;
    }

    log.debug("[{}] Removing owner ...", easyNPC);
    ownerData.setOwner(null);
    return true;
  }
}
