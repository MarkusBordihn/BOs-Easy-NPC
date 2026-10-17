/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.api.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCObjectiveHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EasyNPCObjectiveHandler() {}

  public static boolean addObjective(EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
    if (objectiveType == null) {
      log.error("[{}] Unable to add objective without type!", easyNPC);
      return false;
    }

    return addObjective(easyNPC, new ObjectiveDataEntry(objectiveType));
  }

  public static boolean addObjective(EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
    ObjectiveDataCapable<?> objectiveData = getServerObjectiveData(easyNPC);
    if (objectiveData == null || objectiveDataEntry == null) {
      log.error("[{}] Unable to add objective {}!", easyNPC, objectiveDataEntry);
      return false;
    }

    objectiveData.addOrUpdateCustomObjective(objectiveDataEntry);
    return objectiveData.hasObjective(objectiveDataEntry);
  }

  public static boolean removeObjective(EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
    ObjectiveDataCapable<?> objectiveData = getServerObjectiveData(easyNPC);
    return objectiveData != null
        && objectiveType != null
        && objectiveData.removeCustomObjective(objectiveType);
  }

  public static boolean removeObjective(EasyNPC<?> easyNPC, String objectiveId) {
    ObjectiveDataEntry objectiveDataEntry = getObjective(easyNPC, objectiveId);
    return objectiveDataEntry != null
        && easyNPC.getEasyNPCObjectiveData().removeCustomObjective(objectiveDataEntry);
  }

  public static boolean setPriority(EasyNPC<?> easyNPC, String objectiveId, int priority) {
    ObjectiveDataEntry objectiveDataEntry = getObjective(easyNPC, objectiveId);
    if (objectiveDataEntry == null) {
      return false;
    }

    objectiveDataEntry.setPriority(priority);
    return addObjective(easyNPC, objectiveDataEntry);
  }

  public static ObjectiveDataEntry getObjective(EasyNPC<?> easyNPC, String objectiveId) {
    ObjectiveDataCapable<?> objectiveData = getServerObjectiveData(easyNPC);
    if (objectiveData == null
        || objectiveData.getObjectiveDataSet() == null
        || objectiveId == null
        || objectiveId.isEmpty()) {
      return null;
    }

    return objectiveData.getObjectiveDataSet().getObjective(objectiveId);
  }

  public static boolean hasObjective(EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
    ObjectiveDataCapable<?> objectiveData = getServerObjectiveData(easyNPC);
    return objectiveData != null
        && objectiveType != null
        && objectiveData.hasObjective(objectiveType);
  }

  public static Set<ObjectiveDataEntry> getObjectives(EasyNPC<?> easyNPC) {
    ObjectiveDataCapable<?> objectiveData = getServerObjectiveData(easyNPC);
    if (objectiveData == null) {
      return Set.of();
    }

    ObjectiveDataSet objectiveDataSet = objectiveData.getObjectiveDataSet();
    return objectiveDataSet != null ? objectiveDataSet.getObjectives() : Set.of();
  }

  private static ObjectiveDataCapable<?> getServerObjectiveData(EasyNPC<?> easyNPC) {
    if (easyNPC == null || easyNPC.isClientSideInstance()) {
      return null;
    }

    return easyNPC.getEasyNPCObjectiveData();
  }
}
