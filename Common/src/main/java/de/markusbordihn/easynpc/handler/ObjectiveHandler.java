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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ObjectiveHandler() {}

  public static boolean addOrUpdateCustomObjective(
      EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {

    if (easyNPC == null || objectiveDataEntry == null) {
      log.error("[{}] Error adding or updating custom objective!", easyNPC);
      return false;
    }

    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      log.error("[{}] No objective data available!", easyNPC);
      return false;
    }

    // Add or update custom objective.
    if (!objectiveData.addOrUpdateCustomObjective(objectiveDataEntry)) {
      log.error("[{}] Error adding or updating custom objective!", easyNPC);
      return false;
    }

    // Show details, if debug is enabled.
    if (log.isDebugEnabled()) {
      logObjectiveGoals(objectiveData.getEntityGoalSelector().getAvailableGoals());
      logObjectiveTargets(objectiveData.getEntityTargetSelector().getAvailableGoals());
    }

    return true;
  }

  public static boolean removeCustomObjective(
      EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {

    if (easyNPC == null || objectiveDataEntry == null) {
      log.error("[{}] Error removing custom objective!", easyNPC);
      return false;
    }

    ObjectiveData<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      log.error("[{}] No objective data available!", easyNPC);
      return false;
    }

    // Remove custom objective.
    if (!objectiveData.removeCustomObjective(objectiveDataEntry)) {
      log.error("[{}] Error removing custom objective!", easyNPC);
      return false;
    }

    // Show details, if debug is enabled.
    if (log.isDebugEnabled()) {
      logObjectiveGoals(objectiveData.getEntityGoalSelector().getAvailableGoals());
      logObjectiveTargets(objectiveData.getEntityTargetSelector().getAvailableGoals());
    }

    return true;
  }

  public static void logObjectiveGoals(Set<WrappedGoal> goals) {
    List<Goal> unwrappedGoals = getUnwrappedGoals(goals);
    if (goals != null && !goals.isEmpty() && !unwrappedGoals.isEmpty()) {
      log.debug("Goals: {}", unwrappedGoals);
    }
  }

  public static void logObjectiveTargets(Set<WrappedGoal> goals) {
    List<Goal> unwrappedGoals = getUnwrappedGoals(goals);
    if (goals != null && !goals.isEmpty() && !unwrappedGoals.isEmpty()) {
      log.debug("Targets: {}", unwrappedGoals);
    }
  }

  public static List<Goal> getUnwrappedGoals(Set<WrappedGoal> goals) {
    if (goals == null || goals.isEmpty()) {
      return new ArrayList<>();
    }
    List<Goal> unwrappedGoals = new ArrayList<>();
    for (WrappedGoal wrappedGoal : goals) {
      unwrappedGoals.add(wrappedGoal.getGoal());
    }
    return unwrappedGoals;
  }
}
