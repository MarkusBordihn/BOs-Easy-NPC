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

package de.markusbordihn.easynpc.data.objective.factory;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveGoalFactory;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.world.entity.ai.goal.Goal;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record PathfinderMobObjectiveFactory(ObjectiveGoalFactory delegate)
    implements ObjectiveGoalFactory {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  @Override
  public Goal createGoal(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    if (!this.isCompatible(easyNPC)) {
      this.logIncompatibleEntity(objectiveDataEntry, easyNPC);
      return null;
    }

    return this.delegate.createGoal(objectiveDataEntry, easyNPC);
  }

  @Override
  public Goal createTarget(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    if (!this.isCompatible(easyNPC)) {
      this.logIncompatibleEntity(objectiveDataEntry, easyNPC);
      return null;
    }

    return this.delegate.createTarget(objectiveDataEntry, easyNPC);
  }

  @Override
  public boolean isCompatible(EasyNPC<?> easyNPC) {
    return easyNPC != null && easyNPC.getPathfinderMob() != null;
  }

  @Override
  public int getDefaultPriority() {
    return this.delegate.getDefaultPriority();
  }

  @Override
  public boolean hasTravelObjective() {
    return this.delegate.hasTravelObjective();
  }

  private void logIncompatibleEntity(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    log.debug(
        "Entity {} is not a PathfinderMob, cannot use {} objective!",
        easyNPC != null ? easyNPC.getEntity() : null,
        objectiveDataEntry.getType());
  }
}
