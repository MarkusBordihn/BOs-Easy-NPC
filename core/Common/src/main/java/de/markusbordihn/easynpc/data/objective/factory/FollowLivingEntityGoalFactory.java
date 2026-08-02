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
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.FollowLivingEntityGoal;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.goal.Goal;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record FollowLivingEntityGoalFactory(TargetResolver targetResolver)
    implements ObjectiveGoalFactory {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  @Override
  public Goal createGoal(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    LivingEntity targetEntity = this.targetResolver.resolve(objectiveDataEntry, easyNPC);
    if (targetEntity == null || targetEntity.isRemoved()) {
      log.debug(
          "Unable to find a valid target for {} of {}!",
          objectiveDataEntry.getType(),
          easyNPC.getEntity());
      return null;
    }

    return new FollowLivingEntityGoal(
        easyNPC,
        targetEntity,
        objectiveDataEntry.getSpeedModifier(),
        objectiveDataEntry.getStopDistance(),
        objectiveDataEntry.getStartDistance(),
        objectiveDataEntry.getTeleportDistance(),
        objectiveDataEntry.getFollowOffset());
  }

  @FunctionalInterface
  public interface TargetResolver {
    LivingEntity resolve(ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC);
  }
}
