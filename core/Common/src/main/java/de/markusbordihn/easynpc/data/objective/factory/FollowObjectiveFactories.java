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

import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.register;
import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.requiresPathfinderMob;

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomTemptGoal;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.crafting.Ingredient;

final class FollowObjectiveFactories {

  private FollowObjectiveFactories() {}

  static void registerFactories() {
    register(
        ObjectiveType.FOLLOW_OWNER,
        new FollowLivingEntityGoalFactory(
            (objectiveDataEntry, easyNPC) -> {
              Entity targetOwner = objectiveDataEntry.getTargetOwner(easyNPC);
              return targetOwner instanceof LivingEntity livingEntity ? livingEntity : null;
            }));

    register(
        ObjectiveType.FOLLOW_PLAYER,
        new FollowLivingEntityGoalFactory(
            (objectiveDataEntry, easyNPC) -> objectiveDataEntry.getTargetPlayer()));

    register(
        ObjectiveType.FOLLOW_ENTITY_BY_UUID,
        new FollowLivingEntityGoalFactory(
            (objectiveDataEntry, easyNPC) -> objectiveDataEntry.getTargetEntity(easyNPC)));

    register(
        ObjectiveType.FOLLOW_ITEM,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) -> {
              Ingredient temptItems = TargetItemResolver.resolveTargetItems(objectiveDataEntry);
              if (temptItems == null) {
                return null;
              }

              return new CustomTemptGoal(
                  easyNPC,
                  easyNPC.getPathfinderMob(),
                  objectiveDataEntry.getSpeedModifier(),
                  temptItems,
                  objectiveDataEntry.getCanScare(),
                  objectiveDataEntry.getOnlyWithoutOwner());
            }));
  }
}
