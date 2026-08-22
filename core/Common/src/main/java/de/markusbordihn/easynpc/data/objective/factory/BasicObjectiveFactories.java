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
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomPanicGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomRandomSwimmingGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveBackToHomeGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RandomStrollAroundGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RandomStrollAroundHomeGoal;
import net.minecraft.world.entity.ai.goal.GolemRandomStrollInVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveBackToVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveThroughVillageGoal;
import net.minecraft.world.entity.ai.goal.RestrictSunGoal;
import net.minecraft.world.entity.ai.goal.WaterAvoidingRandomStrollGoal;

final class BasicObjectiveFactories {

  private BasicObjectiveFactories() {}

  static void registerFactories() {
    register(
        ObjectiveType.RANDOM_STROLL,
        (objectiveDataEntry, easyNPC) ->
            new RandomStrollAroundGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier()));

    register(
        ObjectiveType.WATER_AVOIDING_RANDOM_STROLL,
        (objectiveDataEntry, easyNPC) ->
            easyNPC.getPathfinderMob() != null
                ? new WaterAvoidingRandomStrollGoal(
                    easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier())
                : new RandomStrollAroundGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier()));

    register(
        ObjectiveType.MOVE_THROUGH_VILLAGE,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new MoveThroughVillageGoal(
                    easyNPC.getPathfinderMob(),
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.getOnlyAtNight(),
                    objectiveDataEntry.getDistanceToPoi(),
                    objectiveDataEntry.getCanDealWithDoors())));

    register(
        ObjectiveType.MOVE_BACK_TO_HOME,
        (objectiveDataEntry, easyNPC) ->
            new MoveBackToHomeGoal<>(
                easyNPC,
                objectiveDataEntry.getSpeedModifier(),
                objectiveDataEntry.getStopDistance()));

    register(
        ObjectiveType.MOVE_BACK_TO_VILLAGE,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new MoveBackToVillageGoal(
                    easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier(), false)));

    register(
        ObjectiveType.RANDOM_STROLL_AROUND_HOME,
        (objectiveDataEntry, easyNPC) ->
            new RandomStrollAroundHomeGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier()));

    register(
        ObjectiveType.RANDOM_STROLL_IN_VILLAGE,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new GolemRandomStrollInVillageGoal(
                    easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier())));

    register(
        ObjectiveType.RANDOM_SWIMMING,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new CustomRandomSwimmingGoal(
                    easyNPC.getPathfinderMob(),
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.getInterval())));

    register(
        ObjectiveType.PANIC,
        (objectiveDataEntry, easyNPC) ->
            new CustomPanicGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier()));

    register(
        ObjectiveType.AVOID_SUN,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) -> new RestrictSunGoal(easyNPC.getPathfinderMob())));
  }
}
