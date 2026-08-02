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
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.FleeSunGoal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.villager.AbstractVillager;
import net.minecraft.world.entity.player.Player;

final class FleeObjectiveFactories {

  private FleeObjectiveFactories() {}

  static void registerFactories() {
    register(
        ObjectiveType.FLEE_SUN,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new FleeSunGoal(
                    easyNPC.getPathfinderMob(), objectiveDataEntry.getSpeedModifier())));

    registerAvoidEntity(ObjectiveType.FLEE_CREEPER, Creeper.class);
    registerAvoidEntity(ObjectiveType.FLEE_MOB, Mob.class);
    registerAvoidEntity(ObjectiveType.FLEE_MONSTER, Monster.class);
    registerAvoidEntity(ObjectiveType.FLEE_PLAYER, Player.class);
    registerAvoidEntity(ObjectiveType.FLEE_VILLAGER, AbstractVillager.class);
  }

  private static void registerAvoidEntity(
      ObjectiveType objectiveType, Class<? extends LivingEntity> avoidedEntityClass) {
    register(objectiveType, requiresPathfinderMob(new AvoidEntityGoalFactory(avoidedEntityClass)));
  }
}
