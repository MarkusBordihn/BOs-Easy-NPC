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
import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.registerTarget;
import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.requiresPathfinderMob;
import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.targetFactory;

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.BowAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CrossbowAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomFactionHurtByTargetGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomMeleeAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomOwnerHurtByTargetGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.GunAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ZombieAttackGoal;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.target.HurtByTargetGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.villager.AbstractVillager;
import net.minecraft.world.entity.player.Player;

final class AttackObjectiveFactories {

  private AttackObjectiveFactories() {}

  static void registerFactories() {
    registerAttackGoals();
    registerAttackTargets();
  }

  private static void registerAttackGoals() {
    register(
        ObjectiveType.CROSSBOW_ATTACK,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new CrossbowAttackGoal<>(
                    easyNPC,
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.getAttackRadius())));

    register(
        ObjectiveType.BOW_ATTACK,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new BowAttackGoal<>(
                    easyNPC,
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.getAttackInterval(),
                    objectiveDataEntry.getAttackRadius())));

    register(
        ObjectiveType.MELEE_ATTACK,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new CustomMeleeAttackGoal<>(
                    easyNPC,
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.isMustSeeTarget())));

    register(
        ObjectiveType.ZOMBIE_ATTACK,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new ZombieAttackGoal<>(
                    easyNPC,
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.isMustSeeTarget())));

    register(
        ObjectiveType.GUN_ATTACK,
        requiresPathfinderMob(
            (objectiveDataEntry, easyNPC) ->
                new GunAttackGoal<>(
                    easyNPC,
                    objectiveDataEntry.getSpeedModifier(),
                    objectiveDataEntry.getAttackInterval(),
                    objectiveDataEntry.getAttackRadius())));
  }

  private static void registerAttackTargets() {
    registerTarget(ObjectiveType.ATTACK_ANIMAL, new NearestAttackableTargetFactory(Animal.class));
    registerTarget(ObjectiveType.ATTACK_PLAYER, new NearestAttackableTargetFactory(Player.class));
    registerTarget(ObjectiveType.ATTACK_MONSTER, new NearestAttackableTargetFactory(Monster.class));
    registerTarget(
        ObjectiveType.ATTACK_VILLAGER, new NearestAttackableTargetFactory(AbstractVillager.class));

    registerTarget(
        ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER,
        new NearestAttackableTargetFactory(
            Player.class,
            (objectiveDataEntry, easyNPC) ->
                targetEntity ->
                    easyNPC.getEasyNPCOwnerData() != null
                        && targetEntity != easyNPC.getEasyNPCOwnerData().getOwner()));

    registerTarget(
        ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER,
        new NearestAttackableTargetFactory(
            Mob.class,
            (objectiveDataEntry, easyNPC) ->
                targetEntity ->
                    targetEntity instanceof Enemy && !(targetEntity instanceof Creeper)));

    registerTarget(
        ObjectiveType.ATTACK_MOB,
        new NearestAttackableTargetFactory(
            Mob.class, (objectiveDataEntry, easyNPC) -> Enemy.class::isInstance));

    registerTarget(
        ObjectiveType.OWNER_HURT_BY_TARGET,
        (objectiveDataEntry, easyNPC) -> new CustomOwnerHurtByTargetGoal<>(easyNPC));

    registerTarget(
        ObjectiveType.FACTION_HURT_BY_TARGET,
        (objectiveDataEntry, easyNPC) ->
            new CustomFactionHurtByTargetGoal<>(easyNPC, objectiveDataEntry.getInterval()));

    registerTarget(
        ObjectiveType.HURT_BY_TARGET,
        requiresPathfinderMob(
            targetFactory(
                (objectiveDataEntry, easyNPC) ->
                    new HurtByTargetGoal(easyNPC.getPathfinderMob()))));
  }
}
