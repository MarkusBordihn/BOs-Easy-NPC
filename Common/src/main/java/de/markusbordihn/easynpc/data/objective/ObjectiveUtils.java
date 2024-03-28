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

package de.markusbordihn.easynpc.data.objective;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CrossbowAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomMeleeAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.FollowLivingEntityGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveBackToHomeGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RandomStrollAroundGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RandomStrollAroundHomeGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.RangedBowAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ResetLookAtPlayerGoal;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.ZombieAttackGoal;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.goal.FleeSunGoal;
import net.minecraft.world.entity.ai.goal.FloatGoal;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.GolemRandomStrollInVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveBackToVillageGoal;
import net.minecraft.world.entity.ai.goal.MoveThroughVillageGoal;
import net.minecraft.world.entity.ai.goal.OpenDoorGoal;
import net.minecraft.world.entity.ai.goal.PanicGoal;
import net.minecraft.world.entity.ai.goal.RandomLookAroundGoal;
import net.minecraft.world.entity.ai.goal.RandomSwimmingGoal;
import net.minecraft.world.entity.ai.goal.RestrictSunGoal;
import net.minecraft.world.entity.ai.goal.WaterAvoidingRandomStrollGoal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ObjectiveUtils() {}

  public static Goal createObjectiveGoal(
      ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    Entity targetOwner = objectiveDataEntry.getTargetOwner(easyNPC);
    Entity easyNPCEntity = easyNPC.getEasyNPCEntity();
    PathfinderMob pathfinderMob = easyNPC.getPathfinderMob();

    switch (objectiveDataEntry.getType()) {
      case FOLLOW_PLAYER:
        ServerPlayer targetServerPlayer = objectiveDataEntry.getTargetPlayer();
        if (targetServerPlayer != null && !targetServerPlayer.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              targetServerPlayer,
              objectiveDataEntry.getSpeedModifier(),
              objectiveDataEntry.getStopDistance(),
              objectiveDataEntry.getStartDistance());
        } else {
          log.error(
              "Unable to find player {} for {}!",
              objectiveDataEntry.getTargetPlayer(),
              objectiveDataEntry);
        }
        break;
      case FOLLOW_OWNER:
        if (targetOwner instanceof LivingEntity livingEntity && !livingEntity.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              livingEntity,
              objectiveDataEntry.getSpeedModifier(),
              objectiveDataEntry.getStopDistance(),
              objectiveDataEntry.getStartDistance());
        } else {
          log.error(
              "Unable to find valid owner {} for {} with {}!",
              targetOwner,
              easyNPCEntity,
              objectiveDataEntry);
        }
        break;
      case FOLLOW_ENTITY_BY_UUID:
        LivingEntity targetEntityMob = objectiveDataEntry.getTargetEntity(easyNPC);
        if (targetEntityMob != null && !targetEntityMob.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              targetEntityMob,
              objectiveDataEntry.getSpeedModifier(),
              objectiveDataEntry.getStopDistance(),
              objectiveDataEntry.getStartDistance());
        } else {
          log.error(
              "Unable to find living entity {} for {}!",
              objectiveDataEntry.getTargetEntityUUID(),
              objectiveDataEntry);
        }
        break;
      case RANDOM_STROLL:
        return new RandomStrollAroundGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier());
      case WATER_AVOIDING_RANDOM_STROLL:
        return new WaterAvoidingRandomStrollGoal(
            pathfinderMob, objectiveDataEntry.getSpeedModifier());
      case MOVE_THROUGH_VILLAGE:
        return new MoveThroughVillageGoal(
            pathfinderMob,
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getOnlyAtNight(),
            objectiveDataEntry.getDistanceToPoi(),
            objectiveDataEntry.getCanDealWithDoors());
      case MOVE_BACK_TO_HOME:
        return new MoveBackToHomeGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.getStopDistance());
      case MOVE_BACK_TO_VILLAGE:
        return new MoveBackToVillageGoal(
            pathfinderMob, objectiveDataEntry.getSpeedModifier(), false);
      case RANDOM_STROLL_AROUND_HOME:
        return new RandomStrollAroundHomeGoal<>(easyNPC, objectiveDataEntry.getSpeedModifier());
      case RANDOM_STROLL_IN_VILLAGE:
        return new GolemRandomStrollInVillageGoal(
            pathfinderMob, objectiveDataEntry.getSpeedModifier());
      case CROSSBOW_ATTACK:
        return new CrossbowAttackGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.getAttackRadius());
      case BOW_ATTACK:
        return new RangedBowAttackGoal<>(
            easyNPC,
            objectiveDataEntry.getSpeedModifier(),
            objectiveDataEntry.getAttackInterval(),
            objectiveDataEntry.getAttackRadius());
      case MELEE_ATTACK:
        return new CustomMeleeAttackGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.isMustSeeTarget());
      case ZOMBIE_ATTACK:
        return new ZombieAttackGoal<>(
            easyNPC, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.isMustSeeTarget());
      case RANDOM_SWIMMING:
        return new RandomSwimmingGoal(
            pathfinderMob, objectiveDataEntry.getSpeedModifier(), objectiveDataEntry.getInterval());
      case FLOAT:
        return new FloatGoal(pathfinderMob);
      case OPEN_DOOR:
        return new OpenDoorGoal(pathfinderMob, false);
      case CLOSE_DOOR:
        return new OpenDoorGoal(pathfinderMob, true);
      case LOOK_AT_RESET:
        return new ResetLookAtPlayerGoal<>(easyNPC);
      case LOOK_AT_PLAYER:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Player.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getProbability());
      case LOOK_AT_MOB:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Mob.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getProbability());
      case LOOK_AT_ANIMAL:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Animal.class,
            objectiveDataEntry.getLookDistance(),
            objectiveDataEntry.getProbability());
      case LOOK_RANDOM_AROUND:
        return new RandomLookAroundGoal(pathfinderMob);
      case PANIC:
        return new PanicGoal(pathfinderMob, objectiveDataEntry.getSpeedModifier());
      case AVOID_SUN:
        return new RestrictSunGoal(pathfinderMob);
      case FLEE_SUN:
        return new FleeSunGoal(pathfinderMob, objectiveDataEntry.getSpeedModifier());
      default:
        return null;
    }
    return null;
  }

  public static Goal createObjectiveTarget(
      ObjectiveDataEntry objectiveDataEntry, EasyNPC<?> easyNPC) {
    PathfinderMob pathfinderMob = easyNPC.getPathfinderMob();
    return switch (objectiveDataEntry.getType()) {
      case ATTACK_ANIMAL ->
          new NearestAttackableTargetGoal<>(
              pathfinderMob, Animal.class, objectiveDataEntry.isMustSeeTarget());
      case ATTACK_PLAYER ->
          new NearestAttackableTargetGoal<>(
              pathfinderMob, Player.class, objectiveDataEntry.isMustSeeTarget());
      case ATTACK_MONSTER ->
          new NearestAttackableTargetGoal<>(
              pathfinderMob, Monster.class, objectiveDataEntry.isMustSeeTarget());
      case ATTACK_MOB_WITHOUT_CREEPER ->
          new NearestAttackableTargetGoal<>(
              pathfinderMob,
              Mob.class,
              objectiveDataEntry.getInterval(),
              false,
              false,
              entity -> entity instanceof Enemy && !(entity instanceof Creeper));
      case ATTACK_MOB ->
          new NearestAttackableTargetGoal<>(
              pathfinderMob,
              Mob.class,
              objectiveDataEntry.getInterval(),
              false,
              false,
              Enemy.class::isInstance);
      case ATTACK_VILLAGER ->
          new NearestAttackableTargetGoal<>(
              pathfinderMob, AbstractVillager.class, objectiveDataEntry.isMustSeeTarget());
      default -> null;
    };
  }
}
