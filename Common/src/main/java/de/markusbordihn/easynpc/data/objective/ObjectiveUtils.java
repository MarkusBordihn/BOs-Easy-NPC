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
import net.minecraft.world.entity.ai.goal.RandomStrollGoal;
import net.minecraft.world.entity.ai.goal.RandomSwimmingGoal;
import net.minecraft.world.entity.ai.goal.RestrictSunGoal;
import net.minecraft.world.entity.ai.goal.WaterAvoidingRandomStrollGoal;
import net.minecraft.world.entity.ai.goal.target.NearestAttackableTargetGoal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.FlyingAnimal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Enemy;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectiveUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ObjectiveUtils() {
  }

  public static Goal createObjectiveGoal(ObjectiveData objectiveData, EasyNPC<?> easyNPC) {
    Entity targetOwner = objectiveData.getTargetOwner(easyNPC);
    Entity easyNPCEntity = easyNPC.getEasyNPCEntity();
    PathfinderMob pathfinderMob = easyNPC.getPathfinderMob();

    switch (objectiveData.getType()) {
      case FOLLOW_PLAYER:
        ServerPlayer targetServerPlayer = objectiveData.getTargetPlayer();
        if (targetServerPlayer != null && !targetServerPlayer.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              targetServerPlayer,
              objectiveData.getSpeedModifier(),
              objectiveData.getStopDistance(),
              objectiveData.getStartDistance(),
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          log.error(
              "Unable to find player {} for {}!", objectiveData.getTargetPlayer(), objectiveData);
        }
        break;
      case FOLLOW_OWNER:
        if (targetOwner instanceof LivingEntity livingEntity && !livingEntity.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              livingEntity,
              objectiveData.getSpeedModifier(),
              objectiveData.getStopDistance(),
              objectiveData.getStartDistance(),
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          log.error(
              "Unable to find valid owner {} for {} with {}!",
              targetOwner,
              easyNPCEntity,
              objectiveData);
        }
        break;
      case FOLLOW_ENTITY_BY_UUID:
        LivingEntity targetEntityMob = objectiveData.getTargetEntity(easyNPC);
        if (targetEntityMob != null && !targetEntityMob.isRemoved()) {
          return new FollowLivingEntityGoal(
              easyNPC,
              targetEntityMob,
              objectiveData.getSpeedModifier(),
              objectiveData.getStopDistance(),
              objectiveData.getStartDistance(),
              easyNPCEntity instanceof FlyingAnimal);
        } else {
          log.error(
              "Unable to find living entity {} for {}!",
              objectiveData.getTargetEntityUUID(),
              objectiveData);
        }
        break;
      case RANDOM_STROLL:
        return new RandomStrollGoal(pathfinderMob, objectiveData.getSpeedModifier());
      case WATER_AVOIDING_RANDOM_STROLL:
        return new WaterAvoidingRandomStrollGoal(pathfinderMob, objectiveData.getSpeedModifier());
      case MOVE_THROUGH_VILLAGE:
        return new MoveThroughVillageGoal(
            pathfinderMob,
            objectiveData.getSpeedModifier(),
            objectiveData.getOnlyAtNight(),
            objectiveData.getDistanceToPoi(),
            objectiveData.getCanDealWithDoors());
      case MOVE_BACK_TO_HOME:
        return new MoveBackToHomeGoal<>(
            easyNPC, objectiveData.getSpeedModifier(), objectiveData.getStopDistance());
      case MOVE_BACK_TO_VILLAGE:
        return new MoveBackToVillageGoal(pathfinderMob, objectiveData.getSpeedModifier(), false);
      case RANDOM_STROLL_IN_VILLAGE:
        return new GolemRandomStrollInVillageGoal(pathfinderMob, objectiveData.getSpeedModifier());
      case CROSSBOW_ATTACK:
        return new CrossbowAttackGoal<>(
            easyNPC, objectiveData.getSpeedModifier(), objectiveData.getAttackRadius());
      case BOW_ATTACK:
        return new RangedBowAttackGoal<>(
            easyNPC,
            objectiveData.getSpeedModifier(),
            objectiveData.getAttackInterval(),
            objectiveData.getAttackRadius());
      case MELEE_ATTACK:
        return new CustomMeleeAttackGoal<>(
            easyNPC, objectiveData.getSpeedModifier(), objectiveData.isMustSeeTarget());
      case ZOMBIE_ATTACK:
        return new ZombieAttackGoal<>(
            easyNPC, objectiveData.getSpeedModifier(), objectiveData.isMustSeeTarget());
      case RANDOM_SWIMMING:
        return new RandomSwimmingGoal(
            pathfinderMob, objectiveData.getSpeedModifier(), objectiveData.getInterval());
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
            objectiveData.getLookDistance(),
            objectiveData.getProbability());
      case LOOK_AT_MOB:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Mob.class,
            objectiveData.getLookDistance(),
            objectiveData.getProbability());
      case LOOK_AT_ANIMAL:
        return new CustomLookAtPlayerGoal<>(
            easyNPC,
            Animal.class,
            objectiveData.getLookDistance(),
            objectiveData.getProbability());
      case LOOK_RANDOM_AROUND:
        return new RandomLookAroundGoal(pathfinderMob);
      case PANIC:
        return new PanicGoal(pathfinderMob, objectiveData.getSpeedModifier());
      case AVOID_SUN:
        return new RestrictSunGoal(pathfinderMob);
      case FLEE_SUN:
        return new FleeSunGoal(pathfinderMob, objectiveData.getSpeedModifier());
      default:
        return null;
    }
    return null;
  }

  public static Goal createObjectiveTarget(
      ObjectiveData objectiveData, EasyNPC<?> easyNPC) {
    PathfinderMob pathfinderMob = easyNPC.getPathfinderMob();
    return switch (objectiveData.getType()) {
      case ATTACK_ANIMAL -> new NearestAttackableTargetGoal<>(
          pathfinderMob, Animal.class, objectiveData.isMustSeeTarget());
      case ATTACK_PLAYER -> new NearestAttackableTargetGoal<>(
          pathfinderMob, Player.class, objectiveData.isMustSeeTarget());
      case ATTACK_MONSTER -> new NearestAttackableTargetGoal<>(
          pathfinderMob, Monster.class, objectiveData.isMustSeeTarget());
      case ATTACK_MOB_WITHOUT_CREEPER -> new NearestAttackableTargetGoal<>(
          pathfinderMob,
          Mob.class,
          objectiveData.getInterval(),
          false,
          false,
          entity -> entity instanceof Enemy && !(entity instanceof Creeper));
      case ATTACK_MOB -> new NearestAttackableTargetGoal<>(
          pathfinderMob,
          Mob.class,
          objectiveData.getInterval(),
          false,
          false,
          Enemy.class::isInstance);
      case ATTACK_VILLAGER -> new NearestAttackableTargetGoal<>(
          pathfinderMob, AbstractVillager.class, objectiveData.isMustSeeTarget());
      default -> null;
    };
  }
}
